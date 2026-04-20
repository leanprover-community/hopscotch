import Hopscotch.Runner.Types
import Hopscotch.LakefileProcessor
import Hopscotch.GitHub
import Hopscotch.Util

namespace Hopscotch.Runner

open Hopscotch
open Hopscotch.State

/-- Result of invoking one external `lake` command. -/
private structure CommandResult where
  exitCode : UInt32
  deriving Repr

/--
Resolve the external command used for one downstream `lake` invocation.

When the default `lake` command is requested, run it through `elan` using the
downstream project's pinned toolchain. Without this, `lake` would run under
Hopscotch's own Lean version rather than the downstream's, producing incorrect
or spuriously failing builds.

A custom `lakeCommand` (e.g. a mock in tests) is passed through unchanged.
-/
def buildCommand (lakeCommand : String) (projectDir : System.FilePath)
    (args : Array String) : IO (String × Array String) := do
  if lakeCommand == "lake" then
    let toolchain := (← IO.FS.readFile (projectDir / "lean-toolchain")).trimAscii.copy
    return ("elan", #["run", toolchain, "lake"] ++ args)
  return (lakeCommand, args)

/-- Append one streamed line to the log file and optionally mirror it to stdout. -/
private def emitLine (line : String) (logPath : System.FilePath) (quiet : Bool) : IO Unit := do
  IO.FS.withFile logPath .append fun handle => do
    handle.putStr line
    handle.flush
  if !quiet then
    IO.print line

/-- Drain a piped process handle line by line into the log file.
    Declared `partial` because termination over an external I/O handle cannot be proven
    structurally; the OS guarantees it by returning an empty line at EOF. -/
private partial def streamHandle (handle : IO.FS.Handle) (logPath : System.FilePath)
    (quiet : Bool) : IO Unit := do
  let line ← handle.getLine
  if line.isEmpty then
    return ()
  emitLine line logPath quiet
  streamHandle handle logPath quiet

/--
Run `lake` in the downstream project and stream its stdout/stderr into `logPath`.

The log file is updated incrementally while the command runs. Console mirroring is
enabled unless `quiet = true`.
-/
private def runCommand (lakeCommand : String) (projectDir logPath : System.FilePath)
    (args : Array String) (quiet : Bool) : IO CommandResult := do
  ensureParentDir logPath
  IO.FS.writeFile logPath ""
  let (cmd, resolvedArgs) ← buildCommand lakeCommand projectDir args
  let child ← IO.Process.spawn {
    cmd := cmd
    args := resolvedArgs
    cwd := projectDir
    stdin := .null
    stdout := .piped
    stderr := .piped
  }
  -- Read stdout and stderr concurrently; sequential reading could deadlock
  -- if one pipe fills while the process waits to write to the other.
  let stdoutTask ← IO.asTask (streamHandle child.stdout logPath quiet) Task.Priority.dedicated
  let stderrTask ← IO.asTask (streamHandle child.stderr logPath quiet) Task.Priority.dedicated
  let exitCode ← child.wait
  IO.ofExcept stdoutTask.get
  IO.ofExcept stderrTask.get
  return { exitCode := exitCode }

/--
A verify step that runs `lake cache get` to pull prebuilt artifacts from the
configured remote before the build step. A non-zero exit is logged as a warning
and the step still reports success, so a missing/broken cache never marks the
commit as the culprit — the build still runs from source.
-/
private def cacheGet (lakeCommand : String) : ProbeStep := {
  stage := .cacheGet
  label := "lake cache get"
  run := fun projectDir logPath quiet => do
    let result ← runCommand lakeCommand projectDir logPath #["cache", "get"] quiet
    if result.exitCode != 0 then
      emitLine
        s!"warning: `lake cache get` exited with code {result.exitCode}; continuing to build\n"
        logPath quiet
    return true
}

/-- Prepend the cache step to the verify array when `cache` is true. -/
private def withCacheStep (cache : Bool) (lakeCommand : String)
    (verify : Array ProbeStep) : Array ProbeStep :=
  if cache then #[cacheGet lakeCommand] ++ verify else verify

/--
Build the default lakefile-based run strategy.

The bump step rewrites the dependency rev in the project's lakefile (auto-detecting
`lakefile.lean` vs `lakefile.toml`) and runs `lake update <dependencyName>` to fetch
the new version. The verify array holds a single `lake build` step, optionally
preceded by a `lake cache get` step when `cache` is true.
-/
def lakefileStrategy (dependencyName lakeCommand : String)
    (cache : Bool := false) : RunStrategy := {
  name := dependencyName
  mkBump := fun version => {
    stage := .bump
    label := s!"lake update {dependencyName}"
    run := fun projectDir logPath quiet => do
      LakefileProcessor.rewriteAny projectDir dependencyName version
      let result ← runCommand lakeCommand projectDir logPath
        #["update", dependencyName] quiet
      return result.exitCode == 0
  }
  verify := withCacheStep cache lakeCommand #[{
    stage := .build
    label := "lake build"
    run := fun projectDir logPath quiet => do
      let result ← runCommand lakeCommand projectDir logPath #["build"] quiet
      return result.exitCode == 0
  }]
  defaultFromRef := fun projectDir => do
    -- Prefer the manifest's resolved SHA over the lakefile's `rev` field: the
    -- lakefile may pin a branch name like `master` while the manifest records the
    -- concrete commit that `lake build` actually used.
    if let some rev ← LakefileProcessor.readManifestRev projectDir dependencyName then
      return some rev
    LakefileProcessor.readPinnedRevAny projectDir dependencyName
  defaultGitUrl := fun projectDir => do
    if let some url ← LakefileProcessor.readGitUrlAny projectDir dependencyName then
      return some url
    match ← LakefileProcessor.readScopeAny projectDir dependencyName with
    | none => return none
    | some scope =>
        -- Use just the package name (after the `/`) for the Reservoir lookup,
        -- not the full scoped dependency name.
        let pkgName := match dependencyName.splitOn "/" with
          | [_, name] => name
          | _ => dependencyName
        GitHub.fetchReservoirGitUrl scope pkgName
}

/--
Build the toolchain run strategy.

The bump step writes the given toolchain string to `lean-toolchain`. The verify
array holds a single `lake build` step, optionally preceded by a `lake cache get`
step when `cache` is true. `lake build` uses the just-written toolchain via
`buildCommand`'s `elan run` resolution.
-/
def toolchainStrategy (lakeCommand : String) (cache : Bool := false) : RunStrategy := {
  name := "toolchain"
  mkBump := fun version => {
    stage := .bump
    label := s!"write lean-toolchain {version}"
    run := fun projectDir logPath _quiet => do
      IO.FS.writeFile (projectDir / "lean-toolchain") (version ++ "\n")
      IO.FS.writeFile logPath s!"wrote lean-toolchain: {version}\n"
      return true
  }
  verify := withCacheStep cache lakeCommand #[{
    stage := .build
    label := "lake build"
    run := fun projectDir logPath quiet => do
      let result ← runCommand lakeCommand projectDir logPath #["build"] quiet
      return result.exitCode == 0
  }]
}

end Hopscotch.Runner
