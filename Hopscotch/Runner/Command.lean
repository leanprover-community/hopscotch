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
    env := secretScrubEnv
  }
  -- Read stdout and stderr concurrently; sequential reading could deadlock
  -- if one pipe fills while the process waits to write to the other.
  let stdoutTask ← IO.asTask (streamHandle child.stdout logPath quiet) Task.Priority.dedicated
  let stderrTask ← IO.asTask (streamHandle child.stderr logPath quiet) Task.Priority.dedicated
  let exitCode ← child.wait
  IO.ofExcept stdoutTask.get
  IO.ofExcept stderrTask.get
  return { exitCode := exitCode }

/-- One verify step that runs `lake <subcommand>` and treats a zero exit code as success.

    If the step fails with no `<subcommand>` driver configured (`lake test` / `lake lint`
    print `no <test|lint> driver configured` and exit non-zero), abort the whole run as a
    tool error instead of recording a failed hop: that misconfiguration fails on every
    commit/toolchain, so the search would otherwise report a bogus boundary.

    Detection is a best-effort log match, not a `lake check-test` / `check-lint` query:
    those commands only exist from ~v4.12, and a non-zero exit from them is ambiguous
    (e.g. an old toolchain rejecting a newer lake-manifest.json), so they are unreliable
    across the toolchain range a run may probe. A missed match falls back to a failed hop. -/
private def lakeVerifyStep (lakeCommand subcommand : String) (stage : RunStage) : ProbeStep := {
  stage := stage
  label := s!"lake {subcommand}"
  run := fun projectDir logPath quiet => do
    let result ← runCommand lakeCommand projectDir logPath #[subcommand] quiet
    if result.exitCode != 0 then
      let log ← IO.FS.readFile logPath
      if log.contains s!"no {subcommand} driver configured" then
        throw <| IO.userError
          s!"no {subcommand} driver configured, so --{subcommand} can never pass. Add a \
             {subcommand} driver (`@[{subcommand}_driver]` or the `{subcommand}Driver` lakefile \
             field) or drop --{subcommand}. See {logPath}"
    return result.exitCode == 0
}

/--
Build the ordered verify steps shared by the lakefile and toolchain strategies.

`lake build` always runs first; `lake test` and `lake lint` are appended (in that order)
when requested. A failing step ends the probe at its own stage, marking the
commit/toolchain bad.
-/
def mkVerifySteps (lakeCommand : String) (runTest runLint : Bool) : Array ProbeStep :=
  #[lakeVerifyStep lakeCommand "build" .build]
    ++ (if runTest then #[lakeVerifyStep lakeCommand "test" .test] else #[])
    ++ (if runLint then #[lakeVerifyStep lakeCommand "lint" .lint] else #[])

/--
Build the default lakefile-based run strategy.

The bump step rewrites the dependency rev in the project's lakefile (auto-detecting
`lakefile.lean` vs `lakefile.toml`) and runs `lake update <dependencyName>` to fetch
the new version. The verify array runs `lake build`, plus `lake test` / `lake lint`
when `runTest` / `runLint` are set; a failure at any verify step fails the probe.
-/
def lakefileStrategy (dependencyName lakeCommand : String)
    (runTest : Bool := false) (runLint : Bool := false) : RunStrategy := {
  scope := dependencyName
  mkBump := fun version => {
    stage := .bump
    label := s!"lake update {dependencyName}"
    run := fun projectDir logPath quiet => do
      LakefileProcessor.rewriteAny projectDir dependencyName version
      let result ← runCommand lakeCommand projectDir logPath
        #["update", dependencyName] quiet
      return result.exitCode == 0
  }
  verify := mkVerifySteps lakeCommand runTest runLint
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
array runs `lake build` (plus `lake test` / `lake lint` when requested), each using
the just-written toolchain via `buildCommand`'s `elan run` resolution.
-/
def toolchainStrategy (lakeCommand : String)
    (runTest : Bool := false) (runLint : Bool := false) : RunStrategy := {
  scope := "toolchain"
  mkBump := fun version => {
    stage := .bump
    label := s!"write lean-toolchain {version}"
    run := fun projectDir logPath _quiet => do
      IO.FS.writeFile (projectDir / "lean-toolchain") (version ++ "\n")
      IO.FS.writeFile logPath s!"wrote lean-toolchain: {version}\n"
      return true
  }
  verify := mkVerifySteps lakeCommand runTest runLint
}

end Hopscotch.Runner
