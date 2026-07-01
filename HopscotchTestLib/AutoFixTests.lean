import HopscotchTestLib.TestUtil
import HopscotchTestLib.MockLake

open Hopscotch
open Hopscotch.State
open Hopscotch.AutoFix
open Hopscotch.AutoFix.ModuleDeprecation

namespace HopscotchTestLib.AutoFixTests

/-- Discard runner progress output so test logs stay focused on assertions. -/
private def ignoreOutput (_ : String) : IO Unit :=
  pure ()

/-- Current `HEAD` sha of a git repo. -/
private def headSha (repo : System.FilePath) : IO String := do
  let out ← runGitWithOutput repo #["rev-parse", "HEAD"]
  pure out.stdout.trimAscii.copy

/-- Stage everything and commit, returning the new commit sha. -/
private def commitAll (repo : System.FilePath) (msg : String) : IO String := do
  runGit repo #["add", "-A"]
  runGit repo #["commit", "-q", "-m", msg]
  headSha repo

/-- A downstream `lakefile.toml` with no git URL, so the module-deprecation fix's
    replacement resolution stays fully offline (local checkout only). -/
private def offlineLakefile : String :=
  "name = \"demo\"\n\n[[require]]\nname = \"batteries\"\n"

/-- Collect runner/command output lines for assertions. -/
private def captureOutput : IO ((String → IO Unit) × IO (Array String)) := do
  let ref ← IO.mkRef (#[] : Array String)
  pure (fun line => ref.modify (·.push line), ref.get)

/-! ## Pure helpers -/

private def «module name ↔ relative path» : IO Unit := do
  assertEq "Mathlib/Topology/Algebra/Module/LinearMap.lean"
    (moduleToRelPath "Mathlib.Topology.Algebra.Module.LinearMap")
    "dotted module name maps to a slashed .lean path"
  assertEq (some "Mathlib.Topology.Algebra.Module.LinearMap")
    (relPathToModule? "Mathlib/Topology/Algebra/Module/LinearMap.lean")
    "slashed .lean path maps back to the dotted module name"
  assertEq none (relPathToModule? "Mathlib/Foo/Bar.txt")
    "non-.lean paths are not modules"

private def «parse a real deprecated_module shim» : IO Unit := do
  -- Verbatim shape of mathlib's `Topology/Algebra/Module/LinearMap.lean` shim.
  let shim := String.intercalate "\n" [
    "module -- shake: keep-all",
    "",
    "public import Mathlib.Topology.Algebra.Module.ContinuousLinearMap.Basic",
    "public import Mathlib.Topology.Algebra.Module.ContinuousLinearMap.Idempotent",
    "",
    "deprecated_module (since := \"2026-05-21\")",
    ""
  ]
  assertEq #["Mathlib.Topology.Algebra.Module.ContinuousLinearMap.Basic",
             "Mathlib.Topology.Algebra.Module.ContinuousLinearMap.Idempotent"]
    (parseShimImports shim)
    "only the import targets are extracted; module/deprecated_module/blank lines are ignored"

private def «parseShimImports drops the deprecated_module command import» : IO Unit := do
  -- An empty "upstreamed to core" shim: its only import is the command module,
  -- which is infrastructure, not a re-export — so nothing is a migration target.
  let emptyShim := String.intercalate "\n" [
    "module -- shake: keep-all",
    "",
    "public import Mathlib.Tactic.Linter.DeprecatedModule",
    "",
    "deprecated_module \"Upstreamed to core\" (since := \"2026-02-26\")",
    ""
  ]
  assertEq (#[] : Array String) (parseShimImports emptyShim)
    "an empty shim's only (command) import is dropped, so the migration removes the import"
  -- A shim that also re-exports a real module keeps it, minus the command import.
  let realShim := String.intercalate "\n" [
    "module",
    "public import Mathlib.Tactic.Linter.DeprecatedModule",
    "public import Demo.New",
    "deprecated_module (since := \"2026-01-01\")",
    ""
  ]
  assertEq #["Demo.New"] (parseShimImports realShim)
    "the command import is excluded while genuine re-exports are kept"

private def «matchImportOf? is exact on the module token» : IO Unit := do
  assertEq (some ("", "import ", "")) (matchImportOf? "import Demo.Old" "Demo.Old")
    "plain import matches"
  assertEq (some ("  ", "public import ", "")) (matchImportOf? "  public import Demo.Old" "Demo.Old")
    "public import with indentation matches and is preserved"
  assertEq none (matchImportOf? "import Demo.OldExtra" "Demo.Old")
    "a longer module with the same prefix must not match (word boundary)"
  assertEq none (matchImportOf? "import Demo.New" "Demo.Old")
    "an unrelated import does not match"
  assertEq none (matchImportOf? "def Demo.Old := 1" "Demo.Old")
    "non-import lines do not match"

private def «rewriteImports replaces and preserves style» : IO Unit := do
  let src := String.intercalate "\n" ["import A.B", "import Demo.Old", "open Foo", ""]
  assertEq (some (String.intercalate "\n"
      ["import A.B", "import Demo.New1", "import Demo.New2", "open Foo", ""]))
    (rewriteImports src "Demo.Old" #["Demo.New1", "Demo.New2"])
    "one import expands to the replacement modules in order"
  assertEq none (rewriteImports src "Demo.Missing" #["X"])
    "no matching import yields no rewrite"
  assertEq (some (String.intercalate "\n" ["import A.B", "open Foo", ""]))
    (rewriteImports src "Demo.Old" #[])
    "an empty replacement list removes the import line"
  let pubSrc := "public import Demo.Old\n"
  assertEq (some "public import Demo.New\n")
    (rewriteImports pubSrc "Demo.Old" #["Demo.New"])
    "the public import keyword is preserved"

private def «shim recognition, comment awareness, and warning parsing» : IO Unit := do
  let shim := String.intercalate "\n" [
    "module",
    "",
    "public import Demo.New",
    "",
    "/-!",
    "# Old module",
    "import statements like this one are just prose",
    "deprecated_module mentioned in prose is ignored too",
    "-/",
    "",
    "deprecated_module (since := \"2026-01-01\")",
    ""
  ]
  assertEq #["Demo.New"] (parseShimImports shim)
    "imports inside block comments are not extracted"
  assertTrue (isDeprecatedModuleShim shim) "a real shim is recognized"
  assertTrue (!isDeprecatedModuleShim "/-! deprecated_module -/\nimport Demo.New\n")
    "a doc-comment mention alone is not a shim"
  assertTrue (!isDeprecatedModuleShim "  deprecated_module (since := \"x\")\n")
    "the command must start at column 0"
  -- A `--` line comment (or a string literal) that merely *mentions* a
  -- block-comment opener must not hide the imports that follow — the old
  -- occurrence-counting scan opened a phantom comment and dropped them.
  assertEq #["Demo.A", "Demo.B"]
    (parseShimImports (String.intercalate "\n"
      ["import Demo.A", "-- see the /- delimiter in prose", "import Demo.B", ""]))
    "a block-comment opener inside a line comment does not hide later imports"
  assertEq #["Demo.A", "Demo.B"]
    (parseShimImports (String.intercalate "\n"
      ["import Demo.A", "def s : String := \"open /- only\"", "import Demo.B", ""]))
    "block-comment delimiters inside a string literal are inert"
  -- The exact message shape Lean core emits (Lean.formatDeprecatedModuleWarning).
  let log := String.intercalate "\n" [
    "warning: ./Foo.lean:1:0: Upstreamed to core",
    "'Demo.Old' has been deprecated: please replace this import by",
    "",
    "import Demo.New",
    "import Demo.New2",
    "'Demo.Gone' has been deprecated: please replace this import by",
    "",
    "error: build failed"
  ]
  assertEq #[("Demo.Old", #["Demo.New", "Demo.New2"]), ("Demo.Gone", #[])]
    (deprecationWarnings log)
    "the toolchain's warnings yield module names and their verbatim replacements"
  assertEq #[] (deprecationWarnings "error: unknown module prefix 'Demo.Old'")
    "other quoted text is not mistaken for the warning"
  assertEq "remove import Demo.Old"
    (ImportMigration.describe { fixId := "x", oldModule := "Demo.Old", newModules := #[] })
    "empty newModules renders as an import removal"
  -- Shims that still define declarations (compat aliases) mark migrations partial.
  assertTrue (!shimDefinesDeclarations shim) "a pure shim defines nothing"
  let aliasShim := shim ++ String.intercalate "\n" [
    "public section",
    "",
    "@[deprecated \"use newThm\" (since := \"2026-01-01\")]",
    "theorem oldThm : True := trivial",
    ""
  ]
  assertTrue (shimDefinesDeclarations aliasShim)
    "a shim carrying @[deprecated] aliases is detected"
  assertTrue ((ImportMigration.describe
      { fixId := "x", oldModule := "A", newModules := #["B"], partialFix := true
      }).contains "[partial")
    "partial migrations carry the marker in their rendering"

/-! ## Backup store (used by `hopscotch fix apply`/`revert`) -/

private def «backups restore each file's own original (collision regression)» : IO Unit := do
  withTempDir "hopscotch-autofix-backup" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    -- These two paths collided under an old flat (sanitized-name) backup scheme;
    -- the mirrored-tree store must keep their originals apart.
    IO.FS.createDirAll (projectDir / "Demo")
    IO.FS.writeFile (projectDir / "Demo" / "Foo.lean") "import Demo.Old\n-- nested original\n"
    IO.FS.writeFile (projectDir / "Demo_Foo.lean") "import Demo.Old\n-- flat original\n"
    let paths ← mkPaths projectDir
    let m : ImportMigration :=
      { fixId := "module-deprecation", oldModule := "Demo.Old", newModules := #["Demo.New"] }
    let changed ← applyMigrationToWorkspace paths paths.projectDir m
    assertTrue (changed.contains "Demo/Foo.lean" && changed.contains "Demo_Foo.lean")
      "both colliding files are rewritten"
    let restored ← restoreAllBackups paths paths.projectDir
    assertEq changed.size restored.size "every rewritten file restores from its own backup"
    assertEq "import Demo.Old\n-- nested original\n"
      (← IO.FS.readFile (paths.projectDir / "Demo" / "Foo.lean"))
      "the nested file gets its own original back"
    assertEq "import Demo.Old\n-- flat original\n"
      (← IO.FS.readFile (paths.projectDir / "Demo_Foo.lean"))
      "the flat file gets its own original back, not the nested file's"

/-! ## Orchestration (MockLake + a stub fix)

A stub fix whose detection always proposes a canned migration. The run must stop
at the failure boundary, record the proposal (with the owning fix's id stamped by
the framework), and leave the workspace untouched — no retry, no rewrite. -/

private def stubFix : Fix := {
  id := "test-stub"
  description := "Propose a canned migration and advisory for any failure."
  detect := fun _ => pure {
    -- Deliberately mislabeled: the framework must stamp the owning id.
    migrations := #[{ fixId := "mislabeled", oldModule := "Demo.Old"
                      newModules := #["Demo.New"] }]
    advisories := #[{ fixId := "mislabeled", oldModule := "Demo.Dep"
                      newModules := #["Demo.Dep2"] }]
    notes := #["inspected the failure"] }
  apply := fun _ _ _ => pure #[]
}

private def «failure boundary records proposals without retrying or rewriting» : IO Unit := do
  withTempDir "hopscotch-autofix-orch" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "c1\nbadbuild2\n"
    configureMockLake projectDir "fail-build"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      runMode := .linear
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      autoFixes := #[stubFix]
      quiet := true
    } ignoreOutput
    assertEq 1 result.exitCode "the run stops at the failing commit"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.stopped) state.status "failure boundary recorded"
    assertEq (some "badbuild2") state.currentCommit "boundary is the failing commit"
    assertEq 1 state.proposedFixes.size "the proposal is recorded on the final state"
    assertEq "test-stub" (state.proposedFixes[0]!).fixId
      "the framework stamps the owning fix id on the proposal"
    assertEq 1 state.deprecatedImports.size "the advisory is recorded on the final state"
    assertEq "test-stub" (state.deprecatedImports[0]!).fixId
      "advisories get the owning fix id too"
    -- Exactly one build attempt per commit: proposals never trigger retries.
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:c1", "build:c1", "update:badbuild2", "build:badbuild2"] calls
      "no retry after the failure"
    assertTrue (result.summary.contains "Proposed fixes")
      "the summary surfaces the proposed fixes section"
    assertTrue (result.summary.contains "Deprecated imports")
      "the summary surfaces the advisory section"
    assertEq #["[test-stub] inspected the failure"] state.autoFixNotes
      "detection notes are persisted with the owning fix id"
    assertTrue (result.summary.contains "Automated fix notes")
      "the summary surfaces the notes section"

/-- A stub fix that reports whether detection received a build log, and proposes a
    migration — on a green run the runner must hand over the *last successful*
    build log, and fold any proposed migrations into the advisories. -/
private def logProbeFix : Fix := {
  id := "log-probe"
  description := "Report whether a build log reached detection."
  detect := fun ctx => pure {
    migrations := #[{ fixId := "", oldModule := "Demo.Promoted", newModules := #["Demo.X"] }]
    advisories := #[{ fixId := ""
                      oldModule := if ctx.buildLogPath.isSome then "log-present" else "log-absent"
                      newModules := #[] }] }
  apply := fun _ _ _ => pure #[]
}

private def «green conclusion hands the last successful build log to detection» : IO Unit := do
  withTempDir "hopscotch-green-log" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "c1\nc2\n"
    configureMockLake projectDir "success"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      runMode := .linear
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      autoFixes := #[logProbeFix]
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "all commits pass"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "the run is fully successful"
    assertEq 0 state.proposedFixes.size "a green conclusion records no proposals"
    assertTrue (state.deprecatedImports.any (·.oldModule == "log-present"))
      "detection received the last successful build log"
    assertTrue (state.deprecatedImports.any (·.oldModule == "Demo.Promoted"))
      "green-run migrations are folded into the advisories"

/-- Records the file name of the log detection received, so a test can assert
    *which* step's log reached it. -/
private def logNameFix : Fix := {
  id := "log-name"
  description := "Record the file name of the log handed to detection."
  detect := fun ctx => pure {
    advisories := #[{ fixId := ""
                      oldModule := (ctx.buildLogPath.bind (·.fileName)).getD "none"
                      newModules := #[] }] }
  apply := fun _ _ _ => pure #[]
}

/-- With `--test`/`--lint` enabled the verify steps are `[build, test, lint]`, so
    the *last* successful step's log is the lint log. Detection must still receive
    the **build** log, where `linter.deprecated.module` warnings actually fire. -/
private def «detection reads the build log, not the lint log, with --lint enabled» : IO Unit := do
  withTempDir "hopscotch-buildlog" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "c1\nc2\n"
    configureMockLake projectDir "success"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      runMode := .linear
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
                    { runTest := true, runLint := true }
      autoFixes := #[logNameFix]
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "all commits pass with test+lint enabled"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "the run is fully successful"
    let some adv := state.deprecatedImports[0]?
      | fail "detection recorded the log file it received"
    assertTrue (adv.oldModule.endsWith "build.log")
      s!"detection received the build log, not the lint/test log (got {adv.oldModule})"

/-- Bisect counterpart of the previous test. A bisect resolves from a *cached*
    failing probe (`ProbeResult`), so the build log must be carried on that record
    — not just on the live `ProbeRunResult` — for detection to scan it. Here the
    boundary commit's build succeeds but its lint step fails; detection must still
    receive the build log, where `linter.deprecated.module` warnings fire. -/
private def «bisect resolution hands detection the build log, not the lint log» : IO Unit := do
  withTempDir "hopscotch-buildlog-bisect" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    -- Two commits: the good endpoint is assumed; the bad endpoint fails at lint.
    IO.FS.writeFile commitListPath "c1\nbadlint2\n"
    configureMockLake projectDir "fail-lint"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      runMode := .bisect
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
                    { runTest := true, runLint := true }
      autoFixes := #[logNameFix]
      quiet := true
    } ignoreOutput
    assertEq 1 result.exitCode "the bisect stops at the lint-failing boundary"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.stopped) state.status "the boundary is recorded"
    let some adv := state.deprecatedImports[0]?
      | fail "detection recorded the log file it received"
    assertTrue (adv.oldModule.endsWith "build.log")
      s!"bisect detection received the build log, not the lint log (got {adv.oldModule})"

/-! ## Detection against a real dependency git history (git-gated) -/

private def «module-deprecation detection proposes, never applies» : IO Unit := do
  withTempDir "hopscotch-autofix-dep" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    -- A downstream source file importing the soon-to-be-deleted module.
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n\ndef hello := 1\n"
    -- A fake dependency checkout with delete-then-re-add-as-shim history.
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "Old.lean") "def old := 1\n"
    initializeGitRepo depDir
    let baseline := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    -- Deletion commit: Old is split into New.
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "def new := 1\n"
    commitPaths depDir "split: delete Old, add New" #["-A"]
    let deletion := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    -- Re-add commit: Old reappears as a deprecation shim re-exporting New.
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2026-01-01\")\n"
    commitPaths depDir "re-add Old as a deprecated shim" #["-A"]
    let shim := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    -- The failed probe left the dependency checked out at the deletion commit.
    runGit depDir #["checkout", "--quiet", deletion]

    let paths ← mkPaths projectDir
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[deletion, shim]
      currentCommit := deletion
      baseline := baseline
      buildLogPath := some (paths.logsDir / "probe.log")
      quiet := true
    }
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    assertEq 1 det.migrations.size "exactly one migration is proposed"
    let m := det.migrations[0]!
    assertEq "Demo.Old" m.oldModule "the deleted module is recorded"
    assertEq #["Demo.New"] m.newModules "the replacement comes from the deprecation shim"
    assertEq 0 det.advisories.size "a broken import is a proposal, not an advisory"
    let src ← IO.FS.readFile (projectDir / "Foo.lean")
    assertTrue (src.contains "import Demo.Old") "detection leaves the workspace untouched"
    assertTrue (!(← (backupRoot paths).pathExists))
      "no backups are created by detection (nothing was rewritten)"

private def «genuine removal with no shim yields no proposal» : IO Unit := do
  withTempDir "hopscotch-autofix-noshim" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    -- Drop the git URL so replacement resolution stays fully offline: with no shim
    -- and no rename, the only remaining strategy would be a network raw fetch, which
    -- this test deliberately makes unreachable (`repo?` resolves to none).
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Gone\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "Gone.lean") "def gone := 1\n"
    initializeGitRepo depDir
    let baseline := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    -- Delete the module outright; never re-added anywhere (a real API removal).
    IO.FS.removeFile (depDir / "Demo" / "Gone.lean")
    commitPaths depDir "remove Demo.Gone for good" #["-A"]
    let deletion := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    let paths ← mkPaths projectDir
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[deletion]
      currentCommit := deletion
      baseline := baseline
      buildLogPath := some (paths.logsDir / "probe.log")
      quiet := true
    }
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    assertEq 0 det.migrations.size "a genuine removal must not produce a proposal"
    assertEq 0 det.advisories.size "nor an advisory"
    assertTrue (det.notes.any fun n => n.contains "Demo.Gone" && n.contains "no")
      "a note records that no replacement was found"
    let src ← IO.FS.readFile (projectDir / "Foo.lean")
    assertTrue (src.contains "import Demo.Gone") "the downstream import is left untouched"

private def «shim-cleanup deletion resolves from the pre-deletion blob» : IO Unit := do
  withTempDir "hopscotch-autofix-cleanup" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    -- At the baseline the module is *already* a shim (deprecated long ago); the
    -- downstream built fine, with warnings. The upstream cleanup then deletes it.
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "def new := 1\n"
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2025-01-01\")\n"
    initializeGitRepo depDir
    let baseline := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    commitPaths depDir "chore: delete >6 month old deprecated modules" #["-A"]
    let cleanup := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    let paths ← mkPaths projectDir
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[cleanup]
      currentCommit := cleanup
      baseline := baseline
      buildLogPath := none
      quiet := true
    }
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    assertEq 1 det.migrations.size "the shim only exists in the past, but is still found"
    assertEq #["Demo.New"] (det.migrations[0]!).newModules
      "the replacement is read from the blob just before the deletion"
    assertEq 0 det.advisories.size "no advisory: the import is broken, not deprecated"

private def «an empty shim proposes removing the import» : IO Unit := do
  withTempDir "hopscotch-autofix-emptyshim" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\ndef x := 1\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    -- An "Upstreamed to core" style shim: deprecated_module with no imports.
    IO.FS.writeFile (depDir / "Demo" / "Keep.lean") "def keep := 1\n"
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "deprecated_module \"Upstreamed to core\" (since := \"2025-01-01\")\n"
    initializeGitRepo depDir
    let baseline := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    commitPaths depDir "delete the empty shim" #["-A"]
    let cleanup := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    let paths ← mkPaths projectDir
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[cleanup]
      currentCommit := cleanup
      baseline := baseline
      buildLogPath := none
      quiet := true
    }
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    assertEq 1 det.migrations.size "an empty shim still yields a proposal"
    assertEq #[] (det.migrations[0]!).newModules "the proposal is: remove the import"
    -- Applying the proposal deletes the import line.
    let _ ← applyMigrationToWorkspace paths paths.projectDir (det.migrations[0]!)
    let src ← IO.FS.readFile (paths.projectDir / "Foo.lean")
    assertTrue (!(src.contains "import Demo.Old")) "the dead import is removed"
    assertTrue (src.contains "def x := 1") "the rest of the file is intact"

private def «a live shim yields an advisory; the warning log promotes it» : IO Unit := do
  withTempDir "hopscotch-autofix-advisory" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\nimport Demo.Part\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "def new := 1\n"
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2026-01-01\")\n"
    -- A code-carrying shim: defines a compat alias the rewrite would drop.
    IO.FS.writeFile (depDir / "Demo" / "Part.lean") <| String.intercalate "\n" [
      "import Demo.New",
      "",
      "deprecated_module (since := \"2026-01-01\")",
      "",
      "@[deprecated \"use new\" (since := \"2026-01-01\")]",
      "def oldAlias := 1",
      ""
    ]
    initializeGitRepo depDir
    let head := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    let paths ← mkPaths projectDir
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[head]
      currentCommit := head
      baseline := head
      buildLogPath := none
      quiet := true
    }
    -- Plain build regime: the imports work, so these are advisories.
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    let advisories := det.advisories
    assertEq 0 det.migrations.size "a working import is not a proposal"
    assertEq 2 advisories.size "working imports through shims are advisories"
    let some adv := advisories.find? (·.oldModule == "Demo.Old")
      | fail "advisory for Demo.Old expected"
    assertEq #["Demo.New"] adv.newModules "the advisory names the replacement"
    assertTrue (!adv.partialFix) "a pure shim is a clean migration"
    let some part := advisories.find? (·.oldModule == "Demo.Part")
      | fail "advisory for Demo.Part expected"
    assertTrue part.partialFix "the code-carrying shim is flagged partial"
    -- Warnings-as-error regime: the toolchain's warning in the failing log means
    -- the deprecation itself broke the build — promoted to a proposal. A warning
    -- about a module *outside* this dependency (another package's shim) becomes a
    -- log-only advisory, with the replacements the toolchain printed.
    IO.FS.createDirAll paths.logsDir
    let logPath := paths.logsDir / "probe.log"
    IO.FS.writeFile logPath <| String.intercalate "\n" [
      "warning: ./Foo.lean:1:0: ",
      "'Demo.Old' has been deprecated: please replace this import by",
      "",
      "import Demo.New",
      "warning: ./Foo.lean:2:0: ",
      "'OtherPkg.Mod' has been deprecated: please replace this import by",
      "",
      "import OtherPkg.New",
      "error: build failed due to warnings"
    ]
    let det2 ← detectFixes #[moduleDeprecationFix]
      { ctx with buildLogPath := some logPath } ignoreOutput
    assertEq 1 det2.migrations.size "the warned deprecation is promoted to a proposal"
    assertEq #["Demo.New"] (det2.migrations[0]!).newModules "same replacement, now as a fix"
    assertEq 2 det2.advisories.size "the unwarned shim import stays an advisory; the foreign warning joins it"
    let some foreign := det2.advisories.find? (·.oldModule == "OtherPkg.Mod")
      | fail "log-only advisory for the foreign module expected"
    assertEq #["OtherPkg.New"] foreign.newModules
      "the foreign advisory carries the replacements from the log"

/-- Scenario 9: the boundary log warns about an import whose shim has already been
    cleaned up at the newest range commit. The shim is no longer at the newest
    tree, so it is read at the boundary commit itself and promoted to a proposal.
    Also covers scenario 10: a healthy import of a live, non-shim module
    (`Demo.New`) produces no record at all. -/
private def «a warned import whose shim was cleaned up later resolves at the boundary» : IO Unit := do
  withTempDir "hopscotch-autofix-warn-cleanup" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    -- `Demo.Old` resolves through a shim and is warned; `Demo.New` is a live, real
    -- module imported directly — it must yield nothing (scenario 10).
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\nimport Demo.New\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "def new := 1\n"
    -- At the boundary `Demo.Old` is a live shim; a later commit deletes it, so it
    -- is gone at the newest range commit.
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2026-01-01\")\n"
    initializeGitRepo depDir
    let boundary := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    commitPaths depDir "chore: delete the cleaned-up shim" #["-A"]
    let newest := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    runGit depDir #["checkout", "--quiet", boundary]
    let paths ← mkPaths projectDir
    IO.FS.createDirAll paths.logsDir
    let logPath := paths.logsDir / "probe.log"
    IO.FS.writeFile logPath <| String.intercalate "\n" [
      "warning: ./Foo.lean:1:0: ",
      "'Demo.Old' has been deprecated: please replace this import by",
      "",
      "import Demo.New",
      "error: build failed due to warnings"
    ]
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[boundary, newest]
      currentCommit := boundary
      baseline := boundary
      buildLogPath := some logPath
      quiet := true
    }
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    assertEq 1 det.migrations.size "the warned, since-cleaned-up shim is read at the boundary and proposed"
    assertEq "Demo.Old" (det.migrations[0]!).oldModule "the deprecated module is recorded"
    assertEq #["Demo.New"] (det.migrations[0]!).newModules "the replacement comes from the boundary shim"
    assertTrue (det.migrations.all (·.oldModule != "Demo.New") && det.advisories.all (·.oldModule != "Demo.New"))
      "a healthy import of a live, non-shim module produces no record (scenario 10)"

/-- The full iterative protocol against a dependency history with a deprecation
    window (delete `Demo.Old` at c1, re-add as shim at c2) followed by a genuine
    break (delete `Demo.Other` at c4):

    1. The first run stops honestly at the window commit c1 and *proposes* the
       import migration without touching the workspace.
    2. The consumer opts in: `hopscotch fix apply` rewrites the import.
    3. The resumed run retries c1 (now passing), searches on, and stops at the
       genuine break c4 — for which no fix can be proposed. -/
private def «end-to-end linear: boundary proposed, fix applied, re-run finds the real break» : IO Unit := do
  withTempDir "hopscotch-sim-linear" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\nimport Demo.Other\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "Old.lean") "-- Demo.Old\ndef oldThing : Nat := 100\n"
    IO.FS.writeFile (depDir / "Demo" / "Other.lean") "-- Demo.Other\ndef otherThing : String := \"abc\"\n"
    initializeGitRepo depDir
    let c0 ← headSha depDir                                       -- good baseline
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "-- Demo.New\ndef newThing : Nat := 200\n"
    let c1 ← commitAll depDir "window: delete Old, add New"        -- Old import breaks here
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2026-01-01\")\n"
    let c2 ← commitAll depDir "re-add Old as deprecated shim"
    IO.FS.writeFile (depDir / "README.md") "noop\n"
    let c3 ← commitAll depDir "unrelated change"
    IO.FS.removeFile (depDir / "Demo" / "Other.lean")
    let c4 ← commitAll depDir "BREAK: delete Demo.Other"           -- genuine culprit
    runGit depDir #["checkout", "--quiet", c0]
    let commitsFile := dir / "commits.txt"
    IO.FS.writeFile commitsFile (String.intercalate "\n" [c0, c1, c2, c3, c4, ""])
    configureMockLake projectDir "lean-imports"
    let config : Runner.Config := {
      itemSource := .file commitsFile
      projectDir := projectDir
      runMode := .linear
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      autoFixes := Hopscotch.AutoFix.standardAutoFixes
      quiet := true
    }

    -- Run 1: stop at the window commit, with the fix proposed.
    let result1 ← Runner.run config ignoreOutput
    assertEq 1 result1.exitCode "the first run stops at the first breaking change"
    let state1 ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (some c1) state1.currentCommit "the boundary is the window commit"
    assertEq 1 state1.proposedFixes.size "the migration is proposed for the boundary"
    let m := state1.proposedFixes[0]!
    assertEq "Demo.Old" m.oldModule "the deprecated module is recorded"
    assertEq #["Demo.New"] m.newModules "the replacement comes from the shim"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.Old")
      "the run leaves the workspace untouched"

    -- The consumer opts in: apply the proposed fix.
    let applyCode ← Hopscotch.FixCommand.run { action := .apply, projectDir := projectDir } ignoreOutput
    assertEq 0 applyCode "fix apply succeeds"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.New")
      "fix apply rewrites the import"

    -- Run 2 (resume): the boundary now passes; the genuine break is found next.
    let result2 ← Runner.run config ignoreOutput
    assertEq 1 result2.exitCode "the resumed run stops at the genuine regression"
    let state2 ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (some c4) state2.currentCommit "the culprit is the commit that deletes Demo.Other"
    assertEq 0 state2.proposedFixes.size "no fix can be proposed for a genuine removal"
    assertEq 0 state2.deprecatedImports.size
      "the migrated imports no longer resolve through shims"
    assertTrue (state2.autoFixNotes.any (·.contains "Demo.Other"))
      "the genuine removal is explained in the persisted notes"
    assertTrue (result2.summary.contains "Automated fix notes")
      "the summary explains why no fix was proposed"

/-- Bisect over a window with a genuine break at the endpoint. The midpoint probe
    lands inside the deprecation window and fails honestly, so the search converges
    on the window commit — reported with the proposed fix attached (case B1 of the
    deployment scenarios): one breaking change per run. -/
private def «end-to-end bisect: window boundary reported with proposed fix» : IO Unit := do
  withTempDir "hopscotch-sim-bisect" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\nimport Demo.Other\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "Old.lean") "-- Demo.Old\ndef oldThing : Nat := 100\n"
    IO.FS.writeFile (depDir / "Demo" / "Other.lean") "-- Demo.Other\ndef otherThing : String := \"abc\"\n"
    initializeGitRepo depDir
    let c0 ← headSha depDir                                       -- good (assumed lower bound)
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "-- Demo.New\ndef newThing : Nat := 200\n"
    let c1 ← commitAll depDir "window: delete Old, add New"        -- probed midpoint
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2026-01-01\")\n"
    IO.FS.removeFile (depDir / "Demo" / "Other.lean")
    let c2 ← commitAll depDir "re-add Old shim AND delete Demo.Other"  -- genuine break at endpoint
    runGit depDir #["checkout", "--quiet", c0]
    let commitsFile := dir / "commits.txt"
    IO.FS.writeFile commitsFile (String.intercalate "\n" [c0, c1, c2, ""])
    configureMockLake projectDir "lean-imports"

    let result ← Runner.run {
      itemSource := .file commitsFile
      projectDir := projectDir
      runMode := .bisect
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      autoFixes := Hopscotch.AutoFix.standardAutoFixes
      quiet := true
    } ignoreOutput

    assertEq 1 result.exitCode "bisect resolves to a failure boundary"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (some c1) state.currentCommit
      "the boundary is the window commit (the first breaking change for this downstream)"
    assertEq 1 state.proposedFixes.size "the import migration is proposed for the boundary"
    assertEq "Demo.Old" (state.proposedFixes[0]!).oldModule "the deprecated module is recorded"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.Old")
      "the run leaves the workspace untouched"
    assertTrue (result.summary.contains "hopscotch clean")
      "the bisect summary advises a fresh session before the re-run"

/-- A fully green run still audits deprecated imports: the linear scan passes
    every commit, and the conclusion records shim-routed imports as advisories. -/
private def «green run records deprecated-import advisories» : IO Unit := do
  withTempDir "hopscotch-green-advisory" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lakefile.toml") offlineLakefile
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") "def new := 1\n"
    IO.FS.writeFile (depDir / "Demo" / "Old.lean")
      "import Demo.New\n\ndeprecated_module (since := \"2026-01-01\")\n"
    initializeGitRepo depDir
    let c0 ← headSha depDir
    IO.FS.writeFile (depDir / "README.md") "noop\n"
    let c1 ← commitAll depDir "unrelated change"
    runGit depDir #["checkout", "--quiet", c0]
    let commitsFile := dir / "commits.txt"
    IO.FS.writeFile commitsFile (String.intercalate "\n" [c0, c1, ""])
    configureMockLake projectDir "lean-imports"

    let result ← Runner.run {
      itemSource := .file commitsFile
      projectDir := projectDir
      runMode := .linear
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      autoFixes := Hopscotch.AutoFix.standardAutoFixes
      quiet := true
    } ignoreOutput

    assertEq 0 result.exitCode "every commit passes (the shim resolves the import)"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "the run is fully successful"
    assertEq 0 state.proposedFixes.size "nothing to fix on a green run"
    assertEq 1 state.deprecatedImports.size "the shim-routed import is recorded"
    assertEq "Demo.Old" (state.deprecatedImports[0]!).oldModule "the deprecated module is named"
    assertEq #["Demo.New"] (state.deprecatedImports[0]!).newModules "with its replacement"
    assertTrue (result.summary.contains "Deprecated imports")
      "the green summary surfaces the advisory section"

/-! ## `hopscotch fix` — apply proposed fixes outside a run -/

private def «hopscotch fix list/apply/revert round-trips a proposed migration» : IO Unit := do
  withTempDir "hopscotch-fix-cmd" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n"
    let paths ← mkPaths projectDir
    -- Record a proposal the way a real run would, then write results.json from it.
    let state : PersistedState := {
      projectDir := paths.projectDir
      strategyScope := "batteries"
      items := #["c0", "c1"]
      nextIndex := 1
      currentCommit := some "c1"
      lastSuccessfulCommit := some "c0"
      status := .stopped
      stage := some .build
      lastLogPath := none
      proposedFixes := #[{
        fixId := "module-deprecation"
        oldModule := "Demo.Old"
        newModules := #["Demo.New"] }]
      deprecatedImports := #[{
        fixId := "module-deprecation"
        oldModule := "Demo.Adv"
        newModules := #["Demo.Adv2"] }]
      updatedAt := "2026-01-01T00:00:00Z"
    }
    Hopscotch.Results.writeResults paths none state

    -- list: reports the proposal and the advisory without touching the workspace.
    let (out, getLines) ← captureOutput
    let listCode ← Hopscotch.FixCommand.run { action := .list, projectDir := projectDir } out
    assertEq 0 listCode "list succeeds"
    let lines ← getLines
    assertTrue (lines.any fun l => l.contains "Demo.Old" && l.contains "Demo.New")
      "list shows the proposed migration"
    assertTrue (lines.any (·.contains "Demo.Adv"))
      "list shows the deprecated-import advisory"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.Old")
      "list does not modify the workspace"

    -- apply: rewrites the import in the (otherwise pristine) checkout.
    let applyCode ← Hopscotch.FixCommand.run { action := .apply, projectDir := projectDir } ignoreOutput
    assertEq 0 applyCode "apply succeeds"
    let migrated ← IO.FS.readFile (projectDir / "Foo.lean")
    assertTrue (migrated.contains "import Demo.New") "apply rewrites the import"
    assertTrue (!(migrated.contains "import Demo.Old")) "old import removed by apply"

    -- revert: restores the original from the backup apply created.
    let revertCode ← Hopscotch.FixCommand.run { action := .revert, projectDir := projectDir } ignoreOutput
    assertEq 0 revertCode "revert succeeds"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.Old")
      "revert restores the original import"

private def «fix apply migrates advisories by default; --no-advisories restricts to proposals» : IO Unit := do
  withTempDir "hopscotch-fix-advisories" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "Foo.lean")
      "import Demo.Old\nimport Demo.Adv\nimport Demo.Part\n"
    let paths ← mkPaths projectDir
    let state : PersistedState := {
      projectDir := paths.projectDir
      strategyScope := "batteries"
      items := #["c0", "c1"]
      nextIndex := 1
      currentCommit := some "c1"
      lastSuccessfulCommit := some "c0"
      status := .stopped
      stage := some .build
      lastLogPath := none
      proposedFixes := #[{
        fixId := "module-deprecation", oldModule := "Demo.Old", newModules := #["Demo.New"] }]
      deprecatedImports := #[
        { fixId := "module-deprecation", oldModule := "Demo.Adv", newModules := #["Demo.Adv2"] },
        { fixId := "module-deprecation", oldModule := "Demo.Part", newModules := #["Demo.Part2"]
          partialFix := true }]
      updatedAt := "2026-01-01T00:00:00Z"
    }
    Hopscotch.Results.writeResults paths none state

    -- --no-advisories: proposals only; advisories left untouched.
    let code ← Hopscotch.FixCommand.run
      { action := .apply, projectDir := projectDir, includeAdvisories := false } ignoreOutput
    assertEq 0 code "apply --no-advisories succeeds"
    let src ← IO.FS.readFile (projectDir / "Foo.lean")
    assertTrue (src.contains "import Demo.New") "the proposal is applied"
    assertTrue (src.contains "import Demo.Adv\n") "advisories are untouched under --no-advisories"
    assertTrue (src.contains "import Demo.Part\n") "advisories are untouched under --no-advisories"

    -- Default apply: clean advisories migrated too, partial ones skipped loudly.
    let (out, getLines) ← captureOutput
    let code2 ← Hopscotch.FixCommand.run { action := .apply, projectDir := projectDir } out
    assertEq 0 code2 "plain apply succeeds"
    let src2 ← IO.FS.readFile (projectDir / "Foo.lean")
    assertTrue (src2.contains "import Demo.Adv2") "the clean advisory is migrated by default"
    assertTrue (src2.contains "import Demo.Part\n")
      "the partial advisory (shim defines declarations) is left alone"
    assertTrue ((← getLines).any (·.contains "skipping advisory"))
      "the partial advisory is skipped loudly"

private def «hopscotch fix apply skips unknown fix types» : IO Unit := do
  withTempDir "hopscotch-fix-unknown" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n"
    let paths ← mkPaths projectDir
    -- A results.json carrying one supported proposal and one from a hypothetical
    -- future fix type: apply must perform the former and skip the latter loudly.
    let state : PersistedState := {
      projectDir := paths.projectDir
      strategyScope := "batteries"
      items := #["c0", "c1"]
      nextIndex := 1
      currentCommit := some "c1"
      lastSuccessfulCommit := some "c0"
      status := .stopped
      stage := some .build
      lastLogPath := none
      proposedFixes := #[
        { fixId := "module-deprecation", oldModule := "Demo.Old"
          newModules := #["Demo.New"] },
        { fixId := "future-fix", oldModule := "Demo.Other"
          newModules := #["Demo.Whatever"] }]
      updatedAt := "2026-01-01T00:00:00Z"
    }
    Hopscotch.Results.writeResults paths none state
    let (out, getLines) ← captureOutput
    let code ← Hopscotch.FixCommand.run { action := .apply, projectDir := projectDir } out
    assertEq 0 code "apply succeeds while skipping the unknown fix"
    let lines ← getLines
    assertTrue (lines.any (·.contains "skipping [future-fix]"))
      "the unknown fix type is reported, not silently dropped"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.New")
      "the supported migration is still applied"

private def «hopscotch fix rejects a results.json from an incompatible schema» : IO Unit := do
  withTempDir "hopscotch-fix-schema" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n"
    let paths ← mkPaths projectDir
    let state : PersistedState := {
      projectDir := paths.projectDir
      strategyScope := "batteries"
      items := #["c0", "c1"]
      nextIndex := 1
      currentCommit := some "c1"
      lastSuccessfulCommit := some "c0"
      status := .stopped
      stage := some .build
      lastLogPath := none
      proposedFixes := #[{
        fixId := "module-deprecation", oldModule := "Demo.Old", newModules := #["Demo.New"] }]
      updatedAt := "2026-01-01T00:00:00Z"
    }
    Hopscotch.Results.writeResults paths none state
    -- Simulate a results.json produced by an incompatible hopscotch by rewriting
    -- the declared schema version to one this build does not accept.
    let original ← IO.FS.readFile paths.resultsPath
    let stale := original.replace
      s!"\"schemaVersion\": {Hopscotch.Results.resultsSchemaVersion}" "\"schemaVersion\": 1"
    assertTrue (stale != original) "the test patched the on-disk schema version"
    IO.FS.writeFile paths.resultsPath stale
    let (out, getLines) ← captureOutput
    let code ← Hopscotch.FixCommand.run { action := .apply, projectDir := projectDir } out
    assertEq 2 code "an incompatible results schema is rejected with exit 2"
    assertTrue ((← getLines).any (·.contains "schema version"))
      "the failure names the schema-version mismatch instead of a raw JSON error"
    assertTrue ((← IO.FS.readFile (projectDir / "Foo.lean")).contains "import Demo.Old")
      "a rejected fix command leaves the workspace untouched"

/-- A `results.json` produced on one machine (e.g. CI) applies to a different,
    pristine checkout via `--from` — the basis for an automated "fix breaking
    changes" PR. The migration is portable: nothing in it depends on the checkout
    it was detected against, and the backup store lands in the target project. -/
private def «fix apply --from applies a results.json produced elsewhere» : IO Unit := do
  withTempDir "hopscotch-fix-from" fun dir => do
    -- "CI" side: record a proposal and write its results.json, against its checkout.
    let ciDir := dir / "ci"
    makeDownstreamProject ciDir
    let ciPaths ← mkPaths ciDir
    let ciState : PersistedState := {
      projectDir := ciPaths.projectDir
      strategyScope := "batteries"
      items := #["c0", "c1"]
      nextIndex := 1
      currentCommit := some "c1"
      lastSuccessfulCommit := some "c0"
      status := .stopped
      stage := some .build
      lastLogPath := none
      proposedFixes := #[{
        fixId := "module-deprecation", oldModule := "Demo.Old", newModules := #["Demo.New"] }]
      updatedAt := "2026-01-01T00:00:00Z"
    }
    Hopscotch.Results.writeResults ciPaths none ciState

    -- "Developer" side: a fresh checkout with the broken import and no run of its own.
    let devDir := dir / "dev"
    makeDownstreamProject devDir
    IO.FS.writeFile (devDir / "Foo.lean") "import Demo.Old\n"
    let devPaths ← mkPaths devDir

    let applyCode ← Hopscotch.FixCommand.run
      { action := .apply, projectDir := devDir, fromPath := some ciPaths.resultsPath } ignoreOutput
    assertEq 0 applyCode "apply --from succeeds"
    assertTrue ((← IO.FS.readFile (devDir / "Foo.lean")).contains "import Demo.New")
      "the migration from the foreign results.json rewrites the dev checkout"
    assertTrue (← (backupRoot devPaths).pathExists)
      "the backup lands in the dev project's own store, not the CI one"

    -- revert (no --from) restores from the dev project's own backup store.
    let revertCode ← Hopscotch.FixCommand.run { action := .revert, projectDir := devDir } ignoreOutput
    assertEq 0 revertCode "revert succeeds on the dev checkout"
    assertTrue ((← IO.FS.readFile (devDir / "Foo.lean")).contains "import Demo.Old")
      "revert restores the original import on the dev checkout"

/-- When upstream moves a module with `git`-detectable similarity (a rename, no
    shim ever), the deletion's rename target is the replacement: `import Old →
    import New`, a complete (non-partial) fix. -/
private def «a renamed module resolves to its rename target» : IO Unit := do
  withTempDir "hopscotch-autofix-rename" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "Foo.lean") "import Demo.Old\n\ndef hello := 1\n"
    let depDir := projectDir / ".lake" / "packages" / "batteries"
    IO.FS.createDirAll (depDir / "Demo")
    -- Substantial content so git's similarity detection sees the move as a rename.
    let body := "-- Demo module\ndef thing : Nat := 42\ndef other : Nat := 43\ndef more : Nat := 44\n"
    IO.FS.writeFile (depDir / "Demo" / "Old.lean") body
    initializeGitRepo depDir
    let baseline := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    -- Move Old.lean to New.lean with identical content and no shim.
    IO.FS.removeFile (depDir / "Demo" / "Old.lean")
    IO.FS.writeFile (depDir / "Demo" / "New.lean") body
    commitPaths depDir "move: rename Demo.Old to Demo.New" #["-A"]
    let renameCommit := (← runGitWithOutput depDir #["rev-parse", "HEAD"]).stdout.trimAscii.copy
    runGit depDir #["checkout", "--quiet", renameCommit]

    let paths ← mkPaths projectDir
    let ctx : FixContext := {
      projectDir := paths.projectDir
      paths := paths
      dependencyName := "batteries"
      items := #[renameCommit]
      currentCommit := renameCommit
      baseline := baseline
      buildLogPath := none
      quiet := true
    }
    let det ← detectFixes #[moduleDeprecationFix] ctx ignoreOutput
    assertEq 1 det.migrations.size "the rename is proposed as a migration"
    let m := det.migrations[0]!
    assertEq "Demo.Old" m.oldModule "the moved module is recorded"
    assertEq #["Demo.New"] m.newModules "the replacement is the rename target"
    assertTrue (!m.partialFix) "a mechanical rename is a complete fix"

/-- Pure + MockLake tests (no git required). -/
def suite : TestSuite := #[
  test_case «module name ↔ relative path»,
  test_case «parse a real deprecated_module shim»,
  test_case «parseShimImports drops the deprecated_module command import»,
  test_case «matchImportOf? is exact on the module token»,
  test_case «rewriteImports replaces and preserves style»,
  test_case «shim recognition, comment awareness, and warning parsing»,
  test_case «backups restore each file's own original (collision regression)»,
  test_case «failure boundary records proposals without retrying or rewriting»,
  test_case «green conclusion hands the last successful build log to detection»,
  test_case «detection reads the build log, not the lint log, with --lint enabled»,
  test_case «bisect resolution hands detection the build log, not the lint log»,
  test_case «hopscotch fix list/apply/revert round-trips a proposed migration»,
  test_case «fix apply migrates advisories by default; --no-advisories restricts to proposals»,
  test_case «hopscotch fix apply skips unknown fix types»,
  test_case «hopscotch fix rejects a results.json from an incompatible schema»,
  test_case «fix apply --from applies a results.json produced elsewhere»
]

/-- Tests that drive real `git` over a fake dependency history. -/
def gitSuite : TestSuite := #[
  test_case «module-deprecation detection proposes, never applies»,
  test_case «genuine removal with no shim yields no proposal»,
  test_case «shim-cleanup deletion resolves from the pre-deletion blob»,
  test_case «an empty shim proposes removing the import»,
  test_case «a live shim yields an advisory; the warning log promotes it»,
  test_case «green run records deprecated-import advisories»,
  test_case «end-to-end linear: boundary proposed, fix applied, re-run finds the real break»,
  test_case «end-to-end bisect: window boundary reported with proposed fix»,
  test_case «a renamed module resolves to its rename target»,
  test_case «a warned import whose shim was cleaned up later resolves at the boundary»
]

end HopscotchTestLib.AutoFixTests
