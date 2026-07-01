import HopscotchTestLib.TestUtil

namespace HopscotchTestLib

/-- Directory used by the test-only mock `lake` helper inside a fixture project. -/
def mockLakeDir (projectDir : System.FilePath) : System.FilePath :=
  projectDir / ".lake" / "hopscotch-test"

/-- File storing the mock `lake` failure mode for the current fixture project. -/
def mockLakeModePath (projectDir : System.FilePath) : System.FilePath :=
  mockLakeDir projectDir / "mode.txt"

/-- File storing the mock `lake` call log for assertions. -/
def mockLakeCallsPath (projectDir : System.FilePath) : System.FilePath :=
  mockLakeDir projectDir / "calls.log"

/-- File storing the toolchain seen by each mock `lake` invocation. -/
def mockLakeToolchainsPath (projectDir : System.FilePath) : System.FilePath :=
  mockLakeDir projectDir / "toolchains.log"

/-- File storing the dependency name passed to each `lake update` invocation. -/
def mockLakeDepsPath (projectDir : System.FilePath) : System.FilePath :=
  mockLakeDir projectDir / "deps.log"

/-- File storing the extra arguments passed to each `lake <stage>` invocation
    (one `{stage}:{space-joined args}` line per call), for asserting `--*-args` plumbing. -/
def mockLakeArgsPath (projectDir : System.FilePath) : System.FilePath :=
  mockLakeDir projectDir / "args.log"

/-- File storing the `GITHUB_TOKEN` value visible to each mock `lake` invocation.
    Used by the env-isolation regression test to confirm that hopscotch strips the
    token from the environment of every child process it spawns. -/
def mockLakeEnvPath (projectDir : System.FilePath) : System.FilePath :=
  mockLakeDir projectDir / "env.log"

/-- Environment variable used by the tests to locate the mock `lake` executable. -/
def mockLakeCommandEnvVar : String :=
  "HOPSCOTCH_MOCK_LAKE_EXE"

/-- Configure the mock `lake` helper used by the end-to-end tests. -/
def configureMockLake (projectDir : System.FilePath) (mode : String) : IO Unit := do
  let dir := mockLakeDir projectDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (mockLakeModePath projectDir) s!"{mode}\n"
  if !(← (mockLakeCallsPath projectDir).pathExists) then
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
  if !(← (mockLakeToolchainsPath projectDir).pathExists) then
    IO.FS.writeFile (mockLakeToolchainsPath projectDir) ""
  if !(← (mockLakeDepsPath projectDir).pathExists) then
    IO.FS.writeFile (mockLakeDepsPath projectDir) ""
  if !(← (mockLakeArgsPath projectDir).pathExists) then
    IO.FS.writeFile (mockLakeArgsPath projectDir) ""
  if !(← (mockLakeEnvPath projectDir).pathExists) then
    IO.FS.writeFile (mockLakeEnvPath projectDir) ""

/-- Extract the last `rev = "..."` entry from the fixture lakefile. -/
private def readPinnedRev (projectDir : System.FilePath) : IO String := do
  let contents ← IO.FS.readFile (projectDir / "lakefile.toml")
  let revs := (contents.splitOn "\n").filterMap fun line =>
    let line := line.trimAscii.copy
    if line.startsWith "rev = \"" then
      match (line.drop 7 |>.toString).splitOn "\"" with
      | rev :: _ => some rev
      | _ => none
    else
      none
  match revs.reverse.head? with
  | some rev => pure rev
  | none => fail "mock lake could not read a pinned rev from lakefile.toml"

-- Failure modes written to mockLakeModePath by configureMockLake:
--   "success"                         — all commands succeed
--   "fail-build"                       — build fails for revs prefixed "badbuild"
--   "fail-test"                        — test fails for revs prefixed "badtest" (build still succeeds)
--   "fail-lint"                        — lint fails for revs prefixed "badlint" (build/test still succeed)
--   "missing-driver"                   — test/lint emit Lake's "no <test|lint> driver configured" error and fail
--                                        (build/update still succeed); exercises the no-driver short-circuit
--   "fail-update"                      — update fails for revs prefixed "badupdate"
--   "fail-build-and-mutate-toolchain"  — update rewrites lean-toolchain for revs prefixed "mutatetoolchain",
--                                        then build fails for revs prefixed "badbuild"
--   "fail-build-toolchain"             — build fails when lean-toolchain starts with "badbuild"
--   "missing-driver"                   — `check-test`/`check-lint` exit non-zero (no driver configured);
--                                        exercises the test/lint driver preflight

/-- Execute the mock `lake` helper executable used by the IO-heavy tests. -/
def runMockLake (args : List String) : IO UInt32 := do
  let projectDir ← IO.currentDir
  let (stage, dependencyName?, extraArgs) ←
    match args with
    | "update" :: dependencyName :: _ => pure ("update", some dependencyName, #[])
    | stage :: rest => pure (stage, none, rest.toArray)
    | [] => fail s!"mock lake expected a subcommand, got: {args}"
  let mode := (← IO.FS.readFile (mockLakeModePath projectDir)).trimAscii.copy
  -- Driver preflights (`lake check-test` / `check-lint`) are not probe steps, so they are
  -- not recorded in the call logs; "missing-driver" mode reports no driver via a non-zero exit.
  if stage == "check-test" || stage == "check-lint" then
    return if mode == "missing-driver" then 1 else 0
  let rev ← readPinnedRev projectDir
  let toolchain := (← IO.FS.readFile (projectDir / "lean-toolchain")).trimAscii.copy
  IO.FS.withFile (mockLakeCallsPath projectDir) .append fun handle => do
    handle.putStrLn s!"{stage}:{rev}"
  IO.FS.withFile (mockLakeToolchainsPath projectDir) .append fun handle => do
    handle.putStrLn s!"{stage}:{rev}:{toolchain}"
  IO.FS.withFile (mockLakeArgsPath projectDir) .append fun handle => do
    handle.putStrLn s!"{stage}:{String.intercalate " " extraArgs.toList}"
  let observedToken := (← IO.getEnv "GITHUB_TOKEN").getD "(unset)"
  IO.FS.withFile (mockLakeEnvPath projectDir) .append fun handle => do
    handle.putStrLn s!"GITHUB_TOKEN={observedToken}"
  if let some dep := dependencyName? then
    IO.FS.withFile (mockLakeDepsPath projectDir) .append fun handle => do
      handle.putStrLn dep
  if mode == "fail-build-and-mutate-toolchain" && stage == "update" && rev.startsWith "mutatetoolchain" then
    -- Simulates a lake update that rewrites lean-toolchain; tests that bisect restores the baseline before each probe.
    IO.FS.writeFile (projectDir / "lean-toolchain") "leanprover/lean4:v9.9.9\n"
    pure 0
  else if mode == "fail-build" && stage == "build" && rev.startsWith "badbuild" then
    pure 1
  else if mode == "fail-test" && stage == "test" && rev.startsWith "badtest" then
    pure 1
  else if mode == "fail-lint" && stage == "lint" && rev.startsWith "badlint" then
    pure 1
  else if mode == "fail-build-and-mutate-toolchain" && stage == "build" && rev.startsWith "badbuild" then
    pure 1
  else if mode == "fail-update" && stage == "update" && rev.startsWith "badupdate" then
    pure 1
  else if mode == "fail-build-toolchain" && stage == "build" && toolchain.startsWith "badbuild" then
    pure 1
  else if mode == "lean-imports" then
    -- A faithful end-to-end simulation of building against a real dependency
    -- checkout: `update` checks the dependency repo out at the probed rev, and
    -- `build` fails iff some downstream `import Demo.*` has no corresponding file
    -- in the dependency checkout at that rev (i.e. an unknown-module error).
    let pkgsDir := projectDir / ".lake" / "packages"
    let depDir? ← do
      if ← pkgsDir.pathExists then
        let mut found : Option System.FilePath := none
        for e in ← pkgsDir.readDir do
          if found.isNone && (← e.path.isDir) then found := some e.path
        pure found
      else
        pure (none : Option System.FilePath)
    match depDir? with
    | none => pure 0
    | some depDir =>
      if stage == "update" then
        let _ ← IO.Process.output {
          cmd := "git"
          args := #["-C", depDir.toString, "checkout", "--quiet", rev]
          env := Hopscotch.secretScrubEnv
        }
        pure 0
      else
        let files ← Hopscotch.AutoFix.Mathlib.ModuleDeprecation.collectLeanFiles projectDir
        let mut ok := true
        for f in files do
          let contents ← IO.FS.readFile f
          for m in Hopscotch.AutoFix.Mathlib.ModuleDeprecation.parseShimImports contents do
            if m.startsWith "Demo." then
              let modFile := depDir / (Hopscotch.AutoFix.Mathlib.ModuleDeprecation.moduleToRelPath m)
              unless ← modFile.pathExists do ok := false
        pure (if ok then (0 : UInt32) else 1)
  else
    pure 0

/-- Resolve the mock `lake` executable path from the configured test environment. -/
def resolveMockLakeCommand (value? : Option String) : IO String :=
  match value? with
  | some path => pure path
  | none => fail s!"missing {mockLakeCommandEnvVar}; run these tests via `lake test`"

/-- Return the configured mock `lake` executable path for runner invocations. -/
def mockLakeCommand : IO String := do
  resolveMockLakeCommand (← IO.getEnv mockLakeCommandEnvVar)

end HopscotchTestLib
