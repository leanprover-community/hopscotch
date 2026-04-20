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
--   "fail-update"                      — update fails for revs prefixed "badupdate"
--   "fail-build-and-mutate-toolchain"  — update rewrites lean-toolchain for revs prefixed "mutatetoolchain",
--                                        then build fails for revs prefixed "badbuild"
--   "fail-build-toolchain"             — build fails when lean-toolchain starts with "badbuild"
--   "fail-cache"                       — `cache get` fails for revs prefixed "badcache"

/-- Execute the mock `lake` helper executable used by the IO-heavy tests. -/
def runMockLake (args : List String) : IO UInt32 := do
  let projectDir ← IO.currentDir
  let (stage, dependencyName?) ←
    match args with
    | [stage] => pure (stage, none)
    | ["update", dependencyName] => pure ("update", some dependencyName)
    | ["cache", "get"] => pure ("cache", none)
    | _ => fail s!"mock lake expected `build`, `update [dependency]`, or `cache get`, got: {args}"
  let mode := (← IO.FS.readFile (mockLakeModePath projectDir)).trimAscii.copy
  let rev ← readPinnedRev projectDir
  let toolchain := (← IO.FS.readFile (projectDir / "lean-toolchain")).trimAscii.copy
  IO.FS.withFile (mockLakeCallsPath projectDir) .append fun handle => do
    handle.putStrLn s!"{stage}:{rev}"
  IO.FS.withFile (mockLakeToolchainsPath projectDir) .append fun handle => do
    handle.putStrLn s!"{stage}:{rev}:{toolchain}"
  if let some dep := dependencyName? then
    IO.FS.withFile (mockLakeDepsPath projectDir) .append fun handle => do
      handle.putStrLn dep
  if mode == "fail-build-and-mutate-toolchain" && stage == "update" && rev.startsWith "mutatetoolchain" then
    -- Simulates a lake update that rewrites lean-toolchain; tests that bisect restores the baseline before each probe.
    IO.FS.writeFile (projectDir / "lean-toolchain") "leanprover/lean4:v9.9.9\n"
    pure 0
  else if mode == "fail-build" && stage == "build" && rev.startsWith "badbuild" then
    pure 1
  else if mode == "fail-build-and-mutate-toolchain" && stage == "build" && rev.startsWith "badbuild" then
    pure 1
  else if mode == "fail-update" && stage == "update" && rev.startsWith "badupdate" then
    pure 1
  else if mode == "fail-build-toolchain" && stage == "build" && toolchain.startsWith "badbuild" then
    pure 1
  else if mode == "fail-cache" && stage == "cache" && rev.startsWith "badcache" then
    pure 1
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
