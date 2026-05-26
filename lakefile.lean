import Lake

open Lake DSL

package "hopscotch" where
  version := v!"1.5.0"

lean_lib Hopscotch

lean_lib HopscotchTestLib

lean_lib Lakedit

@[default_target]
lean_exe "hopscotch" where
  root := `Main

lean_exe "lakedit" where
  root := `Lakedit.Main

lean_exe HopscotchTest where
  root := `HopscotchTest.Main

lean_exe HopscotchMockLake where
  root := `HopscotchMockLake.Main

/-- Build the mock lake helper and test executable, then run the tests with the helper path wired in. -/
@[test_driver] script test (args) do
  let ws ← getWorkspace
  let some mockLakeExe := ws.findLeanExe? `HopscotchMockLake
    | error "missing test helper executable `HopscotchMockLake`"
  let mockLakeExeFile ← ws.runBuild mockLakeExe.fetch
  let some testExe := ws.findLeanExe? `HopscotchTest
    | error "missing test executable `HopscotchTest`"
  let testExeFile ← ws.runBuild testExe.fetch
  -- Set a sentinel `GITHUB_TOKEN` in the test-process env so the env-isolation
  -- regression test in `IOTests` has something concrete to assert is scrubbed
  -- from each mock `lake` child invocation. Hopscotch's own test suite does not
  -- read `GITHUB_TOKEN` (it never hits the live GitHub API), so overriding it
  -- here is safe.
  let child ← IO.Process.spawn {
    cmd := testExeFile.toString
    args := args.toArray
    env := (← getAugmentedEnv)
      |>.push ("HOPSCOTCH_MOCK_LAKE_EXE", some mockLakeExeFile.toString)
      |>.push ("GITHUB_TOKEN", some "hopscotch-test-sentinel-do-not-leak")
  }
  child.wait
