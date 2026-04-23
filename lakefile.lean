import Lake

open Lake DSL

package "hopscotch" where
  version := v!"1.4.0"

lean_lib Hopscotch

lean_lib HopscotchTestLib

@[default_target]
lean_exe "hopscotch" where
  root := `Main

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
  let child ← IO.Process.spawn {
    cmd := testExeFile.toString
    args := args.toArray
    env := (← getAugmentedEnv).push ("HOPSCOTCH_MOCK_LAKE_EXE", some mockLakeExeFile.toString)
  }
  child.wait
