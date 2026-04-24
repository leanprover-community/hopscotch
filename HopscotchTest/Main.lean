import HopscotchTestLib.BehaviorTests
import HopscotchTestLib.GitIOTests
import HopscotchTestLib.IOTests
import HopscotchTestLib.LakefileReadTests
import HopscotchTestLib.LakefileLeanReadTests
import HopscotchTestLib.ParseTests
import HopscotchTestLib.ResultsTests
import HopscotchTestLib.ToolchainIOTests
import HopscotchTestLib.UtilTests
import HopscotchTestLib.LakefileRewriting

open HopscotchTestLib

namespace HopscotchTest

/-- Print a readable failure summary after all tests have run. -/
private def printFailureSummary (failures : Array TestFailure) : IO Unit := do
  IO.eprintln ""
  IO.eprintln s!"{failures.size} test(s) failed:"
  for failure in failures do
    IO.eprintln s!"- {failure.name}"
    IO.eprintln s!"  failed assertion: {failure.message}"

/-- Return whether the `git` executable is available for the git-specific test suite. -/
private def gitAvailable : IO Bool := do
  try
    let output ← IO.Process.output {
      cmd := "git"
      args := #["--version"]
    }
    pure (output.exitCode == 0)
  catch _ =>
    pure false

/-- Select the test suites to run, skipping git-specific tests when git is unavailable. -/
def testSuitesToRun : IO (Array (String × TestSuite)) := do
  let baseSuites : Array (String × TestSuite) := #[
    ("BehaviorTests",         BehaviorTests.suite),
    ("ParseTests",            ParseTests.suite),
    ("IOTests",               IOTests.suite),
    ("LakefileReadTests",     LakefileReadTests.suite),
    ("LakefileRewriting",     LakefileRewriting.suite),
    ("LakefileLeanReadTests", LakefileLeanReadTests.suite),
    ("ResultsTests",          ResultsTests.suite),
    ("ToolchainIOTests",      ToolchainIOTests.suite),
    ("UtilTests",             UtilTests.suite)
  ]
  if ← gitAvailable then
    pure (baseSuites.push ("GitIOTests", GitIOTests.suite))
  else
    IO.eprintln "warning: git not available; skipping GitIOTests"
    pure baseSuites

def runTests : IO (Array TestFailure) := do
  let suites ← testSuitesToRun
  suites.foldlM (init := #[]) fun failures (name, suite) => do
    IO.println s!"--- {name}"
    let x ← runSuite suite
    pure (failures ++ x)

end HopscotchTest

/-- Top-level test executable entrypoint used by `lake test`. -/
def main : IO UInt32 := do
  try
    let failures ← HopscotchTest.runTests
    if failures.isEmpty then
      IO.println "hopscotch tests passed"
      pure 0
    else
      HopscotchTest.printFailureSummary failures
      pure 1
  catch error =>
    IO.eprintln s!"test runner failed: {error}"
    pure 2
