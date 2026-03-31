import Hopscotch

open Hopscotch
open Hopscotch.State
open Lean

namespace HopscotchTestLib

/-- Captured information about one failed test case. -/
structure TestFailure where
  name : String
  message : String

/-- Common representation of one test suite as named IO actions. -/
abbrev TestSuite := Array (String × IO Unit)

/-- Fail the current test with a readable `IO.userError`. -/
def fail (message : String) : IO α :=
  throw <| IO.userError message

/-- Assert that a boolean condition is true. -/
def assertTrue (condition : Bool) (message : String) : IO Unit := do
  unless condition do
    fail message

/-- Assert equality and print both sides on failure. -/
def assertEq [BEq α] [Repr α] (expected actual : α) (message : String := "") : IO Unit := do
  unless expected == actual do
    fail s!"{message}\nexpected: {repr expected}\nactual:   {repr actual}"

/-- Assert that a string contains an expected substring. -/
def assertContains (needle haystack : String) (message : String) : IO Unit := do
  assertTrue (haystack.contains needle) message

/-- Allocate a temporary directory for a test case and clean it up afterwards. -/
def withTempDir (tempPrefix : String) (action : System.FilePath → IO Unit) : IO Unit := do
  IO.FS.withTempDir fun dir => do
    let scopedDir := dir / tempPrefix
    IO.FS.createDirAll scopedDir
    action scopedDir

/-- Load persisted runner state from a fixture project. -/
def loadState (path : System.FilePath) : IO PersistedState :=
  readJsonFile (α := PersistedState) path

/-- Build the smallest downstream fixture accepted by the MVP. -/
def makeDownstreamProject (projectDir : System.FilePath) : IO Unit := do
  IO.FS.createDirAll projectDir
  IO.FS.writeFile (projectDir / "lean-toolchain") "leanprover/lean4:v4.28.0\n"
  IO.FS.writeFile (projectDir / "lakefile.toml") <|
    String.intercalate "\n" [
      "name = \"demo\"",
      "",
      "[[require]]",
      "name = \"batteries\"",
      "git = \"https://github.com/leanprover-community/batteries.git\"",
      "rev = \"main\"",
      ""
    ]

/-- Run one external command in `cwd` and return its captured output. -/
def runProcess (cwd : System.FilePath) (cmd : String) (args : Array String) : IO IO.Process.Output :=
  IO.Process.output {
    cmd := cmd
    args := args
    cwd := cwd
  }

/-- Require that an external command succeeded and include its output on failure. -/
def requireProcessSuccess (label : String) (output : IO.Process.Output) : IO Unit := do
  unless output.exitCode == 0 do
    fail s!"{label} failed with exit code {output.exitCode}\nstdout:\n{output.stdout}\nstderr:\n{output.stderr}"

/-- Run `git` inside the fixture project and fail the test if it does not succeed. -/
def runGitWithOutput (projectDir : System.FilePath) (args : Array String) : IO IO.Process.Output := do
  let output ← runProcess projectDir "git" args
  requireProcessSuccess s!"git {String.intercalate " " args.toList}" output
  pure output

def runGit (projectDir : System.FilePath) (args : Array String) : IO Unit := do
  let _ ← runGitWithOutput projectDir args
  pure ()

/-- Initialize a temp downstream project as a git repo with one baseline commit. -/
def initializeGitRepo (projectDir : System.FilePath) (extraIgnoredPaths : List String := []) : IO Unit := do
  let ignoredPaths := ".lake/" :: ".lake/hopscotch-test/" :: extraIgnoredPaths
  IO.FS.writeFile (projectDir / ".gitignore") <|
    String.intercalate "\n" (ignoredPaths ++ [""])
  runGit projectDir #["init"]
  runGit projectDir #["config", "user.name", "Mathlib Advance Tests"]
  runGit projectDir #["config", "user.email", "hopscotch-tests@example.com"]
  runGit projectDir #["add", "."]
  runGit projectDir #["commit", "-m", "initial"]

/-- Commit a selected set of paths after a simulated downstream fix. -/
def commitPaths (projectDir : System.FilePath) (message : String) (paths : Array String) : IO Unit := do
  runGit projectDir <| #["add"] ++ paths
  runGit projectDir #["commit", "-m", message]

/-- Run one named test case and return its failure details, if any. -/
def runTest (name : String) (action : IO Unit) : IO (Option TestFailure) := do
  try
    action
    IO.println s!"✓ {name}"
    pure none
  catch error =>
    IO.println s!"✗ {name}"
    pure <| some { name := name, message := error.toString }

/-- Execute a named test suite and return the failures that occurred. -/
def runSuite (suite : TestSuite) : IO (Array TestFailure) := do
  let failures ← suite.mapM fun (name, test) =>
    runTest name test
  pure <| failures.filterMap id

/-- Extract a test label string from a Lean identifier AST node.
    `eraseMacroScopes` strips hygiene markers added during macro expansion. -/
private def testLabel (name : TSyntax `ident) : String :=
  match name.getId.eraseMacroScopes with
  | Lean.Name.anonymous => "[anonymous]"
  | Lean.Name.str _ value => value
  | Lean.Name.num _ value => toString value


/-- Expand `test_case foo` to `("foo", foo)`: the `(label, action)` element type of `TestSuite`. -/
syntax "test_case " ident : term
macro_rules
  | `(test_case $name:ident) =>
      `(($(quote (testLabel name)), $name))

end HopscotchTestLib
