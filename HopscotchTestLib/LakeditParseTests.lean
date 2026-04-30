import HopscotchTestLib.TestUtil
import Lakedit.CLI

open HopscotchTestLib

namespace HopscotchTestLib.LakeditParseTests

private def «parse: set --rev produces rev target» : IO Unit := do
  match ← Lakedit.CLI.parse ["set", "mathlib", "--rev", "abc123"] with
  | .set name (.rev sha) projectDir quiet =>
      assertEq "mathlib" name "dep name"
      assertEq "abc123" sha "rev sha"
      assertEq (System.FilePath.mk ".") projectDir "default project dir"
      assertEq false quiet "default quiet"
  | _ => fail "expected .set (.rev ...)"

private def «parse: set --path produces path target» : IO Unit := do
  match ← Lakedit.CLI.parse ["set", "batteries", "--path", "/local/batteries"] with
  | .set name (.path p) _ _ =>
      assertEq "batteries" name "dep name"
      assertEq (System.FilePath.mk "/local/batteries") p "path"
  | _ => fail "expected .set (.path ...)"

private def «parse: set --rev with --project-dir and --quiet» : IO Unit := do
  match ← Lakedit.CLI.parse ["set", "mathlib", "--rev", "abc123", "--project-dir", "/proj", "--quiet"] with
  | .set _ (.rev sha) projectDir quiet =>
      assertEq "abc123" sha "rev sha"
      assertEq (System.FilePath.mk "/proj") projectDir "project dir"
      assertEq true quiet "quiet flag"
  | _ => fail "expected .set (.rev ...) with options"

private def «parse: set with no target errors» : IO Unit := do
  try
    let _ ← Lakedit.CLI.parse ["set", "mathlib"]
    fail "should have thrown when no target given"
  catch _ => pure ()

private def «parse: --help produces help» : IO Unit := do
  match ← Lakedit.CLI.parse ["--help"] with
  | .help => pure ()
  | _ => fail "expected .help"

private def «parse: no args produces help» : IO Unit := do
  match ← Lakedit.CLI.parse [] with
  | .help => pure ()
  | _ => fail "expected .help for empty args"

private def «parse: --version produces version» : IO Unit := do
  match ← Lakedit.CLI.parse ["--version"] with
  | .version => pure ()
  | _ => fail "expected .version"

private def «parse: unknown subcommand errors» : IO Unit := do
  try
    let _ ← Lakedit.CLI.parse ["frobnicate"]
    fail "should have thrown for unknown subcommand"
  catch _ => pure ()

private def «parse: set with no dep name errors» : IO Unit := do
  try
    let _ ← Lakedit.CLI.parse ["set"]
    fail "should have thrown when dep name is missing"
  catch _ => pure ()

def suite : TestSuite := #[
  test_case «parse: set --rev produces rev target»,
  test_case «parse: set --path produces path target»,
  test_case «parse: set --rev with --project-dir and --quiet»,
  test_case «parse: set with no target errors»,
  test_case «parse: --help produces help»,
  test_case «parse: no args produces help»,
  test_case «parse: --version produces version»,
  test_case «parse: unknown subcommand errors»,
  test_case «parse: set with no dep name errors»
]

end HopscotchTestLib.LakeditParseTests
