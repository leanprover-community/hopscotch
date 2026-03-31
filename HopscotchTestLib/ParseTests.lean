import HopscotchTestLib.TestUtil

open Hopscotch
open Hopscotch.State

namespace HopscotchTestLib.ParseTests

/-- Assert that parsing the given args succeeds and return the config. -/
private def parse (args : List String) : IO Runner.Config :=
  CLI.parseArgs args

/-- Assert that parsing the given args fails. -/
private def assertParseError (args : List String) (hint : String) : IO Unit := do
  try
    let _ ← CLI.parseArgs args
    fail s!"expected parse error but succeeded: {hint}"
  catch _ =>
    pure ()

-- ---------------------------------------------------------------------------
-- dep: valid parses
-- ---------------------------------------------------------------------------

/-- Scenario: `dep` with `--from` and `--to` produces a range source. -/
private def «dep range source» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc", "--to", "def"]
  match config.itemSource with
  | .range (some "def") (some "abc") none => pure ()
  | other => fail s!"expected range source, got {repr other}"
  assertEq .bisect config.runMode "should default to bisect mode"
  assertEq "mathlib" config.strategy.name "strategy scope should be dep name"
  assertEq false config.quiet "should default to quiet=false"
  assertEq false config.allowDirtyWorkspace "should default to allowDirtyWorkspace=false"

/-- Scenario: `dep` with `--commits-file` produces a file source. -/
private def «dep file source» : IO Unit := do
  let config ← parse ["dep", "batteries", "--commits-file", "/p/commits.txt"]
  match config.itemSource with
  | .file path => assertEq "/p/commits.txt" path.toString "file path should match"
  | other => fail s!"expected file source, got {repr other}"
  assertEq "batteries" config.strategy.name "strategy scope should be dep name"

/-- Scenario: `dep` with `--scan-mode bisect` sets bisect mode. -/
private def «dep scan mode bisect» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc", "--to", "def", "--scan-mode", "bisect"]
  assertEq .bisect config.runMode "should be bisect mode"

/-- Scenario: `dep` with `--scan-mode linear` sets linear mode. -/
private def «dep scan mode linear» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc", "--to", "def", "--scan-mode", "linear"]
  assertEq .linear config.runMode "should be linear mode"

/-- Scenario: `dep` with `--quiet` sets quiet flag. -/
private def «dep quiet flag» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc", "--to", "def", "--quiet"]
  assertEq true config.quiet "should be quiet"

/-- Scenario: `dep` with `--allow-dirty-workspace` sets that flag. -/
private def «dep allow dirty workspace» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc", "--to", "def", "--allow-dirty-workspace"]
  assertEq true config.allowDirtyWorkspace "should allow dirty workspace"

/-- Scenario: `dep` with `--project-dir` sets project directory. -/
private def «dep project dir» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc", "--to", "def", "--project-dir", "/my/proj"]
  assertEq "/my/proj" config.projectDir.toString "project dir should match"

/-- Scenario: `dep` with only `--from` (no `--to`) produces a range with no upper bound. -/
private def «dep from only» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--from", "abc"]
  match config.itemSource with
  | .range none (some "abc") none => pure ()
  | other => fail s!"expected range with no toRef, got {repr other}"

/-- Scenario: `dep` with only `--git-url` (no `--from` or `--to`) produces a range. -/
private def «dep git url only» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--git-url", "https://github.com/foo/bar"]
  match config.itemSource with
  | .range none none (some "https://github.com/foo/bar") => pure ()
  | other => fail s!"expected range with gitUrl, got {repr other}"

/-- Scenario: `dep` passes `--git-url` and `--to` together into the range. -/
private def «dep to and git url» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--to", "abc", "--git-url", "https://github.com/foo/bar"]
  match config.itemSource with
  | .range (some "abc") none (some _) => pure ()
  | other => fail s!"expected range with toRef and gitUrl, got {repr other}"

-- ---------------------------------------------------------------------------
-- dep: config file
-- ---------------------------------------------------------------------------

/-- Scenario: `dep` reads `fromRef` and `gitUrl` from a config file. -/
private def «dep config file» : IO Unit := do
  withTempDir "parse-tests-dep-cfg" fun dir => do
    let cfgPath := dir / "config.json"
    IO.FS.writeFile cfgPath "{\"fromRef\": \"cfgFrom\", \"gitUrl\": \"https://github.com/cfg/repo\"}"
    let config ← parse ["dep", "mathlib", "--to", "abc", "--config-file", cfgPath.toString]
    match config.itemSource with
    | .range (some "abc") (some "cfgFrom") (some "https://github.com/cfg/repo") => pure ()
    | other => fail s!"expected config file values to fill in fromRef/gitUrl, got {repr other}"

/-- Scenario: explicit CLI flags override config file values. -/
private def «dep cli overrides config file» : IO Unit := do
  withTempDir "parse-tests-dep-cfg-override" fun dir => do
    let cfgPath := dir / "config.json"
    IO.FS.writeFile cfgPath "{\"fromRef\": \"cfgFrom\"}"
    let config ← parse ["dep", "mathlib", "--from", "cliFrom", "--to", "abc",
                         "--config-file", cfgPath.toString]
    match config.itemSource with
    | .range (some "abc") (some "cliFrom") none => pure ()
    | other => fail s!"CLI --from should override config file fromRef, got {repr other}"

-- ---------------------------------------------------------------------------
-- dep: invalid parses
-- ---------------------------------------------------------------------------

/-- Scenario: `dep` with no source flags is rejected. -/
private def «dep no source» : IO Unit :=
  assertParseError ["dep", "mathlib"] "no source flags"

/-- Scenario: `dep` with no dependency name is rejected. -/
private def «dep no dep name» : IO Unit :=
  assertParseError ["dep"] "missing dep name"

/-- Scenario: `dep` with `--from` combined with `--commits-file` is rejected. -/
private def «dep from with commits file» : IO Unit :=
  assertParseError ["dep", "mathlib", "--commits-file", "/p", "--from", "abc"]
    "--from with --commits-file"

/-- Scenario: `dep` with both `--commits-file` and `--to` is rejected. -/
private def «dep commits file and to» : IO Unit :=
  assertParseError ["dep", "mathlib", "--commits-file", "/p", "--to", "abc"]
    "--commits-file and --to"

/-- Scenario: `dep --scan-mode bisect` combined with `--allow-dirty-workspace` parses successfully;
    the incompatibility is enforced at runtime by `Runner.run` (tested in IOTests). -/
private def «dep bisect with allow dirty workspace» : IO Unit := do
  let config ← parse ["dep", "mathlib", "--scan-mode", "bisect", "--allow-dirty-workspace", "--from", "a", "--to", "b"]
  assertEq .bisect config.runMode "bisect scan mode should be parsed"
  assertEq true config.allowDirtyWorkspace "allow-dirty-workspace flag should be parsed"

/-- Scenario: unknown flag under `dep` is rejected. -/
private def «dep unknown flag» : IO Unit :=
  assertParseError ["dep", "mathlib", "--unknown"] "unknown flag"

-- ---------------------------------------------------------------------------
-- toolchain: valid parses
-- ---------------------------------------------------------------------------

/-- Scenario: `toolchain` with `--toolchains-file` produces a file source. -/
private def «toolchain file source» : IO Unit := do
  let config ← parse ["toolchain", "--toolchains-file", "/p/toolchains.txt"]
  match config.itemSource with
  | .file path => assertEq "/p/toolchains.txt" path.toString "file path should match"
  | other => fail s!"expected file source, got {repr other}"
  assertEq .bisect config.runMode "should default to bisect mode"
  assertEq "toolchain" config.strategy.name "strategy scope should be 'toolchain'"
  assertEq false config.quiet "should default to quiet=false"

/-- Scenario: `toolchain` with `--scan-mode bisect` sets bisect mode. -/
private def «toolchain scan mode bisect» : IO Unit := do
  let config ← parse ["toolchain", "--toolchains-file", "/p/toolchains.txt", "--scan-mode", "bisect"]
  assertEq .bisect config.runMode "should be bisect mode"

/-- Scenario: `toolchain` with `--scan-mode linear` sets linear mode. -/
private def «toolchain scan mode linear» : IO Unit := do
  let config ← parse ["toolchain", "--toolchains-file", "/p/toolchains.txt", "--scan-mode", "linear"]
  assertEq .linear config.runMode "should be linear mode"

/-- Scenario: `toolchain` with `--quiet` sets quiet flag. -/
private def «toolchain quiet flag» : IO Unit := do
  let config ← parse ["toolchain", "--toolchains-file", "/p/toolchains.txt", "--quiet"]
  assertEq true config.quiet "should be quiet"

/-- Scenario: `toolchain` with `--allow-dirty-workspace` sets that flag. -/
private def «toolchain allow dirty workspace» : IO Unit := do
  let config ← parse ["toolchain", "--toolchains-file", "/p/toolchains.txt", "--allow-dirty-workspace"]
  assertEq true config.allowDirtyWorkspace "should allow dirty workspace"

/-- Scenario: `toolchain` with `--project-dir` sets project directory. -/
private def «toolchain project dir» : IO Unit := do
  let config ← parse ["toolchain", "--toolchains-file", "/p/f", "--project-dir", "/my/proj"]
  assertEq "/my/proj" config.projectDir.toString "project dir should match"

/-- Scenario: `toolchain` reads `toolchainsFile` from a config file. -/
private def «toolchain config file» : IO Unit := do
  withTempDir "parse-tests-tc-cfg" fun dir => do
    let cfgPath := dir / "config.json"
    IO.FS.writeFile cfgPath "{\"toolchainsFile\": \"/cfg/toolchains.txt\"}"
    let config ← parse ["toolchain", "--config-file", cfgPath.toString]
    match config.itemSource with
    | .file path => assertEq "/cfg/toolchains.txt" path.toString "config file path should be used"
    | other => fail s!"expected file source from config, got {repr other}"

-- ---------------------------------------------------------------------------
-- toolchain: invalid parses
-- ---------------------------------------------------------------------------

/-- Scenario: `toolchain` with no `--toolchains-file` is rejected. -/
private def «toolchain no file» : IO Unit :=
  assertParseError ["toolchain"] "missing --toolchains-file"

/-- Scenario: unknown flag under `toolchain` is rejected. -/
private def «toolchain unknown flag» : IO Unit :=
  assertParseError ["toolchain", "--toolchains-file", "/p", "--unknown"] "unknown flag"

-- ---------------------------------------------------------------------------
-- Top-level dispatch
-- ---------------------------------------------------------------------------

/-- Scenario: no arguments at all is rejected. -/
private def «no args» : IO Unit :=
  assertParseError [] "no args"

/-- Scenario: an unknown subcommand is rejected. -/
private def «unknown subcommand» : IO Unit :=
  assertParseError ["run", "mathlib"] "unknown subcommand"

/-- Scenario: `--commits-file` appearing as the last token with no following value is rejected;
    flag-value pairs require both the flag and its argument. -/
private def «dep --commits-file with no following argument is rejected» : IO Unit :=
  assertParseError ["dep", "mathlib", "--commits-file"]
    "--commits-file at end of args should fail, not silently use an empty path"

/-- Scenario: `--toolchains-file` appearing as the last token is rejected for the same reason. -/
private def «toolchain --toolchains-file with no following argument is rejected» : IO Unit :=
  assertParseError ["toolchain", "--toolchains-file"]
    "--toolchains-file at end of args should fail, not silently use an empty path"

def suite : TestSuite := #[
  test_case «dep range source»,
  test_case «dep file source»,
  test_case «dep scan mode bisect»,
  test_case «dep scan mode linear»,
  test_case «dep quiet flag»,
  test_case «dep allow dirty workspace»,
  test_case «dep project dir»,
  test_case «dep from only»,
  test_case «dep git url only»,
  test_case «dep to and git url»,
  test_case «dep config file»,
  test_case «dep cli overrides config file»,
  test_case «dep no source»,
  test_case «dep no dep name»,
  test_case «dep from with commits file»,
  test_case «dep commits file and to»,
  test_case «dep bisect with allow dirty workspace»,
  test_case «dep unknown flag»,
  test_case «toolchain file source»,
  test_case «toolchain scan mode bisect»,
  test_case «toolchain scan mode linear»,
  test_case «toolchain quiet flag»,
  test_case «toolchain allow dirty workspace»,
  test_case «toolchain project dir»,
  test_case «toolchain config file»,
  test_case «toolchain no file»,
  test_case «toolchain unknown flag»,
  test_case «no args»,
  test_case «unknown subcommand»,
  test_case «dep --commits-file with no following argument is rejected»,
  test_case «toolchain --toolchains-file with no following argument is rejected»
]

end HopscotchTestLib.ParseTests
