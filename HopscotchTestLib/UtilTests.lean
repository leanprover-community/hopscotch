import HopscotchTestLib.TestUtil

open Hopscotch

namespace HopscotchTestLib.UtilTests

/-- Scenario: `shortCommit` truncates long SHAs and leaves short strings unchanged. -/
private def «shortCommit truncates long hashes to twelve characters» : IO Unit := do
  -- A full 40-character SHA is truncated to 12.
  assertEq "abcdef012345"
    (shortCommit "abcdef012345678901234567890123456789abcd")
    "40-char SHA should be truncated to 12 characters"
  -- A string already under 12 characters passes through verbatim.
  assertEq "abc"
    (shortCommit "abc")
    "short strings should not be padded or changed"
  -- An empty string returns an empty string.
  assertEq ""
    (shortCommit "")
    "empty string should return empty string"
  -- Exactly 12 characters is returned unchanged.
  assertEq "abcdef012345"
    (shortCommit "abcdef012345")
    "exactly 12 characters should pass through unchanged"

/-- Scenario: `sanitizeForFileName` replaces non-portable characters with underscores. -/
private def «sanitizeForFileName replaces non-portable characters with underscores» : IO Unit := do
  -- Alphanumeric characters and hyphens are preserved.
  assertEq "abc-XYZ-123"
    (sanitizeForFileName "abc-XYZ-123")
    "alphanumeric characters and hyphens should pass through unchanged"
  -- Slashes, colons, dots, and spaces become underscores.
  assertEq "a_b_c_d_e"
    (sanitizeForFileName "a/b:c.d e")
    "slashes, colons, dots, and spaces should be replaced with underscores"
  -- An empty string returns an empty string.
  assertEq ""
    (sanitizeForFileName "")
    "empty string should return empty string"
  -- A string of only special characters becomes all underscores.
  assertEq "___"
    (sanitizeForFileName "/.@")
    "all-special strings should become all underscores"

/-- Scenario: `colorize` wraps all ConsoleStyle values with distinct ANSI codes when enabled
    and leaves text unchanged when disabled. -/
private def «colorize wraps all ConsoleStyle values with distinct ANSI codes when enabled» : IO Unit := do
  let ansiPrefix := String.singleton (Char.ofNat 27) ++ "["
  let ansiReset  := String.singleton (Char.ofNat 27) ++ "[0m"
  let styles : List ConsoleStyle := [
    .attempt, .info, .running, .success, .failure, .path, .header
  ]
  -- Enabled mode: every style produces an ANSI-prefixed string ending with the reset code.
  for style in styles do
    let result := colorize .enabled style "text"
    assertTrue (result.startsWith ansiPrefix)
      s!"enabled colorize for {repr style} should start with ANSI prefix"
    assertTrue (result.endsWith ansiReset)
      s!"enabled colorize for {repr style} should end with ANSI reset"
    assertTrue (result.contains "text")
      s!"enabled colorize for {repr style} should contain the original text"
  -- Disabled mode: every style returns the text unchanged.
  for style in styles do
    let result := colorize .disabled style "plain"
    assertEq "plain" result
      s!"disabled colorize for {repr style} should leave text unchanged"
  -- All enabled styles produce distinct ANSI code sequences.
  let codes := styles.map fun style =>
    let result := colorize .enabled style "x"
    -- Extract the code between ESC[ and m.
    match (result.drop ansiPrefix.length |>.copy).splitOn "m" with
    | code :: _ => code
    | []        => ""
  let uniqueCodes := codes.foldl (fun acc c => if acc.contains c then acc else acc ++ [c]) []
  assertEq codes.length uniqueCodes.length
    "each ConsoleStyle should produce a distinct ANSI code"

def suite : TestSuite := #[
  test_case «shortCommit truncates long hashes to twelve characters»,
  test_case «sanitizeForFileName replaces non-portable characters with underscores»,
  test_case «colorize wraps all ConsoleStyle values with distinct ANSI codes when enabled»
]

end HopscotchTestLib.UtilTests
