import HopscotchTestLib.TestUtil

open Hopscotch

namespace HopscotchTestLib.LakefileSetPathTests

private def makeMultiline (strings : List String) : String :=
  String.intercalate "\n" strings

-- ---------------------------------------------------------------------------
-- TOML tests
-- ---------------------------------------------------------------------------

private def «setPathContents drops git and rev, inserts path» : IO Unit := do
  let base := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"abc123\"",
    ""
  ]
  let expected := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "path = \"/local/mathlib\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setPathContents base "mathlib" "/local/mathlib"
  assertEq expected result "should drop git and rev, insert path after name"

private def «setPathContents drops source line, inserts path» : IO Unit := do
  let base := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "source = { git = \"https://github.com/leanprover-community/batteries.git\", rev = \"main\" }",
    ""
  ]
  let expected := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "path = \"/local/batteries\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setPathContents base "batteries" "/local/batteries"
  assertEq expected result "should drop source line, insert path after name"

private def «setPathContents replaces existing path» : IO Unit := do
  let base := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "path = \"/old/path\"",
    ""
  ]
  let expected := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "path = \"/new/path\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setPathContents base "batteries" "/new/path"
  assertEq expected result "should replace existing path value"

private def «setPathContents preserves scope and drops git and rev» : IO Unit := do
  let base := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "scope = \"leanprover-community\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\"",
    ""
  ]
  let expected := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "path = \"/local/batteries\"",
    "scope = \"leanprover-community\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setPathContents base "batteries" "/local/batteries"
  assertEq expected result "should preserve scope and drop git/rev"

private def «setPathContents preserves CRLF newlines» : IO Unit := do
  let crlf := "\r\n"
  let base := String.intercalate crlf [
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"abc123\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setPathContents base "mathlib" "/local/mathlib"
  assertTrue (result.contains crlf) "should preserve CRLF line endings"
  assertTrue (result.contains "path = \"/local/mathlib\"") "should insert path field"
  assertTrue (!result.contains "git =") "should remove git field"
  assertTrue (!result.contains "rev =") "should remove rev field"

private def «setPathContents throws on missing dep» : IO Unit := do
  let base := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://example.com/batteries.git\"",
    ""
  ]
  match LakefileProcessor.setPathContents base "mathlib" "/local/mathlib" with
  | .ok _ => fail "should throw for missing dependency"
  | .error _ => pure ()

private def «setPathContents throws on duplicate blocks» : IO Unit := do
  let base := makeMultiline [
    "[[require]]",
    "name = \"batteries\"",
    "rev = \"old1\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "rev = \"old2\"",
    ""
  ]
  match LakefileProcessor.setPathContents base "batteries" "/local/batteries" with
  | .ok _ => fail "should throw for duplicate [[require]] blocks"
  | .error _ => pure ()

-- ---------------------------------------------------------------------------
-- Lean tests
-- ---------------------------------------------------------------------------

private def «setLeanPathContents rewrites single-line git dep» : IO Unit := do
  let base := makeMultiline [
    "import Lake",
    "open Lake DSL",
    "",
    "require batteries from git \"https://github.com/leanprover-community/batteries.git\" @ \"abc123\"",
    ""
  ]
  let expected := makeMultiline [
    "import Lake",
    "open Lake DSL",
    "",
    "require batteries from \"/local/batteries\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setLeanPathContents base "batteries" "/local/batteries"
  assertEq expected result "should rewrite single-line git dep to path dep"

private def «setLeanPathContents rewrites scoped dep» : IO Unit := do
  let base := makeMultiline [
    "require \"leanprover-community\" / \"batteries\" from git \"https://github.com/leanprover-community/batteries.git\" @ \"abc123\"",
    ""
  ]
  let expected := makeMultiline [
    "require \"leanprover-community\" / \"batteries\" from \"/local/batteries\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setLeanPathContents base "leanprover-community/batteries" "/local/batteries"
  assertEq expected result "should rewrite scoped dep to path dep"

private def «setLeanPathContents normalizes multi-line block to single line» : IO Unit := do
  let base := makeMultiline [
    "require batteries from git",
    "  \"https://github.com/leanprover-community/batteries.git\" @ \"abc123\"",
    ""
  ]
  let expected := makeMultiline [
    "require batteries from \"/local/batteries\"",
    ""
  ]
  let result ← IO.ofExcept <| LakefileProcessor.setLeanPathContents base "batteries" "/local/batteries"
  assertEq expected result "should collapse multi-line block to single require line"

private def «setLeanPathContents throws on missing require» : IO Unit := do
  let base := makeMultiline [
    "require batteries from git \"https://example.com/batteries.git\" @ \"abc123\"",
    ""
  ]
  match LakefileProcessor.setLeanPathContents base "mathlib" "/local/mathlib" with
  | .ok _ => fail "should throw when require block is absent"
  | .error _ => pure ()

private def «setLeanPathContents throws on with clause» : IO Unit := do
  let base := makeMultiline [
    "require batteries from git \"https://example.com/batteries.git\" @ \"abc123\" with {}",
    ""
  ]
  match LakefileProcessor.setLeanPathContents base "batteries" "/local/batteries" with
  | .ok _ => fail "should throw when with clause is present"
  | .error _ => pure ()

def suite : TestSuite := #[
  test_case «setPathContents drops git and rev, inserts path»,
  test_case «setPathContents drops source line, inserts path»,
  test_case «setPathContents replaces existing path»,
  test_case «setPathContents preserves scope and drops git and rev»,
  test_case «setPathContents preserves CRLF newlines»,
  test_case «setPathContents throws on missing dep»,
  test_case «setPathContents throws on duplicate blocks»,
  test_case «setLeanPathContents rewrites single-line git dep»,
  test_case «setLeanPathContents rewrites scoped dep»,
  test_case «setLeanPathContents normalizes multi-line block to single line»,
  test_case «setLeanPathContents throws on missing require»,
  test_case «setLeanPathContents throws on with clause»
]

end HopscotchTestLib.LakefileSetPathTests
