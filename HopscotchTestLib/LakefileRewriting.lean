import HopscotchTestLib.TestUtil

open Hopscotch
open Hopscotch.State

namespace HopscotchTestLib.LakefileRewriting

private def makeMultiline (strings : List String) : String :=
  String.intercalate "\n" strings

/-- Basic rewrite tests --/
private def «lakefile dependency rev rewriting basic test» : IO Unit := do
  -- Prepare: a lakefile with two dependency blocks and one target rev to rewrite.
  let base := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"old\" # keep this comment",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\"",
    ""
  ]

  let expectedRewrite := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"new\" # keep this comment",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\"",
    ""
  ]

  -- Act: rewrite the target dependency rev in the prepared lakefile.
  let rewritten ← IO.ofExcept <| LakefileProcessor.rewriteContents base "mathlib" "new"
  -- Assert: the rewritten lakefile changes only the requested dependency rev.
  assertEq expectedRewrite rewritten
    "rewrite should update only the requested dependency rev, preserving formatting and comments"

/-- Basic rewrite tests --/
private def «lakefile dependency rev rewriting basic test with lean_lib multiblock» : IO Unit := do
  -- Prepare: a lakefile with two dependency blocks and one target rev to rewrite.
  let base := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"old\" # keep this comment",
    "",
    "[[lean_lib]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\""
  ]

  let expectedRewrite := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"new\" # keep this comment",
    "",
    "[[lean_lib]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\""
  ]

  -- Act: rewrite the target dependency rev in the prepared lakefile.
  let rewritten ← IO.ofExcept <| LakefileProcessor.rewriteContents base "mathlib" "new"
  -- Assert: the rewritten lakefile changes only the requested dependency rev.
  assertEq expectedRewrite rewritten
    "rewrite should update only the requested dependency rev, preserving formatting and comments"


  /-- Basic rewrite tests --/
private def «lakefile dependency rev rewriting basic test with missing rev lean_lib multiblock» : IO Unit := do
  -- Prepare: a lakefile with two dependency blocks and one target rev to rewrite.
  let base := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "",
    "[[lean_lib]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\""
  ]

  let expectedRewrite := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://github.com/leanprover-community/mathlib4.git\"",
    "rev = \"new\"",
    "",
    "[[lean_lib]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\""
  ]

  -- Act: rewrite the target dependency rev in the prepared lakefile.
  let rewritten ← IO.ofExcept <| LakefileProcessor.rewriteContents base "mathlib" "new"
  -- Assert: the rewritten lakefile changes only the requested dependency rev.
  assertEq expectedRewrite rewritten
    "rewrite should update only the requested dependency rev, preserving formatting and comments"


private def «lakefile dependency rev rewriting: missing rev» : IO Unit := do
  -- Prepare: a matching dependency block without a rev line.
  let missingRev := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    ""
  ]

  -- Prepare: a matching dependency block without a rev line.
  let expectedRewrite := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"inserted\"",
    ""
  ]

  -- Act: rewrite
  let insertedRev ← IO.ofExcept <| LakefileProcessor.rewriteContents missingRev "batteries" "inserted"
  -- Assert: the rev is inserted into the block rather than causing an error.

  assertEq expectedRewrite insertedRev
    "rewrite should insert a rev entry when none exists"


private def «lakefile dependency rev rewriting: missing rev multiblock» : IO Unit := do
  -- Prepare: a matching dependency block without a rev line.
  let missingRev := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "",
    "[[require]]",
    "name = \"something else\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    ""
  ]

  -- Prepare: a matching dependency block without a rev line.
  let expectedRewrite := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"inserted\"",
    "",
    "[[require]]",
    "name = \"something else\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    ""
  ]

  -- Act: rewrite
  let insertedRev ← IO.ofExcept <| LakefileProcessor.rewriteContents missingRev "batteries" "inserted"
  -- Assert: the rev is inserted into the block rather than causing an error.

  assertEq expectedRewrite insertedRev
    "rewrite should insert a rev entry when none exists"


private def «lakefile dependency rev rewriting: weird whitespace» : IO Unit := do
  -- Prepare: a matching dependency block without a rev line.
  let missingRev := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "",
    "name = \"batteries\"",
    "",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "",
    "",
    "",
    "",
    "[[require]]",
    "",
    "name = \"something else\"",
    "",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    ""
  ]

  -- Prepare: a matching dependency block without a rev line.
  let expectedRewrite := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "",
    "name = \"batteries\"",
    "",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"inserted\"",
    "",
    "",
    "",
    "",
    "[[require]]",
    "",
    "name = \"something else\"",
    "",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    ""
  ]

  -- Act: rewrite
  let insertedRev ← IO.ofExcept <| LakefileProcessor.rewriteContents missingRev "batteries" "inserted"
  -- Assert: the rev is inserted into the block rather than causing an error.

  assertEq expectedRewrite insertedRev
    "rewrite should insert a rev entry when none exists"


/-- Scenario: the lakefile rewrite updates the first matching dependency rev in place. -/
private def «lakefile dependency failed rev rewriting» : IO Unit := do
  -- Prepare: a lakefile that omits the requested dependency block.
  let missing := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "rev = \"main\"",
    ""
  ]

  -- Act: attempt the rewrite
  -- Asssert the missing dependency is rejected.
  match LakefileProcessor.rewriteContents missing "batteries" "oops" with
  | .ok _ => fail "missing dependency should be rejected"
  | .error _ => pure ()

  -- Prepare: a lakefile with duplicate dependency blocks for the requested name.
  let duplicate := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "rev = \"old1\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "rev = \"old2\"",
    ""
  ]

  -- Act: attempt the rewrite
  -- Assert: duplicate dependency blocks are rejected.
  match LakefileProcessor.rewriteContents duplicate "batteries" "oops" with
  | .ok _ => fail "multiple matching dependency blocks should be rejected"
  | .error _ => pure ()

  -- Prepare: a matching dependency block without a rev that is followed by another block.
  let missingRevMulti := makeMultiline [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "",
    "[[require]]",
    "name = \"mathlib\"",
    "rev = \"keepme\"",
    ""
  ]

  -- Act: insert a rev only into the target block when followed by another block.
  let insertedRevMulti ← IO.ofExcept <| LakefileProcessor.rewriteContents missingRevMulti "batteries" "inserted2"
  -- Assert: only the target block receives the inserted rev; the other block is untouched.
  assertTrue (insertedRevMulti.contains "rev = \"inserted2\"")
    "rewrite should insert a rev entry before the next [[require]] block"
  assertTrue (insertedRevMulti.contains "rev = \"keepme\"")
    "rewrite should leave other dependency revs untouched when inserting"
  -- Prepare: a CRLF-formatted lakefile with an inline rev comment.
  let crlf := "\r\n"
  let baseCrlf := String.intercalate crlf [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"old\" # trailing comment",
    ""
  ]

  -- Act: rewrite the target dependency rev in the CRLF-formatted lakefile.
  let rewrittenCrlf ← IO.ofExcept <| LakefileProcessor.rewriteContents baseCrlf "batteries" "newcrlf"
  -- Assert: the CRLF rewrite preserves the inline suffix and line endings.
  assertTrue (rewrittenCrlf.contains s!"rev = \"newcrlf\" # trailing comment")
    "rewrite should preserve inline suffixes on the updated rev line"
  assertTrue (rewrittenCrlf.contains crlf)
    "rewrite should preserve CRLF line endings"

/-- Regression: `set --path` then `set --rev` on a TOML block used to silently
emit both `path` and `rev`, producing a lakefile Lake refuses to load. -/
private def «rewriteContents rejects rev on path-source block» : IO Unit := do
  let pathSource := makeMultiline [
    "[[require]]",
    "name = \"mathlib\"",
    "path = \"/local/mathlib\"",
    ""
  ]
  match LakefileProcessor.rewriteContents pathSource "mathlib" "abc123" with
  | .ok _ => fail "should refuse to pin a rev on a path-source dependency"
  | .error _ => pure ()

/-- A quote inside the rev would close the TOML string early and produce
invalid output, so the rewriter must reject it. -/
private def «rewriteContents rejects rev containing a quote» : IO Unit := do
  let base := makeMultiline [
    "[[require]]",
    "name = \"mathlib\"",
    "git = \"https://example.com/m.git\"",
    "rev = \"old\"",
    ""
  ]
  match LakefileProcessor.rewriteContents base "mathlib" "ab\"cd" with
  | .ok _ => fail "should reject a rev containing a double quote"
  | .error _ => pure ()

/-- Same defense for the `lakefile.lean` rewriter. -/
private def «rewriteLeanContents rejects rev containing a quote» : IO Unit := do
  let base := makeMultiline [
    "require batteries from git \"https://example.com/b.git\" @ \"old\"",
    ""
  ]
  match LakefileProcessor.rewriteLeanContents base "batteries" "ab\"cd" with
  | .ok _ => fail "should reject a rev containing a double quote"
  | .error _ => pure ()

def suite : TestSuite := #[
  test_case «lakefile dependency rev rewriting basic test»,
  test_case «lakefile dependency rev rewriting basic test with lean_lib multiblock»,
  test_case «lakefile dependency rev rewriting basic test with missing rev lean_lib multiblock»,
  test_case «lakefile dependency rev rewriting: missing rev»,
  test_case «lakefile dependency rev rewriting: missing rev multiblock»,
  test_case «lakefile dependency rev rewriting: weird whitespace»,
  test_case «lakefile dependency failed rev rewriting»,
  test_case «rewriteContents rejects rev on path-source block»,
  test_case «rewriteContents rejects rev containing a quote»,
  test_case «rewriteLeanContents rejects rev containing a quote»
]

end HopscotchTestLib.LakefileRewriting
