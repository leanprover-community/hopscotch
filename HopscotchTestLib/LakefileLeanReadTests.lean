import HopscotchTestLib.TestUtil

open Hopscotch

namespace HopscotchTestLib.LakefileLeanReadTests

/-- Write a `lakefile.lean` string to a temp file and return its path. -/
private def writeLakefileLean (dir : System.FilePath) (contents : String) : IO System.FilePath := do
  let path := dir / "lakefile.lean"
  IO.FS.writeFile path contents
  return path

/-- A minimal `lakefile.lean` with a single-line batteries require (URL + rev). -/
private def singleLineBlock : String :=
  String.intercalate "\n" [
    "import Lake",
    "open Lake DSL",
    "",
    "package \"demo\" where",
    "  version := v!\"0.1.0\"",
    "",
    "require batteries from git \"https://github.com/leanprover-community/batteries.git\" @ \"abc123\"",
    ""
  ]

/-- A `lakefile.lean` where the URL is on a continuation line. -/
private def multiLineUrlBlock : String :=
  String.intercalate "\n" [
    "require batteries from git",
    "  \"https://github.com/leanprover-community/batteries.git\" @ \"abc123\"",
    ""
  ]

/-- A `lakefile.lean` where the rev annotation is on a continuation line. -/
private def multiLineRevBlock : String :=
  String.intercalate "\n" [
    "require batteries from git \"https://github.com/leanprover-community/batteries.git\"",
    "  @ \"abc123\"",
    ""
  ]

/-- A `lakefile.lean` with a git dep but no rev annotation. -/
private def noRevBlock : String :=
  String.intercalate "\n" [
    "require batteries from git \"https://github.com/leanprover-community/batteries.git\"",
    ""
  ]

/-- A `lakefile.lean` with a path dep (no `from git`). -/
private def pathDepBlock : String :=
  String.intercalate "\n" [
    "require batteries from \"../batteries\"",
    ""
  ]

/-- A `lakefile.lean` with a Reservoir dep (`@` before `from`). -/
private def reservoirDepBlock : String :=
  String.intercalate "\n" [
    "require batteries @ \"v1.0.0\"",
    ""
  ]

/-- A `lakefile.lean` where the bare dependency name is quoted (e.g. `require "mathlib" from git`).
    This is valid Lake DSL and should be recognised alongside the unquoted form. -/
private def quotedBareNameBlock : String :=
  String.intercalate "\n" [
    "import Lake",
    "open Lake DSL",
    "",
    "package «PhysLib»",
    "",
    "require \"mathlib\" from git \"https://github.com/leanprover-community/mathlib4.git\" @ \"v4.28.0\"",
    ""
  ]

/-- A `lakefile.lean` with a scoped Reservoir dep using `@ git "rev"`. -/
private def scopedAtGitBlock : String :=
  String.intercalate "\n" [
    "import Lake",
    "open Lake DSL",
    "",
    "package \"demo\" where",
    "  version := v!\"0.1.0\"",
    "",
    "require \"leanprover-community\" / \"batteries\" @ git \"main\"",
    ""
  ]

-- ---------------------------------------------------------------------------
-- Read path tests
-- ---------------------------------------------------------------------------

/-- Scenario: `readLeanGitUrl` extracts the URL from a single-line require. -/
private def «readLeanGitUrl returns the git URL from a single-line require» : IO Unit := do
  withTempDir "hopscotch-lean-git-url" fun dir => do
    let path ← writeLakefileLean dir singleLineBlock
    let result ← LakefileProcessor.readLeanGitUrl path "batteries"
    assertEq (some "https://github.com/leanprover-community/batteries.git") result
      "readLeanGitUrl should return the URL from the single-line require"

/-- Scenario: `readLeanPinnedRev` extracts the rev from a single-line require. -/
private def «readLeanPinnedRev returns the rev from a single-line require» : IO Unit := do
  withTempDir "hopscotch-lean-rev-single" fun dir => do
    let path ← writeLakefileLean dir singleLineBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "batteries"
    assertEq (some "abc123") result
      "readLeanPinnedRev should return the rev from the single-line require"

/-- Scenario: `readLeanGitUrl` handles a multi-line require where the URL is on a
    continuation line. -/
private def «readLeanGitUrl handles multi-line URL continuation» : IO Unit := do
  withTempDir "hopscotch-lean-git-url-multiline" fun dir => do
    let path ← writeLakefileLean dir multiLineUrlBlock
    let result ← LakefileProcessor.readLeanGitUrl path "batteries"
    assertEq (some "https://github.com/leanprover-community/batteries.git") result
      "readLeanGitUrl should return the URL even when it is on a continuation line"

/-- Scenario: `readLeanPinnedRev` handles a multi-line require where the rev is on a
    separate continuation line. -/
private def «readLeanPinnedRev handles multi-line rev continuation» : IO Unit := do
  withTempDir "hopscotch-lean-rev-multiline" fun dir => do
    let path ← writeLakefileLean dir multiLineRevBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "batteries"
    assertEq (some "abc123") result
      "readLeanPinnedRev should return the rev even when it is on a continuation line"

/-- Scenario: `readLeanGitUrl` returns `none` without throwing when the dependency is absent. -/
private def «readLeanGitUrl returns none when the dependency block is absent» : IO Unit := do
  withTempDir "hopscotch-lean-git-url-absent" fun dir => do
    let path ← writeLakefileLean dir singleLineBlock
    let result ← LakefileProcessor.readLeanGitUrl path "mathlib"
    assertEq (none : Option String) result
      "readLeanGitUrl should return none for an absent dependency, not throw"

/-- Scenario: `readLeanPinnedRev` returns `none` for a path dep that has no git URL. -/
private def «readLeanPinnedRev returns none for a path dep» : IO Unit := do
  withTempDir "hopscotch-lean-rev-path" fun dir => do
    let path ← writeLakefileLean dir pathDepBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "batteries"
    assertEq (none : Option String) result
      "readLeanPinnedRev should return none when there is no from git clause"

/-- Scenario: `readLeanPinnedRev` returns `none` for a Reservoir dep (`@` before `from git`). -/
private def «readLeanPinnedRev returns none for a Reservoir dep» : IO Unit := do
  withTempDir "hopscotch-lean-rev-reservoir" fun dir => do
    let path ← writeLakefileLean dir reservoirDepBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "batteries"
    assertEq (none : Option String) result
      "readLeanPinnedRev should return none for a Reservoir dep (no from git)"

/-- Scenario: `readLeanPinnedRev` returns `none` when the git URL is present but no rev
    annotation exists. This triggers a fallback to the default-branch tip. -/
private def «readLeanPinnedRev returns none when rev is absent» : IO Unit := do
  withTempDir "hopscotch-lean-rev-absent" fun dir => do
    let path ← writeLakefileLean dir noRevBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "batteries"
    assertEq (none : Option String) result
      "readLeanPinnedRev should return none when no @ annotation exists"

-- ---------------------------------------------------------------------------
-- Scoped `require "scope" / "name" @ git "rev"` tests
-- ---------------------------------------------------------------------------

/-- Scenario: `readLeanPinnedRev` extracts the rev from `@ git "rev"` for a scoped dep. -/
private def «readLeanPinnedRev returns rev from scoped @ git dep» : IO Unit := do
  withTempDir "hopscotch-lean-scoped-rev" fun dir => do
    let path ← writeLakefileLean dir scopedAtGitBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "leanprover-community/batteries"
    assertEq (some "main") result
      "readLeanPinnedRev should extract the rev from @ git for a scoped dep"

/-- Scenario: `readLeanGitUrl` returns `none` for a Reservoir dep with `@ git "rev"`,
    since there is no explicit URL in the syntax. -/
private def «readLeanGitUrl returns none for scoped @ git dep» : IO Unit := do
  withTempDir "hopscotch-lean-scoped-url" fun dir => do
    let path ← writeLakefileLean dir scopedAtGitBlock
    let result ← LakefileProcessor.readLeanGitUrl path "leanprover-community/batteries"
    assertEq (none : Option String) result
      "readLeanGitUrl should return none when there is no from git clause"

/-- Scenario: `readLeanScope` returns the scope from a scoped dependency name. -/
private def «readLeanScope returns scope from scoped dep name» : IO Unit := do
  withTempDir "hopscotch-lean-scope" fun dir => do
    let path ← writeLakefileLean dir scopedAtGitBlock
    let result ← LakefileProcessor.readLeanScope path "leanprover-community/batteries"
    assertEq (some "leanprover-community") result
      "readLeanScope should extract the scope from the dependency name"
  withTempDir "hopscotch-lean-scope-bare" fun dir => do
    let path ← writeLakefileLean dir singleLineBlock
    let result ← LakefileProcessor.readLeanScope path "batteries"
    assertEq (none : Option String) result
      "readLeanScope should return none for a bare (unscoped) dependency name"

/-- Scenario: `rewriteLeanContents` rewrites `@ git "old"` to `@ git "new"` for a scoped dep. -/
private def «rewriteLeanContents rewrites scoped @ git rev» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents scopedAtGitBlock
    "leanprover-community/batteries" "newrev"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "@ git \"newrev\"")
        "rewritten content should contain the new @ git rev"
      assertTrue (!rewritten.contains "@ git \"main\"")
        "rewritten content should not contain the old @ git rev"

-- ---------------------------------------------------------------------------
-- Quoted bare name tests
-- ---------------------------------------------------------------------------

/-- Scenario: `readLeanGitUrl` recognises `require "mathlib"` (quoted bare name). -/
private def «readLeanGitUrl handles quoted bare name» : IO Unit := do
  withTempDir "hopscotch-lean-quoted-bare-url" fun dir => do
    let path ← writeLakefileLean dir quotedBareNameBlock
    let result ← LakefileProcessor.readLeanGitUrl path "mathlib"
    assertEq (some "https://github.com/leanprover-community/mathlib4.git") result
      "readLeanGitUrl should recognise a quoted bare dependency name"

/-- Scenario: `readLeanPinnedRev` recognises `require "mathlib"` (quoted bare name). -/
private def «readLeanPinnedRev handles quoted bare name» : IO Unit := do
  withTempDir "hopscotch-lean-quoted-bare-rev" fun dir => do
    let path ← writeLakefileLean dir quotedBareNameBlock
    let result ← LakefileProcessor.readLeanPinnedRev path "mathlib"
    assertEq (some "v4.28.0") result
      "readLeanPinnedRev should recognise a quoted bare dependency name"

/-- Scenario: `rewriteLeanContents` can rewrite a rev in a quoted-bare-name require block. -/
private def «rewriteLeanContents rewrites quoted bare name block» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents quotedBareNameBlock "mathlib" "newrev"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "@ \"newrev\"")
        "rewritten content should contain the new rev annotation"
      assertTrue (!rewritten.contains "@ \"v4.28.0\"")
        "rewritten content should not contain the old rev annotation"

-- ---------------------------------------------------------------------------
-- Rewrite path tests
-- ---------------------------------------------------------------------------

/-- Scenario: `rewriteLeanContents` rewrites an existing single-line rev. -/
private def «rewriteLeanContents rewrites an existing single-line rev» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents singleLineBlock "batteries" "newrev"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "@ \"newrev\"")
        "rewritten content should contain the new rev annotation"
      assertTrue (!rewritten.contains "@ \"abc123\"")
        "rewritten content should not contain the old rev annotation"

/-- Scenario: `rewriteLeanContents` rewrites a rev that is on a separate continuation line. -/
private def «rewriteLeanContents rewrites an existing multi-line rev» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents multiLineRevBlock "batteries" "newrev"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "@ \"newrev\"")
        "rewritten content should contain the new rev annotation"
      assertTrue (!rewritten.contains "@ \"abc123\"")
        "rewritten content should not contain the old rev annotation"

/-- Scenario: `rewriteLeanContents` inserts a new `@ "rev"` when none exists. -/
private def «rewriteLeanContents inserts rev when absent» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents noRevBlock "batteries" "freshrev"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "@ \"freshrev\"")
        "rewritten content should contain the inserted rev annotation"
      assertTrue (rewritten.contains "batteries.git")
        "original URL line should be preserved"

/-- Scenario: rewriting one dependency does not alter other `require` blocks. -/
private def «rewriteLeanContents preserves other require blocks unchanged» : IO Unit := do
  let contents := String.intercalate "\n" [
    "require batteries from git \"https://github.com/leanprover-community/batteries.git\" @ \"old\"",
    "require mathlib from git \"https://github.com/leanprover-community/mathlib4.git\" @ \"untouched\"",
    ""
  ]
  let result := LakefileProcessor.rewriteLeanContents contents "batteries" "new"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "batteries.git\" @ \"new\"")
        "batteries rev should be rewritten"
      assertTrue (rewritten.contains "mathlib4.git\" @ \"untouched\"")
        "mathlib rev should be unchanged"

/-- Scenario: `rewriteLeanContents` returns an error when the dependency is absent. -/
private def «rewriteLeanContents errors when dependency is absent» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents singleLineBlock "mathlib" "rev"
  match result with
  | .ok _ => fail "rewriteLeanContents should reject a missing dependency, but succeeded"
  | .error _ => pure ()

/-- Scenario: `rewriteLeanContents` returns an error for a non-git dependency. -/
private def «rewriteLeanContents errors for a non-git dep» : IO Unit := do
  let result := LakefileProcessor.rewriteLeanContents pathDepBlock "batteries" "rev"
  match result with
  | .ok _ => fail "rewriteLeanContents should reject a non-git dep, but succeeded"
  | .error _ => pure ()

/-- Scenario: `rewriteLeanContents` preserves CRLF line endings throughout. -/
private def «rewriteLeanContents preserves CRLF line endings» : IO Unit := do
  let crlfBlock := String.intercalate "\r\n" [
    "require batteries from git \"https://github.com/leanprover-community/batteries.git\" @ \"abc123\"",
    ""
  ]
  let result := LakefileProcessor.rewriteLeanContents crlfBlock "batteries" "newrev"
  match result with
  | .error e => fail s!"rewriteLeanContents failed unexpectedly: {e}"
  | .ok rewritten =>
      assertTrue (rewritten.contains "\r\n")
        "CRLF line endings should be preserved after rewriting"
      assertTrue (rewritten.contains "@ \"newrev\"")
        "the new rev should appear in the CRLF-formatted output"

/-- Scenario: `rewriteLeanFile` writes the rewritten contents back to disk. -/
private def «rewriteLeanFile writes changes to disk» : IO Unit := do
  withTempDir "hopscotch-lean-rewrite-file" fun dir => do
    let path ← writeLakefileLean dir singleLineBlock
    LakefileProcessor.rewriteLeanFile path "batteries" "diskrev"
    let contents ← IO.FS.readFile path
    assertTrue (contents.contains "@ \"diskrev\"")
      "the file on disk should contain the new rev after rewriteLeanFile"
    assertTrue (!contents.contains "@ \"abc123\"")
      "the file on disk should not contain the old rev after rewriteLeanFile"

def suite : TestSuite := #[
  test_case «readLeanGitUrl returns the git URL from a single-line require»,
  test_case «readLeanPinnedRev returns the rev from a single-line require»,
  test_case «readLeanGitUrl handles multi-line URL continuation»,
  test_case «readLeanPinnedRev handles multi-line rev continuation»,
  test_case «readLeanGitUrl returns none when the dependency block is absent»,
  test_case «readLeanPinnedRev returns none for a path dep»,
  test_case «readLeanPinnedRev returns none for a Reservoir dep»,
  test_case «readLeanPinnedRev returns none when rev is absent»,
  test_case «readLeanGitUrl handles quoted bare name»,
  test_case «readLeanPinnedRev handles quoted bare name»,
  test_case «rewriteLeanContents rewrites quoted bare name block»,
  test_case «readLeanPinnedRev returns rev from scoped @ git dep»,
  test_case «readLeanGitUrl returns none for scoped @ git dep»,
  test_case «readLeanScope returns scope from scoped dep name»,
  test_case «rewriteLeanContents rewrites scoped @ git rev»,
  test_case «rewriteLeanContents rewrites an existing single-line rev»,
  test_case «rewriteLeanContents rewrites an existing multi-line rev»,
  test_case «rewriteLeanContents inserts rev when absent»,
  test_case «rewriteLeanContents preserves other require blocks unchanged»,
  test_case «rewriteLeanContents errors when dependency is absent»,
  test_case «rewriteLeanContents errors for a non-git dep»,
  test_case «rewriteLeanContents preserves CRLF line endings»,
  test_case «rewriteLeanFile writes changes to disk»
]

end HopscotchTestLib.LakefileLeanReadTests
