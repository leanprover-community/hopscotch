import HopscotchTestLib.TestUtil

open Hopscotch

namespace HopscotchTestLib.LakefileReadTests

/-- Write a TOML string to a temp file and return its path. -/
private def writeLakefile (dir : System.FilePath) (contents : String) : IO System.FilePath := do
  let path := dir / "lakefile.toml"
  IO.FS.writeFile path contents
  return path

/-- A minimal lakefile with a batteries block that has all three fields. -/
private def fullBlock : String :=
  String.intercalate "\n" [
    "name = \"demo\"",
    "",
    "[[require]]",
    "name = \"batteries\"",
    "scope = \"leanprover-community\"",
    "git = \"https://github.com/leanprover-community/batteries.git\"",
    "rev = \"abc123\"",
    ""
  ]

/-- Scenario: `readGitUrl` extracts the `git` field from the named dependency block. -/
private def «readGitUrl returns the git field from a matching block» : IO Unit := do
  withTempDir "hopscotch-read-git" fun dir => do
    let path ← writeLakefile dir fullBlock
    let result ← LakefileProcessor.readGitUrl path "batteries"
    assertEq (some "https://github.com/leanprover-community/batteries.git") result
      "readGitUrl should return the git URL from the matching block"

/-- Scenario: `readGitUrl` returns `none` without throwing when the dependency is absent. -/
private def «readGitUrl returns none when the dependency block is absent» : IO Unit := do
  withTempDir "hopscotch-read-git-absent" fun dir => do
    let path ← writeLakefile dir fullBlock
    let result ← LakefileProcessor.readGitUrl path "mathlib"
    assertEq (none : Option String) result
      "readGitUrl should return none for an absent dependency, not throw"

/-- Scenario: `readScope` returns `none` when the field is absent from the block. -/
private def «readScope returns none when the scope field is absent from the block» : IO Unit := do
  withTempDir "hopscotch-read-scope-absent" fun dir => do
    -- A block that exists but has no scope field.
    let contents := String.intercalate "\n" [
      "name = \"demo\"",
      "",
      "[[require]]",
      "name = \"batteries\"",
      "git = \"https://github.com/leanprover-community/batteries.git\"",
      "rev = \"abc123\"",
      ""
    ]
    let path ← writeLakefile dir contents
    let result ← LakefileProcessor.readScope path "batteries"
    assertEq (none : Option String) result
      "readScope should return none when the scope field is absent"
  withTempDir "hopscotch-read-scope-present" fun dir => do
    let path ← writeLakefile dir fullBlock
    let result ← LakefileProcessor.readScope path "batteries"
    assertEq (some "leanprover-community") result
      "readScope should return the scope field when present"

/-- Scenario: `readPinnedRev` extracts the `rev` field from the named dependency block. -/
private def «readPinnedRev returns the rev field from a matching block» : IO Unit := do
  withTempDir "hopscotch-read-rev" fun dir => do
    let path ← writeLakefile dir fullBlock
    let result ← LakefileProcessor.readPinnedRev path "batteries"
    assertEq (some "abc123") result
      "readPinnedRev should return the rev from the matching block"

/-- Scenario: `readPinnedRev` returns `none` when the block exists but has no `rev` entry.
    This is the value returned to `lakefileStrategy.defaultFromRef`, which triggers a
    fallback to the default-branch tip. -/
private def «readPinnedRev returns none when no rev field exists» : IO Unit := do
  withTempDir "hopscotch-read-rev-absent" fun dir => do
    let contents := String.intercalate "\n" [
      "name = \"demo\"",
      "",
      "[[require]]",
      "name = \"batteries\"",
      "git = \"https://github.com/leanprover-community/batteries.git\"",
      ""
    ]
    let path ← writeLakefile dir contents
    let result ← LakefileProcessor.readPinnedRev path "batteries"
    assertEq (none : Option String) result
      "readPinnedRev should return none when the rev field is absent"

/-- Scenario: the read path silently uses the first matching block when dependency names are
    duplicated, unlike `rewriteContents` which rejects duplicates with an error.
    This asymmetry is intentional: reads are best-effort, writes must be unambiguous. -/
private def «read path silently uses the first block when names are duplicated» : IO Unit := do
  withTempDir "hopscotch-read-dup" fun dir => do
    let contents := String.intercalate "\n" [
      "name = \"demo\"",
      "",
      "[[require]]",
      "name = \"batteries\"",
      "rev = \"first\"",
      "",
      "[[require]]",
      "name = \"batteries\"",
      "rev = \"second\"",
      ""
    ]
    let path ← writeLakefile dir contents
    -- All three read functions return the first block's value, not an error.
    let rev ← LakefileProcessor.readPinnedRev path "batteries"
    assertEq (some "first") rev
      "readPinnedRev should silently use the first block when names are duplicated"
    -- Confirm this is asymmetric: rewriteContents rejects the same input.
    let rewriteResult := LakefileProcessor.rewriteContents contents "batteries" "new"
    match rewriteResult with
    | .ok _    => fail "rewriteContents should reject duplicate blocks, but succeeded"
    | .error _ => pure ()

-- ---------------------------------------------------------------------------
-- readManifestRev tests
-- ---------------------------------------------------------------------------

/-- A minimal lake-manifest.json with a single mathlib entry. -/
private def sampleManifest : String :=
  String.intercalate "\n" [
    "{\"version\": \"1.0.0\",",
    " \"packagesDir\": \".lake/packages\",",
    " \"packages\":",
    " [{\"url\": \"https://github.com/leanprover-community/mathlib4\",",
    "   \"type\": \"git\",",
    "   \"subDir\": null,",
    "   \"scope\": \"leanprover-community\",",
    "   \"rev\": \"22a0afa903bcf65285152eea298a3d319badc78d\",",
    "   \"name\": \"mathlib\",",
    "   \"inputRev\": \"master\",",
    "   \"inherited\": false,",
    "   \"configFile\": \"lakefile.toml\"}],",
    " \"name\": \"demo\",",
    " \"lakeDir\": \".lake\"}",
    ""
  ]

/-- Scenario: `readManifestRev` returns the `rev` SHA for a package listed in the manifest. -/
private def «readManifestRev returns the rev SHA for a listed package» : IO Unit := do
  withTempDir "hopscotch-manifest-rev" fun dir => do
    IO.FS.writeFile (dir / "lake-manifest.json") sampleManifest
    let result ← LakefileProcessor.readManifestRev dir "mathlib"
    assertEq (some "22a0afa903bcf65285152eea298a3d319badc78d") result
      "readManifestRev should return the rev SHA from the manifest"

/-- Scenario: `readManifestRev` returns `none` when the manifest does not list the package. -/
private def «readManifestRev returns none for an absent package» : IO Unit := do
  withTempDir "hopscotch-manifest-absent" fun dir => do
    IO.FS.writeFile (dir / "lake-manifest.json") sampleManifest
    let result ← LakefileProcessor.readManifestRev dir "batteries"
    assertEq (none : Option String) result
      "readManifestRev should return none when the package is absent"

/-- Scenario: `readManifestRev` returns `none` when no manifest file exists. -/
private def «readManifestRev returns none when no manifest file exists» : IO Unit := do
  withTempDir "hopscotch-manifest-missing" fun dir => do
    let result ← LakefileProcessor.readManifestRev dir "mathlib"
    assertEq (none : Option String) result
      "readManifestRev should return none when lake-manifest.json is absent"

/-- Scenario: `readManifestRev` strips the scope prefix when looking up a scoped dep name. -/
private def «readManifestRev matches bare name for a scoped dep» : IO Unit := do
  withTempDir "hopscotch-manifest-scoped" fun dir => do
    IO.FS.writeFile (dir / "lake-manifest.json") sampleManifest
    -- The dep name used in the lakefile may be "leanprover-community/mathlib4"; the
    -- manifest stores just "mathlib", so readManifestRev must strip the scope prefix.
    let result ← LakefileProcessor.readManifestRev dir "leanprover-community/mathlib"
    assertEq (some "22a0afa903bcf65285152eea298a3d319badc78d") result
      "readManifestRev should strip the scope prefix and match the bare package name"

def suite : TestSuite := #[
  test_case «readGitUrl returns the git field from a matching block»,
  test_case «readGitUrl returns none when the dependency block is absent»,
  test_case «readScope returns none when the scope field is absent from the block»,
  test_case «readPinnedRev returns the rev field from a matching block»,
  test_case «readPinnedRev returns none when no rev field exists»,
  test_case «read path silently uses the first block when names are duplicated»,
  test_case «readManifestRev returns the rev SHA for a listed package»,
  test_case «readManifestRev returns none for an absent package»,
  test_case «readManifestRev returns none when no manifest file exists»,
  test_case «readManifestRev matches bare name for a scoped dep»
]

end HopscotchTestLib.LakefileReadTests
