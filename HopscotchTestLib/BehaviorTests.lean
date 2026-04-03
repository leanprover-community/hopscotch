import HopscotchTestLib.TestUtil

open Hopscotch
open Hopscotch.State

namespace HopscotchTestLib.BehaviorTests

private def makeMultiline (strings : List String) : String :=
  String.intercalate "\n" strings

/-- Execute the pure bisect bound updates against a fixed per-index outcome table. -/
private def resolveBisectBoundary (outcomes : Array ProbeOutcome) : Nat := Id.run do
  let mut bounds : Runner.BisectBounds := {
    knownGoodIndex := 0
    knownBadIndex := outcomes.size - 1
  }
  -- Mirror runBisect's initial step: confirm the bad endpoint before probing midpoints.
  bounds := Runner.advanceBisectBounds bounds bounds.knownBadIndex outcomes[bounds.knownBadIndex]!
  repeat
    match Runner.nextBisectProbeIndex? bounds with
    | none => break
    | some probeIndex =>
        bounds := Runner.advanceBisectBounds bounds probeIndex outcomes[probeIndex]!
  bounds.knownBadIndex

/-- Scenario: commit-list parsing ignores comments and rejects an empty normalized file. -/
private def «commit list normalization» : IO Unit := do
  -- Prepare: commit-list contents with blanks and comments.
  let contents := makeMultiline [
    "",
    "  # ignore me",
    "a1",
    "",
    "  b2  ",
    "# another comment",
    "c3"
  ]
  -- Act: normalize the commit-list contents
  -- Assert: the expected commits remain.
  assertEq #["a1", "b2", "c3"] (ItemList.normalizeLines contents)
    "commit normalization should ignore blanks and comments"
  withTempDir "hopscotch-commits" fun dir => do
    -- Prepare: an otherwise empty commit-list file.
    let path := dir / "commits.txt"
    IO.FS.writeFile path "# only comments\n\n"
    -- Act: load the item-list file
    -- Assert: the empty normalized file is rejected.
    try
      let _ ← ItemList.load path
      fail "expected empty commit file to fail"
    catch _ =>
      pure ()

/-- Scenario: console color helpers only add ANSI escapes when enabled. -/
private def «console color helper behavior» : IO Unit := do
  -- Act: evaluate the color helper cases
  -- Assert: each mode behaves as expected.
  assertEq ColorMode.enabled (decideColorMode none true)
    "tty output without NO_COLOR should enable color"
  assertEq ColorMode.disabled (decideColorMode (some "1") true)
    "NO_COLOR should disable color"
  assertEq ColorMode.disabled (decideColorMode none false)
    "non-tty output should disable color"
  assertEq "plain" (colorize .disabled .success "plain")
    "disabled color should leave text unchanged"
  -- Prepare: a colorized failure string for escape-sequence assertions.
  let colored := colorize .enabled .failure "boom"
  -- Assert: the enabled colorized string contains the expected ANSI wrappers.
  assertTrue (colored.startsWith (String.singleton (Char.ofNat 27) ++ "["))
    "enabled color should add ANSI prefix"
  assertTrue (colored.endsWith (String.singleton (Char.ofNat 27) ++ "[0m"))
    "enabled color should add ANSI reset"

/-- Scenario: terminal summary formatting colors only the displayed summary, not the stored text. -/
private def «summary display formatting» : IO Unit := do
  -- Prepare: a failed persisted state for summary rendering.
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyName := "mathlib"
    items := #["good1", "bad2"]
    runMode := .linear
    nextIndex := 1
    currentCommit := some "bad2"
    lastSuccessfulCommit := some "good1"
    status := .failed
    stage := some RunStage.build
    lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/1-bad2-build.log"
    updatedAt := "2026-03-17T00:00:00Z"
  }
  -- Act: render the plain summary text for the failed state.
  let plain := Runner.summaryText state
  let expectedPlain := makeMultiline [
    "# Summary",
    "",
    "Status: failed",
    "Mode: linear",
    "First failing commit: bad2",
    "Failure stage: lake build",
    "Log file: /tmp/demo/.lake/hopscotch/logs/1-bad2-build.log",
    ""
  ]
  -- Assert: the plain and terminal-rendered summaries preserve the expected content.
  assertEq expectedPlain plain "plain summary text should stay unchanged"
  let colored := Runner.renderSummaryForTerminal .enabled plain
  assertTrue (colored.contains (String.singleton (Char.ofNat 27) ++ "["))
    "display summary should add ANSI escapes when enabled"
  let uncolored := Runner.renderSummaryForTerminal .disabled plain
  assertEq plain uncolored "display summary should remain plain when color is disabled"

/-- Scenario: the git cleanliness failure stage renders a dedicated summary label. -/
private def «summary git check formatting» : IO Unit := do
  -- Prepare: a failed persisted state at the git-cleanliness stage.
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyName := "mathlib"
    items := #["good1", "bad2"]
    runMode := .linear
    nextIndex := 1
    currentCommit := some "bad2"
    lastSuccessfulCommit := some "good1"
    status := .failed
    stage := some RunStage.gitCheck
    lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/1-bad2-build.log"
    updatedAt := "2026-03-17T00:00:00Z"
  }
  -- Act: render the plain summary text for the git-cleanliness failure.
  let plain := Runner.summaryText state
  -- Assert: the rendered summary names the git-cleanliness stage explicitly.
  assertContains "Failure stage: git cleanliness check" plain
    "git-cleanliness failures should render a dedicated summary label"

/-- Scenario: bisect midpoint selection and bound updates resolve the monotonic boundary. -/
private def «bisect monotonic boundary resolution» : IO Unit := do
  let culprit := resolveBisectBoundary #[
    .success,
    .success,
    .failure,
    .failure,
    .failure
  ]
  assertEq 2 culprit "bisect should resolve the first failing commit in a monotonic history"

/-- Scenario: bisect resets the known-good bound after a successful midpoint probe. -/
private def «bisect reset on success» : IO Unit := do
  let culprit := resolveBisectBoundary #[
    .success,
    .failure,
    .success,
    .success,
    .failure
  ]
  assertEq 4 culprit
    "bisect should target the boundary after the latest observed success, not an isolated earlier failure"

/-- Scenario: bisect still resolves correctly when an earlier failing midpoint appears first. -/
private def «bisect mixed midpoint order» : IO Unit := do
  let culprit := resolveBisectBoundary #[
    .success,
    .success,
    .failure,
    .success,
    .failure
  ]
  assertEq 2 culprit "bisect should keep narrowing to the boundary failure after the latest success"

/-- Scenario: bisect summary formatting includes the known-good and known-bad search context. -/
private def «bisect summary formatting» : IO Unit := do
  let bisect : BisectState := {
    knownGoodIndex := 1
    knownBadIndex := 4
    verifiedBad := true
    baselineToolchain := "leanprover/lean4:v4.28.0\n"
    probeResults := #[
      {
        index := 4
        commit := "bad4"
        outcome := .failure
        stage := some .build
        logPath := some "/tmp/demo/.lake/hopscotch/logs/4-bad4-build.log"
      },
      {
        index := 3
        commit := "mid3"
        outcome := .success
      }
    ]
  }
  let runningState : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyName := "mathlib"
    items := #["good0", "good1", "mid2", "mid3", "bad4"]
    runMode := .bisect
    bisect := some bisect
    nextIndex := 2
    currentCommit := some "mid2"
    lastSuccessfulCommit := some "good1"
    status := .running
    stage := none
    lastLogPath := none
    updatedAt := "2026-03-20T00:00:00Z"
  }
  let failedState : PersistedState := {
    runningState with
      nextIndex := 4
      currentCommit := some "bad4"
      status := .failed
      stage := some .build
      lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/4-bad4-build.log"
      bisect := some { bisect with knownGoodIndex := 3 }
      lastSuccessfulCommit := some "mid3"
  }
  let runningPlain := Runner.summaryText runningState
  let failedPlain := Runner.summaryText failedState
  assertContains "Mode: bisect" runningPlain "running bisect summaries should name the mode"
  assertContains "Known good commit: good1" runningPlain
    "running bisect summaries should show the current known-good commit"
  assertContains "Known bad commit: bad4" runningPlain
    "running bisect summaries should show the current known-bad commit"
  assertContains "Next probe commit: mid2" runningPlain
    "running bisect summaries should show the next probe commit"
  assertContains "Previous known good commit: mid3" failedPlain
    "final bisect summaries should name the previous known-good commit"
  assertContains "Failure stage: lake build" failedPlain
    "final bisect summaries should retain the failing stage"
  assertContains "Log file: /tmp/demo/.lake/hopscotch/logs/4-bad4-build.log" failedPlain
    "final bisect summaries should include the culprit log path"

/-- Scenario: `parseRepoId` handles all supported GitHub URL forms. -/
private def «GitHub URL parsing» : IO Unit := do
  -- HTTPS without .git
  assertEq (some ("owner", "repo"))
    (GitHub.parseRepoId "https://github.com/owner/repo")
    "HTTPS URL without .git should parse correctly"
  -- HTTPS with .git
  assertEq (some ("owner", "repo"))
    (GitHub.parseRepoId "https://github.com/owner/repo.git")
    "HTTPS URL with .git should strip the suffix"
  -- SSH without .git
  assertEq (some ("owner", "repo"))
    (GitHub.parseRepoId "git@github.com:owner/repo")
    "SSH URL without .git should parse correctly"
  -- SSH with .git
  assertEq (some ("owner", "repo"))
    (GitHub.parseRepoId "git@github.com:owner/repo.git")
    "SSH URL with .git should strip the suffix"
  -- Non-GitHub URL
  assertEq (none : Option (String × String))
    (GitHub.parseRepoId "https://gitlab.com/owner/repo")
    "non-GitHub URLs should return none"
  -- HTTPS URL with extra path segments
  assertEq (none : Option (String × String))
    (GitHub.parseRepoId "https://github.com/owner/repo/extra")
    "HTTPS URL with extra path segments should return none"
  -- SSH URL with extra path segments
  assertEq (none : Option (String × String))
    (GitHub.parseRepoId "git@github.com:owner/repo/extra")
    "SSH URL with extra path segments should return none"
  -- Empty string
  assertEq (none : Option (String × String))
    (GitHub.parseRepoId "")
    "empty string should return none"

/-- Scenario: `warnIfNoToken` emits a warning iff the token is absent. -/
private def «GitHub token warning» : IO Unit := do
  -- No warning when token is present.
  let warnings1 ← IO.mkRef ([] : List String)
  GitHub.warnIfNoToken (some "tok") (fun s => do warnings1.modify (· ++ [s]))
  let captured1 ← warnings1.get
  assertEq ([] : List String) captured1
    "no warning should be emitted when a token is present"
  -- Warning emitted when token is absent.
  let warnings2 ← IO.mkRef ([] : List String)
  GitHub.warnIfNoToken none (fun s => do warnings2.modify (· ++ [s]))
  let captured2 ← warnings2.get
  assertTrue (captured2.length > 0)
    "at least one warning should be emitted when the token is absent"
  assertTrue (captured2.any (·.contains "GITHUB_TOKEN"))
    "the warning should mention GITHUB_TOKEN"

/-- Scenario: CRLF line endings in a commit list file are normalized the same as LF. -/
private def «CRLF line endings in item list are normalized» : IO Unit := do
  let crlfContents := "a1\r\nb2\r\n\r\n# comment\r\nc3\r\n"
  assertEq #["a1", "b2", "c3"] (ItemList.normalizeLines crlfContents)
    "CRLF-terminated commit lists should produce the same items as LF-terminated lists"

/-- Scenario: CR-only line endings in a commit list file are normalized the same as LF. -/
private def «CR-only line endings in item list are normalized» : IO Unit := do
  let crContents := "a1\rb2\r\r# comment\rc3\r"
  assertEq #["a1", "b2", "c3"] (ItemList.normalizeLines crContents)
    "CR-only commit lists should produce the same items as LF-terminated lists"

/-- Scenario: GitHub API error detection requires *both* `message` and `documentation_url`.
    An object with only `message` is not treated as an error — normal API responses can
    contain a `message` field, and misidentifying them would silently drop valid data. -/
private def «GitHub API error detection requires both message and documentation_url» : IO Unit := do
  -- A JSON object with BOTH fields IS recognized as a GitHub API error.
  let bothFields ← IO.ofExcept <| Lean.Json.parse
    "{\"message\":\"Not Found\",\"documentation_url\":\"https://docs.github.com/\"}"
  assertTrue (GitHub.parseApiError? bothFields).isSome
    "an object with both message and documentation_url should be parsed as a GitHub API error"
  -- A JSON object with ONLY message is NOT a GitHub API error.
  let messageOnly ← IO.ofExcept <| Lean.Json.parse "{\"message\":\"some normal response\"}"
  assertTrue (GitHub.parseApiError? messageOnly).isNone
    "an object with only message should not be parsed as a GitHub API error"
  -- A non-object value is not a GitHub API error either.
  let notObject ← IO.ofExcept <| Lean.Json.parse "\"just a string\""
  assertTrue (GitHub.parseApiError? notObject).isNone
    "a non-object JSON value should not be parsed as a GitHub API error"

/-- Scenario: `nextBisectProbeIndex?` returns `none` for a 2-element list because the bounds
    are already adjacent, meaning the culprit is known without any midpoint probes. -/
private def «bisect 2-element list is immediately resolved» : IO Unit := do
  let bounds : Runner.BisectBounds := { knownGoodIndex := 0, knownBadIndex := 1 }
  assertEq (none : Option Nat) (Runner.nextBisectProbeIndex? bounds)
    "adjacent bisect bounds should return none — no midpoint probe needed"

def suite : TestSuite := #[
  test_case «commit list normalization»,
  test_case «console color helper behavior»,
  test_case «summary display formatting»,
  test_case «summary git check formatting»,
  test_case «bisect monotonic boundary resolution»,
  test_case «bisect reset on success»,
  test_case «bisect mixed midpoint order»,
  test_case «bisect summary formatting»,
  test_case «GitHub URL parsing»,
  test_case «GitHub token warning»,
  test_case «CRLF line endings in item list are normalized»,
  test_case «CR-only line endings in item list are normalized»,
  test_case «GitHub API error detection requires both message and documentation_url»,
  test_case «bisect 2-element list is immediately resolved»
]

end HopscotchTestLib.BehaviorTests
