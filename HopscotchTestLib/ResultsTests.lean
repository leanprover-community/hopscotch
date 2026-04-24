import HopscotchTestLib.TestUtil

open Hopscotch
open Hopscotch.State
open Hopscotch.Results

namespace HopscotchTestLib.ResultsTests

/-- Build a `Paths` value without touching the filesystem (mirrors `mkPaths`). -/
private def mkPathsUnchecked (projectDir : System.FilePath) : Paths :=
  let stateRoot := projectDir / ".lake" / "hopscotch"
  { projectDir := projectDir
    stateRoot := stateRoot
    statePath := stateRoot / "state.json"
    summaryPath := stateRoot / "summary.md"
    resultsPath := stateRoot / "results.json"
    logsDir := stateRoot / "logs"
    culpritLogsDir := stateRoot / "logs" / "culprit" }

/-- Scenario: a completed linear run produces a status=fullySuccessful results payload with exitCode=0. -/
private def «results.json for completed linear run» : IO Unit := do
  let paths := mkPathsUnchecked "/tmp/demo"
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyScope := "mathlib"
    items := #["good1", "good2"]
    runMode := .linear
    nextIndex := 2
    currentCommit := none
    lastSuccessfulCommit := some "good2"
    status := .fullySuccessful
    stage := none
    lastLogPath := none
    updatedAt := "2026-04-24T00:00:00Z"
  }
  let r := fromState paths state
  assertEq resultsSchemaVersion r.schemaVersion "schema version should be exposed"
  assertEq "fullySuccessful" r.status "fullySuccessful status should serialize"
  assertEq "linear" r.mode "linear mode should serialize"
  assertEq 0 r.exitCode "fullySuccessful runs should have exit code 0"
  assertEq (none : Option String) r.firstFailingCommit
    "fullySuccessful runs should have no firstFailingCommit"
  assertEq (some "good2") r.lastSuccessfulCommit
    "fullySuccessful runs should expose the last successful commit"
  assertEq (none : Option String) r.failureStage
    "fullySuccessful runs should have no failure stage"
  assertEq (none : Option String) r.culpritLogPath
    "fullySuccessful runs should have no culprit log path"
  assertEq (none : Option BisectJson) r.bisect
    "linear runs should have no bisect field"

/-- Scenario: a failed linear run maps currentCommit to firstFailingCommit and builds the culprit path. -/
private def «results.json for failed linear run» : IO Unit := do
  let paths := mkPathsUnchecked "/tmp/demo"
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyScope := "mathlib"
    items := #["good1", "bad2"]
    runMode := .linear
    nextIndex := 1
    currentCommit := some "bad2"
    lastSuccessfulCommit := some "good1"
    status := .stopped
    stage := some RunStage.build
    lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/1-bad2-build.log"
    updatedAt := "2026-04-24T00:00:00Z"
  }
  let r := fromState paths state
  assertEq "stopped" r.status "failed status should serialize"
  assertEq 1 r.exitCode "failed runs should have exit code 1"
  assertEq (some "bad2") r.firstFailingCommit
    "failed runs should expose currentCommit as firstFailingCommit"
  assertEq (some "good1") r.lastSuccessfulCommit
    "failed runs should retain the last successful commit"
  assertEq (some "lake build") r.failureStage
    "build stage should map to the 'lake build' label"
  assertEq (some "/tmp/demo/.lake/hopscotch/logs/culprit/1-bad2-build.log") r.culpritLogPath
    "culprit log path should point into culprit/ with the original log's basename"

/-- Scenario: the gitCheck stage maps to its dedicated label in results.json. -/
private def «results.json git-check failure» : IO Unit := do
  let paths := mkPathsUnchecked "/tmp/demo"
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyScope := "mathlib"
    items := #["good1", "bad2"]
    runMode := .linear
    nextIndex := 1
    currentCommit := some "bad2"
    lastSuccessfulCommit := some "good1"
    status := .stopped
    stage := some RunStage.gitCheck
    lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/1-bad2-build.log"
    updatedAt := "2026-04-24T00:00:00Z"
  }
  let r := fromState paths state
  assertEq (some "git cleanliness check") r.failureStage
    "gitCheck stage should map to 'git cleanliness check'"

/-- Scenario: a resolved bisect run exposes bounds, probe history, and the knownGoodWasProbed flag. -/
private def «results.json for bisect resolved» : IO Unit := do
  let paths := mkPathsUnchecked "/tmp/demo"
  let bisect : BisectState := {
    knownGoodIndex := 3
    knownBadIndex := 4
    verifiedBad := true
    baselineToolchain := "leanprover/lean4:v4.28.0\n"
    probeResults := #[
      {
        index := 4
        commit := "bad4"
        outcome := .failure
        stage := some .build
        logPath := some "/tmp/demo/.lake/hopscotch/logs/0-4-bad4-build.log"
      },
      {
        index := 3
        commit := "mid3"
        outcome := .success
      }
    ]
  }
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyScope := "mathlib"
    items := #["good0", "good1", "mid2", "mid3", "bad4"]
    runMode := .bisect
    bisect := some bisect
    nextIndex := 4
    currentCommit := some "bad4"
    lastSuccessfulCommit := some "mid3"
    status := .stopped
    stage := some RunStage.build
    lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/0-4-bad4-build.log"
    updatedAt := "2026-04-24T00:00:00Z"
  }
  let r := fromState paths state
  assertEq "bisect" r.mode "bisect mode should serialize"
  let some b := r.bisect | fail "bisect view should be populated for bisect runs"
  assertEq 3 b.knownGoodIndex "known good index should match"
  assertEq 4 b.knownBadIndex "known bad index should match"
  assertEq (some "mid3") b.knownGoodCommit "known good commit should resolve from items"
  assertEq (some "bad4") b.knownBadCommit "known bad commit should resolve from items"
  assertTrue b.knownGoodWasProbed
    "knownGoodWasProbed should be true when a probeResult at the good index succeeded"
  assertTrue b.verifiedBad "verifiedBad should carry through from the persisted state"
  assertEq 2 b.probeResults.size "probeResults should round-trip all cached results"
  assertEq "failure" b.probeResults[0]!.outcome
    "first probe result should preserve its failure outcome"
  assertEq (some "lake build") b.probeResults[0]!.stage
    "failing probe result should include the mapped stage label"
  assertEq "success" b.probeResults[1]!.outcome
    "second probe result should preserve its success outcome"

/-- Scenario: when the bisect good endpoint was never probed (assumed at index 0), knownGoodWasProbed is false. -/
private def «results.json bisect good endpoint assumed» : IO Unit := do
  let paths := mkPathsUnchecked "/tmp/demo"
  let bisect : BisectState := {
    knownGoodIndex := 0
    knownBadIndex := 1
    verifiedBad := true
    baselineToolchain := "leanprover/lean4:v4.28.0\n"
    probeResults := #[
      {
        index := 1
        commit := "bad1"
        outcome := .failure
        stage := some .build
        logPath := some "/tmp/demo/.lake/hopscotch/logs/0-1-bad1-build.log"
      }
    ]
  }
  let state : PersistedState := {
    schemaVersion := currentSchemaVersion
    projectDir := "/tmp/demo"
    strategyScope := "mathlib"
    items := #["good0", "bad1"]
    runMode := .bisect
    bisect := some bisect
    nextIndex := 1
    currentCommit := some "bad1"
    lastSuccessfulCommit := none
    status := .stopped
    stage := some RunStage.build
    lastLogPath := some "/tmp/demo/.lake/hopscotch/logs/0-1-bad1-build.log"
    updatedAt := "2026-04-24T00:00:00Z"
  }
  let r := fromState paths state
  let some b := r.bisect | fail "bisect view should be populated"
  assertTrue (!b.knownGoodWasProbed)
    "knownGoodWasProbed should be false when the good endpoint was only assumed"

/-- Scenario: writeResults writes both the internal file and the optional extra path with identical content. -/
private def «writeResults writes both paths» : IO Unit := do
  withTempDir "hopscotch-results" fun dir => do
    let paths := mkPathsUnchecked dir
    IO.FS.createDirAll paths.stateRoot
    let extraPath := dir / "out" / "mirror.json"
    let state : PersistedState := {
      schemaVersion := currentSchemaVersion
      projectDir := dir
      strategyScope := "mathlib"
      items := #["c0", "c1"]
      runMode := .linear
      nextIndex := 1
      currentCommit := some "c1"
      lastSuccessfulCommit := some "c0"
      status := .stopped
      stage := some RunStage.build
      lastLogPath := some (paths.logsDir / "1-c1-build.log").toString
      updatedAt := "2026-04-24T00:00:00Z"
    }
    writeResults paths (some extraPath) state
    assertTrue (← paths.resultsPath.pathExists) "internal results.json should be written"
    assertTrue (← extraPath.pathExists) "external mirror should be written"
    let internal ← IO.FS.readFile paths.resultsPath
    let external ← IO.FS.readFile extraPath
    assertEq internal external "internal and external copies should be byte-identical"
    let parsed ← readJsonFile (α := ResultsJson) paths.resultsPath
    assertEq "stopped" parsed.status "round-tripped results should preserve status"
    assertEq (some "c1") parsed.firstFailingCommit
      "round-tripped results should preserve firstFailingCommit"

/-- Scenario: without an extra path, writeResults only writes the internal file. -/
private def «writeResults without extra path» : IO Unit := do
  withTempDir "hopscotch-results-single" fun dir => do
    let paths := mkPathsUnchecked dir
    IO.FS.createDirAll paths.stateRoot
    let state : PersistedState := {
      schemaVersion := currentSchemaVersion
      projectDir := dir
      strategyScope := "mathlib"
      items := #["c0"]
      runMode := .linear
      nextIndex := 1
      currentCommit := none
      lastSuccessfulCommit := some "c0"
      status := .fullySuccessful
      stage := none
      lastLogPath := none
      updatedAt := "2026-04-24T00:00:00Z"
    }
    writeResults paths none state
    assertTrue (← paths.resultsPath.pathExists)
      "internal results.json should be written even without an extra path"

def suite : TestSuite := #[
  test_case «results.json for completed linear run»,
  test_case «results.json for failed linear run»,
  test_case «results.json git-check failure»,
  test_case «results.json for bisect resolved»,
  test_case «results.json bisect good endpoint assumed»,
  test_case «writeResults writes both paths»,
  test_case «writeResults without extra path»
]

end HopscotchTestLib.ResultsTests
