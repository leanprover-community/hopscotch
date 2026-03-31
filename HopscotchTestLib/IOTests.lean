import HopscotchTestLib.TestUtil
import HopscotchTestLib.MockLake

open Hopscotch
open Hopscotch.State

namespace HopscotchTestLib.IOTests

/-- Discard runner progress output so test logs stay focused on assertions. -/
private def ignoreOutput (_ : String) : IO Unit :=
  pure ()

/-- Interrupt the runner when the nth `lake build` stage is about to start.
    The match uses the `step.label` string emitted by `runProbe` ("Running lake build");
    this couples the helper to that wording, which is stable for the default strategy. -/
private def interruptOnNthBuildStart (target : Nat) : IO (String → IO Unit) := do
  let seen ← IO.mkRef 0
  pure fun line => do
    if line.contains "] Running lake build" then
      let current ← seen.get
      let next := current + 1
      seen.set next
      if next == target then
        throw <| IO.userError "simulated interruption"

/-- Scenario: the default `lake` command is wrapped through `elan` using the downstream toolchain. -/
private def «downstream toolchain command resolution» : IO Unit := do
  withTempDir "hopscotch-toolchain" fun dir => do
    -- Prepare a downstream project with a non-default Lean toolchain.
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "lean-toolchain") "leanprover/lean4:v4.29.0-rc3\n"
    -- Act: resolve the default `lake build` command through the downstream toolchain.
    let (cmd, args) ← Runner.buildCommand "lake" projectDir #["build"]
    -- Assert the default command is rewritten to the expected `elan run ... lake` form.
    assertEq "elan" cmd "default lake command should be resolved through elan"
    assertEq #["run", "leanprover/lean4:v4.29.0-rc3", "lake", "build"] args
      "elan invocation should use the downstream lean-toolchain"
    -- Act: resolve a custom lake command override unchanged.
    let (customCmd, customArgs) ← Runner.buildCommand "/tmp/mock-lake" projectDir #["update"]
    -- Assert the explicit lake command override is preserved verbatim.
    assertEq "/tmp/mock-lake" customCmd
      "explicit lake command overrides should be preserved"
    assertEq #["update"] customArgs
      "explicit lake command overrides should keep their original args"

/-- Scenario: the runner stops on the first build failure and leaves that rev pinned. -/
private def «stop at first build failure» : IO Unit := do
  withTempDir "hopscotch-fail-build" fun dir => do
    -- Prepare a downstream project and commit list that fail during `lake build`.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    -- Act: run the stepping session until the first failure is reached.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the runner records the failing build state and leaves the failing rev pinned.
    assertEq 1 result.exitCode "runner should stop at the first failing build"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.failed) state.status "state should record failure"
    assertEq (some "badbuild") state.currentCommit "state should record the failing commit"
    assertEq (some RunStage.build) state.stage "state should record the failing stage"
    let lakefile := (← IO.FS.readFile (projectDir / "lakefile.toml"))
    assertTrue (lakefile.contains "rev = \"badbuild\"")
      "lakefile should stay pinned to the failing commit"
    -- Assert no later commits were attempted after the failing build.
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:good1", "build:good1", "update:badbuild", "build:badbuild"] calls
      "runner should stop immediately after the first failure"

/-- Scenario: a bump failure stops the loop before any verify steps run for that commit. -/
private def «stop at first update failure» : IO Unit := do
  withTempDir "hopscotch-fail-update" fun dir => do
    -- Prepare a downstream project and commit list that fail during `lake update`.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadupdate\ngood2\n"
    configureMockLake projectDir "fail-update"
    -- Act: run the stepping session until the bump failure is reached.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the failure is recorded at the bump stage and no build runs afterward.
    assertEq 1 result.exitCode "runner should stop at the first failing bump"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (some RunStage.bump) state.stage
      "bump failures should record the bump stage"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:good1", "build:good1", "update:badupdate"] calls
      "build should not run after a bump failure"

/-- Scenario: restarting after a failure retries that same commit before advancing. -/
private def «resume from failed commit and complete» : IO Unit := do
  withTempDir "hopscotch-resume" fun dir => do
    -- Prepare a downstream project with a resumable failing build.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    -- Act: run the first session until it fails on the target commit.
    let firstResult ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the first run stops at the expected failing commit.
    assertEq 1 firstResult.exitCode "first run should fail"
    -- Prepare the resumed run by clearing the helper log and switching the mock lake to success.
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"
    -- Act: resume the stepping session from the previously failing commit.
    let secondResult ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the resumed run completes and retries the failed commit before advancing.
    assertEq 0 secondResult.exitCode "second run should resume and finish"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.completed) state.status "state should record completion after resume"
    assertEq (some "good2") state.lastSuccessfulCommit
      "resume should continue from the previously failing commit"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:badbuild", "build:badbuild", "update:good2", "build:good2"] calls
      "resume should retry the failed commit first"

/-- Scenario: resuming with a different commit list is rejected with a reset hint. -/
private def «reject changed commit list on resume» : IO Unit := do
  withTempDir "hopscotch-commit-change" fun dir => do
    -- Prepare persisted state from a failing run and then mutate the commit list.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    configureMockLake projectDir "fail-build"
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    -- Act: create the initial failing state for the original commit list.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Prepare a changed commit list for the resume attempt.
    IO.FS.writeFile commitListPath "good1\nother\n"
    -- Act: resume with the changed commit list and assert the reset hint is returned.
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        quiet := true
      } ignoreOutput
      fail "changing the commit list should be rejected"
    catch error =>
      assertTrue (error.toString.contains "delete .lake/hopscotch/ to start over")
        "changed commit list should produce the reset instruction"

/-- Scenario: an all-success run completes and leaves the final successful rev pinned. -/
private def «complete all-success run» : IO Unit := do
  withTempDir "hopscotch-success" fun dir => do
    -- Prepare a downstream project and an all-success commit list.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\ngood2\n"
    configureMockLake projectDir "success"
    -- Act: run the stepping session across only successful commits.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the successful run completes and leaves the final rev pinned.
    assertEq 0 result.exitCode "all-success runs should succeed"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.completed) state.status "state should record completion"
    assertEq (some "good2") state.lastSuccessfulCommit
      "last successful commit should be the final commit"
    let lakefile := (← IO.FS.readFile (projectDir / "lakefile.toml"))
    assertTrue (lakefile.contains "rev = \"good2\"")
      "lakefile should stay pinned to the final successful commit"

/-- Scenario: `lake update` targets only the configured dependency. -/
private def «update targets dependency only» : IO Unit := do
  withTempDir "hopscotch-update-target" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\n"
    configureMockLake projectDir "success"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "single successful probes should complete"
    let deps := (← IO.FS.readFile (mockLakeDepsPath projectDir)).trimAscii.toString
    assertContains "batteries" deps
      "lake update should target the configured dependency name"

/-- Scenario: resuming with a different dependency name is rejected with a reset hint. -/
private def «reject changed dependency on resume» : IO Unit := do
  withTempDir "hopscotch-dependency-change" fun dir => do
    -- Prepare persisted state from a failing run with one dependency name.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    configureMockLake projectDir "fail-build"
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    -- Act: create the initial failing state for the original dependency name.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Act: resume with a different dependency name and assert the reset hint is returned.
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "mathlib" (← mockLakeCommand)
        quiet := true
      } ignoreOutput
      fail "changing the dependency name should be rejected"
    catch error =>
      assertTrue (error.toString.contains "delete .lake/hopscotch/ to start over")
        "changed dependency should produce the reset instruction"

/-- Scenario: bisect mode probes only the expected commits and reports the exact boundary failure. -/
private def «bisect probes expected commits» : IO Unit := do
  withTempDir "hopscotch-bisect-build" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath
      "good0\ngood1\ngood2\ngood3\nbadbuild4\nbadbuild5\nbadbuild6\n"
    configureMockLake projectDir "fail-build"

    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } ignoreOutput

    assertEq 1 result.exitCode "bisect should exit with failure after resolving the culprit"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.failed) state.status "bisect should finish with a failed boundary result"
    assertEq (some "badbuild4") state.currentCommit "bisect should resolve the exact failing commit"
    assertEq (some "good3") state.lastSuccessfulCommit
      "bisect should retain the previous known-good commit"
    assertEq (some RunStage.build) state.stage
      "bisect build failures should be reported as build-stage failures"

    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq
      ["update:badbuild6", "build:badbuild6", "update:good3", "build:good3", "update:badbuild4", "build:badbuild4"]
      calls
      "bisect should probe only the endpoint and required midpoints"

    let summary := (← IO.FS.readFile result.summaryPath)
    assertContains "First failing commit: badbuild4" summary
      "final bisect summary should name the culprit"
    assertContains "Previous known good commit: good3" summary
      "final bisect summary should name the previous known-good commit"

/-- Scenario: bisect restores the original toolchain before each fresh probe. -/
private def «bisect resets toolchain between probes» : IO Unit := do
  withTempDir "hopscotch-bisect-toolchain-reset" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath
      "good0\ngood1\ngood2\nmutatetoolchain3\ngood4\nbadbuild5\nbadbuild6\n"
    configureMockLake projectDir "fail-build-and-mutate-toolchain"

    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } ignoreOutput

    assertEq 1 result.exitCode "bisect should still resolve the culprit in the presence of toolchain bumps"
    let toolchainCalls := (← IO.FS.readFile (mockLakeToolchainsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq
      [
        "update:badbuild6:leanprover/lean4:v4.28.0",
        "build:badbuild6:leanprover/lean4:v4.28.0",
        "update:mutatetoolchain3:leanprover/lean4:v4.28.0",
        "build:mutatetoolchain3:leanprover/lean4:v9.9.9",
        "update:good4:leanprover/lean4:v4.28.0",
        "build:good4:leanprover/lean4:v4.28.0",
        "update:badbuild5:leanprover/lean4:v4.28.0",
        "build:badbuild5:leanprover/lean4:v4.28.0"
      ]
      toolchainCalls
      "bisect should restore the original toolchain before every fresh probe while preserving intra-probe updates"

/-- Scenario: bisect treats bump failures as bad probe results. -/
private def «bisect treats update failures as bad» : IO Unit := do
  withTempDir "hopscotch-bisect-update" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good0\ngood1\ngood2\nbadupdate3\nbadupdate4\n"
    configureMockLake projectDir "fail-update"

    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } ignoreOutput

    assertEq 1 result.exitCode "bisect should treat bump failures as bad results"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (some "badupdate3") state.currentCommit
      "bisect should resolve the first failing bump commit at the boundary"
    assertEq (some RunStage.bump) state.stage
      "bump failures should be attributed to the bump stage"

    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:badupdate4", "update:good2", "build:good2", "update:badupdate3"] calls
      "build should not run after a bump failure"

/-- Scenario: interrupted bisect probes resume at the same commit and reuse cached results. -/
private def «bisect resumes interrupted midpoint probe» : IO Unit := do
  withTempDir "hopscotch-bisect-resume" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good0\ngood1\ngood2\ngood3\nbadbuild4\n"
    configureMockLake projectDir "fail-build"
    let interruptOutput ← interruptOnNthBuildStart 2

    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        runMode := .bisect
        quiet := true
      } interruptOutput
      fail "the simulated interruption should abort the first bisect run"
    catch error =>
      assertContains "simulated interruption" error.toString
        "the first run should stop at the injected interruption"

    let interruptedState ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.running) interruptedState.status "interrupted bisect runs should remain resumable"
    assertEq (some "good2") interruptedState.currentCommit
      "the interrupted midpoint should stay pinned in the persisted state"
    assertEq (some RunStage.build) interruptedState.stage
      "the interrupted midpoint should persist the in-flight build stage"

    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } ignoreOutput

    assertEq 1 result.exitCode "resumed bisect runs should still resolve the culprit"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["build:good2", "update:good3", "build:good3"] calls
      "resumed bisect runs should reuse cached probes and restart the interrupted midpoint at the saved stage"

/-- Scenario: bisect aborts if the supplied bad endpoint no longer fails on re-verification. -/
private def «bisect rejects successful bad endpoint» : IO Unit := do
  withTempDir "hopscotch-bisect-endpoint" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good0\ngood1\ngood2\nbadbuild3\n"
    configureMockLake projectDir "success"

    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        runMode := .bisect
        quiet := true
      } ignoreOutput
      fail "bisect should reject a last commit that no longer fails"
    catch error =>
      assertContains "requires a known failing endpoint" error.toString
        "bisect should raise a precondition error when the bad endpoint passes"

/-- Scenario: switching between linear and bisect on one state directory is rejected. -/
private def «reject mode switch on resume» : IO Unit := do
  withTempDir "hopscotch-mode-switch" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good0\nbadbuild1\n"
    configureMockLake projectDir "fail-build"

    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput

    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        runMode := .bisect
        quiet := true
      } ignoreOutput
      fail "switching modes on one state directory should be rejected"
    catch error =>
      assertContains "run mode changed since the last run" error.toString
        "switching between linear and bisect should require resetting persisted state"

/-- Scenario: bisect resumes reject a changed commit list with the usual reset hint. -/
private def «reject changed bisect commit list on resume» : IO Unit := do
  withTempDir "hopscotch-bisect-commit-change" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good0\ngood1\nbadbuild2\n"
    configureMockLake projectDir "fail-build"

    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } ignoreOutput

    IO.FS.writeFile commitListPath "good0\ngood1\notherbad2\n"
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        runMode := .bisect
        quiet := true
      } ignoreOutput
      fail "changing the bisect commit list should be rejected"
    catch error =>
      assertContains "delete .lake/hopscotch/ to start over" error.toString
        "changed bisect commit lists should produce the reset instruction"

/-- Scenario: bisect with --allow-dirty-workspace is accepted and resolves normally.
    The git check in bisect is bypassed when the flag is set, so the run should
    reach the failure boundary rather than erroring on the flag combination. -/
private def «bisect allow dirty workspace is accepted» : IO Unit := do
  withTempDir "hopscotch-bisect-dirty-flag" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good0\nbadbuild1\n"
    configureMockLake projectDir "fail-build"

    -- bisect skips its session-start git check
    -- when --allow-dirty-workspace is set and proceeds to resolve the boundary.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
      allowDirtyWorkspace := true
    } ignoreOutput
    assertEq 1 result.exitCode
      "bisect with --allow-dirty-workspace should resolve the boundary, not error on the flag combination"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.failed) state.status "bisect should record the resolved boundary"
    assertEq (some "badbuild1") state.currentCommit "bisect should identify the failing commit"

/-- Scenario: re-running after a completed session returns 0 immediately without invoking
    the build tool again, making completed runs safely idempotent. -/
private def «completed session returns immediately without re-executing» : IO Unit := do
  withTempDir "hopscotch-completed-rerun" fun dir => do
    -- Prepare: run to completion.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\ngood2\n"
    configureMockLake projectDir "success"
    let firstResult ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    assertEq 0 firstResult.exitCode "first run should complete"
    -- Prepare: clear the call log before the second run.
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    -- Act: re-run the same configuration against the completed state.
    let secondResult ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert: second run exits 0 immediately with no new build or update invocations.
    assertEq 0 secondResult.exitCode "re-running a completed session should return 0"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy
    assertEq "" calls
      "re-running a completed session should not invoke lake again"

/-- Scenario: running against a project without any lakefile is rejected immediately
    with a clear error message. -/
private def «missing lakefile is rejected with a clear error» : IO Unit := do
  withTempDir "hopscotch-no-lakefile" fun dir => do
    -- Prepare: a project directory with a toolchain file but no lakefile.toml.
    let projectDir := dir / "downstream"
    IO.FS.createDirAll projectDir
    IO.FS.writeFile (projectDir / "lean-toolchain") "leanprover/lean4:v4.28.0\n"
    let commitListPath := dir / "commits.txt"
    IO.FS.writeFile commitListPath "good1\n"
    -- Act: attempt to run against the incomplete project.
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" "lake"
        quiet := true
      } ignoreOutput
      fail "missing lakefile should be rejected before any build runs"
    catch error =>
      assertContains "missing lakefile.toml or lakefile.lean" error.toString
        "error should identify the missing lakefile"

/-- Scenario: resuming with a `state.json` from an older schema version is rejected with a
    clear message explaining how to reset. -/
private def «resume with stale schema version is rejected» : IO Unit := do
  withTempDir "hopscotch-stale-schema" fun dir => do
    -- Prepare: a downstream project and commit list.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    -- Write a stale state.json directly via State.save so the test does not depend on the
    -- exact JSON serialization format used by Json.pretty.
    let paths ← State.mkPaths projectDir
    let staleState : PersistedState := {
      schemaVersion := State.currentSchemaVersion - 1
      projectDir := paths.projectDir
      strategyName := "batteries"
      items := #["good1", "badbuild"]
      runMode := .linear
      nextIndex := 0
      currentCommit := some "good1"
      lastSuccessfulCommit := none
      status := .running
      stage := none
      lastLogPath := none
      updatedAt := "2026-01-01T00:00:00Z"
    }
    State.save paths staleState
    -- Act: attempt to resume with the stale schema state.
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        quiet := true
      } ignoreOutput
      fail "stale schema version should be rejected"
    catch error =>
      assertContains "incompatible" error.toString
        "stale schema rejection should mention incompatibility"
      assertContains "delete .lake/hopscotch/ to start over" error.toString
        "stale schema rejection should give the reset instruction"

/-- Scenario: range mode rejects a `gitUrl` that is not a GitHub URL, because only
    github.com repositories are supported by the commit-fetching API. -/
private def «range mode rejects a non-GitHub git URL» : IO Unit := do
  withTempDir "hopscotch-range-nongithub" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    -- Act: supply a GitLab URL as the git URL for range mode.
    try
      let _ ← Runner.run {
        itemSource := .range (some "abc123") none (some "https://gitlab.com/owner/repo")
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" "lake"
        quiet := true
      } ignoreOutput
      fail "non-GitHub git URL should be rejected before any API call"
    catch error =>
      assertContains "not a recognized GitHub URL" error.toString
        "range mode should explain that only github.com repositories are supported"

/-- Scenario: running with `quiet := false` exercises the stdout-mirroring code path in
    `emitLine` and does not suppress the runner's progress messages to the output callback. -/
private def «quiet := false does not suppress runner progress messages» : IO Unit := do
  withTempDir "hopscotch-quiet-false" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\n"
    configureMockLake projectDir "success"
    let captured ← IO.mkRef ([] : List String)
    let captureOutput (line : String) : IO Unit := captured.modify (· ++ [line])
    -- Act: run with quiet := false (lake subprocess output is also mirrored to stdout).
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := false
    } captureOutput
    -- Assert: run completes and the output callback receives progress messages as normal.
    assertEq 0 result.exitCode "quiet=false run should complete successfully"
    let lines ← captured.get
    assertTrue (lines.any fun line => line.contains "Attempting commit good1")
      "quiet=false should not suppress the runner's attempt messages"
    -- The calls log confirms the build ran.
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii
    assertTrue (calls.contains "build:good1")
      "the build should run regardless of quiet setting"

def suite : TestSuite := #[
  test_case «downstream toolchain command resolution»,
  test_case «stop at first build failure»,
  test_case «stop at first update failure»,
  test_case «resume from failed commit and complete»,
  test_case «reject changed commit list on resume»,
  test_case «complete all-success run»,
  test_case «update targets dependency only»,
  test_case «reject changed dependency on resume»,
  test_case «bisect probes expected commits»,
  test_case «bisect resets toolchain between probes»,
  test_case «bisect treats update failures as bad»,
  test_case «bisect resumes interrupted midpoint probe»,
  test_case «bisect rejects successful bad endpoint»,
  test_case «reject mode switch on resume»,
  test_case «reject changed bisect commit list on resume»,
  test_case «bisect allow dirty workspace is accepted»,
  test_case «completed session returns immediately without re-executing»,
  test_case «missing lakefile is rejected with a clear error»,
  test_case «resume with stale schema version is rejected»,
  test_case «range mode rejects a non-GitHub git URL»,
  test_case «quiet := false does not suppress runner progress messages»
]

end HopscotchTestLib.IOTests
