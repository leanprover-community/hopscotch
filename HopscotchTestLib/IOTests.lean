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
    assertEq (.stopped) state.status "state should record failure"
    assertEq (some "badbuild") state.currentCommit "state should record the failing commit"
    assertEq (some RunStage.build) state.stage "state should record the failing stage"
    let lakefile := (← IO.FS.readFile (projectDir / "lakefile.toml"))
    assertTrue (lakefile.contains "rev = \"badbuild\"")
      "lakefile should stay pinned to the failing commit"
    -- Assert no later commits were attempted after the failing build.
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:good1", "build:good1", "update:badbuild", "build:badbuild"] calls
      "runner should stop immediately after the first failure"
    -- Assert that results.json is written alongside state.json with the public schema.
    let resultsPath := projectDir / ".lake" / "hopscotch" / "results.json"
    assertTrue (← resultsPath.pathExists) "results.json should be written on failure"
    let results ← readJsonFile (α := Hopscotch.Results.ResultsJson) resultsPath
    assertEq "stopped" results.status "results.json should record the stopped status"
    assertEq 1 results.exitCode "results.json should record exit code 1"
    assertEq (some "badbuild") results.firstFailingCommit
      "results.json should expose firstFailingCommit"
    assertEq (some "lake build") results.failureStage
      "results.json should expose the build failure stage"

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
    assertEq (.fullySuccessful) state.status "state should record completion after resume"
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
    assertEq (.fullySuccessful) state.status "state should record completion"
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
    assertEq (.stopped) state.status "bisect should finish with a failed boundary result"
    assertEq (some "badbuild4") state.currentCommit "bisect should resolve the exact failing commit"
    assertEq (some "good3") state.lastSuccessfulCommit
      "bisect should retain the previous known-good commit"
    assertEq (some RunStage.build) state.stage
      "bisect build failures should be reported as build-stage failures"

    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq
      ["update:badbuild6", "build:badbuild6", "update:good3", "build:good3", "update:badbuild4", "build:badbuild4",
       "update:badbuild4"]
      calls
      "bisect should probe only the endpoint and required midpoints, then restore the culprit commit"

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
        "build:badbuild5:leanprover/lean4:v4.28.0",
        "update:badbuild5:leanprover/lean4:v4.28.0"
      ]
      toolchainCalls
      "bisect should restore the original toolchain before every fresh probe while preserving intra-probe updates, then restore the culprit commit"

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
    assertEq ["update:badupdate4", "update:good2", "build:good2", "update:badupdate3", "update:badupdate3"] calls
      "build should not run after a bump failure; restore attempt runs after search completes"

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
    assertEq ["build:good2", "update:good3", "build:good3", "update:badbuild4"] calls
      "resumed bisect runs should reuse cached probes and restart the interrupted midpoint at the saved stage, then restore the culprit commit"

/-- Scenario: bisect aborts if the supplied bad endpoint no longer fails on re-verification
    (without --keep-last-good). -/
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

/-- Scenario: bisect with --keep-last-good exits 0 and pins the top commit when the bad
    endpoint passes (all-pass; no culprit). -/
private def «bisect all-pass with keep-last-good» : IO Unit := do
  withTempDir "hopscotch-bisect-allpass" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\ngood2\ngood3\n"
    configureMockLake projectDir "success"
    let result ← Runner.run {
      itemSource   := .file commitListPath
      projectDir   := projectDir
      strategy     := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode      := .bisect
      quiet        := true
      keepLastGood := true
    } ignoreOutput
    assertEq 0 result.exitCode
      "bisect all-pass with --keep-last-good should exit 0"
    let lakefile ← IO.FS.readFile (projectDir / "lakefile.toml")
    assertTrue (lakefile.contains "rev = \"good3\"")
      "lakefile should be pinned to the last (top) commit"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status
      "state should be fullySuccessful"
    assertEq (some "good3") state.lastSuccessfulCommit
      "lastSuccessfulCommit should be the top commit"
    assertContains "no culprit found" result.summary
      "summary should say no culprit was found"

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
    assertEq (.stopped) state.status "bisect should record the resolved boundary"
    assertEq (some "badbuild1") state.currentCommit "bisect should identify the failing commit"

/-- Scenario: re-running after a completed session returns 0 immediately without invoking
    the build tool again, making completed runs safely idempotent. -/
private def «fullySuccessful session returns immediately without re-executing» : IO Unit := do
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
      strategyScope := "batteries"
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

/-- Scenario: bisect default end-state leaves the lakefile pinned to the first bad commit. -/
private def «bisect default end state pins first bad commit» : IO Unit := do
  withTempDir "hopscotch-bisect-end-bad" fun dir => do
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

    assertEq 1 result.exitCode "bisect should report failure"
    let lakefile := (← IO.FS.readFile (projectDir / "lakefile.toml"))
    assertTrue (lakefile.contains "rev = \"badbuild4\"")
      "bisect default end state should leave the lakefile pinned to the first bad commit"

/-- Scenario: bisect with --keep-last-good leaves the lakefile pinned to the last good commit. -/
private def «bisect keep last good pins last good commit» : IO Unit := do
  withTempDir "hopscotch-bisect-end-good" fun dir => do
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
      keepLastGood := true
    } ignoreOutput

    assertEq 1 result.exitCode "bisect should report failure"
    let lakefile := (← IO.FS.readFile (projectDir / "lakefile.toml"))
    assertTrue (lakefile.contains "rev = \"good3\"")
      "bisect with --keep-last-good should leave the lakefile pinned to the last good commit"

/-- Scenario: linear with --keep-last-good leaves the lakefile pinned to the last good commit. -/
private def «linear keep last good pins last good commit» : IO Unit := do
  withTempDir "hopscotch-linear-end-good" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"

    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
      keepLastGood := true
    } ignoreOutput

    assertEq 1 result.exitCode "linear should report failure"
    let lakefile := (← IO.FS.readFile (projectDir / "lakefile.toml"))
    assertTrue (lakefile.contains "rev = \"good1\"")
      "linear with --keep-last-good should leave the lakefile pinned to the last good commit"

/-- Scenario: when verify steps are stripped (the mechanism behind HOPSCOTCH_SKIP_BUILD),
    commits that would fail the build succeed and no `lake build` calls are made. -/
private def «skip build: build step not invoked» : IO Unit := do
  withTempDir "hopscotch-skip-build" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    -- "badbuild" prefix would normally fail the build; its presence here proves the build was skipped.
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    let baseStrategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := { baseStrategy with verify := #[] }
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "runner should succeed when build step is skipped"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "all commits should pass with no verify step"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertTrue (calls.all (·.startsWith "update:"))
      "only lake update calls should appear when the build step is skipped"

/-- Scenario: `GITHUB_TOKEN` is stripped from the environment of every `lake` child process. -/
private def «strip GITHUB_TOKEN from lake child env» : IO Unit := do
  withTempDir "hopscotch-env-scrub" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\ngood2\n"
    configureMockLake projectDir "success"
    -- The `lake test` driver sets `GITHUB_TOKEN` in the test-process env so this
    -- scenario can verify it is scrubbed. If the driver ever stops doing so, both
    -- the protected and unprotected code paths would observe `(unset)` in the
    -- child, silently masking a regression; guard against that drift here.
    match ← IO.getEnv "GITHUB_TOKEN" with
    | none =>
        fail "GITHUB_TOKEN missing from test-process env; the lake test driver \
              should set a sentinel value so this scenario can verify scrubbing"
    | some _ => pure ()
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "all-success run should complete"
    let envLog := (← IO.FS.readFile (mockLakeEnvPath projectDir)).trimAscii.copy
    let observations := (envLog.splitOn "\n").filter (! ·.isEmpty)
    assertTrue (!observations.isEmpty)
      "mock lake should have been invoked at least once; env.log was empty"
    for line in observations do
      assertEq "GITHUB_TOKEN=(unset)" line
        "lake child observed GITHUB_TOKEN in its environment"

/-- Scenario: with `--test` enabled, a commit that builds but fails `lake test` stops the run
    at the test stage and counts as a failed hop. -/
private def «lake test failure counts as a failed hop» : IO Unit := do
  withTempDir "hopscotch-fail-test" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadtest\ngood2\n"
    configureMockLake projectDir "fail-test"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { runTest := true }
      quiet := true
    } ignoreOutput
    assertEq 1 result.exitCode "a failing lake test should stop the run"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.stopped) state.status "state should record failure"
    assertEq (some "badtest") state.currentCommit "state should record the test-failing commit"
    assertEq (some RunStage.test) state.stage
      "state should record the test stage as the failure point"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq
      ["update:good1", "build:good1", "test:good1", "update:badtest", "build:badtest", "test:badtest"]
      calls
      "build should run before test, and the run should stop at the first failing test"
    let resultsPath := projectDir / ".lake" / "hopscotch" / "results.json"
    let results ← readJsonFile (α := Hopscotch.Results.ResultsJson) resultsPath
    assertEq (some "lake test") results.failureStage
      "results.json should expose the test failure stage"

/-- Scenario: with `--lint` enabled, a commit that builds but fails `lake lint` stops the run
    at the lint stage; no test step runs when only `--lint` is set. -/
private def «lake lint failure counts as a failed hop» : IO Unit := do
  withTempDir "hopscotch-fail-lint" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadlint\n"
    configureMockLake projectDir "fail-lint"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { runLint := true }
      quiet := true
    } ignoreOutput
    assertEq 1 result.exitCode "a failing lake lint should stop the run"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (some RunStage.lint) state.stage
      "state should record the lint stage as the failure point"
    assertEq (some "badlint") state.currentCommit "state should record the lint-failing commit"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq
      ["update:good1", "build:good1", "lint:good1", "update:badlint", "build:badlint", "lint:badlint"]
      calls
      "lint runs after build; no test step runs when only --lint is enabled"

/-- Scenario: with both `--test` and `--lint`, each commit runs build → test → lint in order. -/
private def «build, test, and lint run in order for each commit» : IO Unit := do
  withTempDir "hopscotch-build-test-lint" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\n"
    configureMockLake projectDir "success"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { runTest := true, runLint := true }
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "all checks passing should complete the run"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "every check passing should complete the session"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:good1", "build:good1", "test:good1", "lint:good1"] calls
      "each commit should run build, then test, then lint"

/-- Scenario: resuming a session with a different verify set (e.g. dropping `--test`) is rejected,
    because changing which checks run changes what pass/fail means for already-recorded probes. -/
private def «reject changed verify steps on resume» : IO Unit := do
  withTempDir "hopscotch-verify-change" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    -- First run with the test step enabled stops at the failing build.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { runTest := true }
      quiet := true
    } ignoreOutput
    -- Resuming without --test must be rejected with the usual reset hint.
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        quiet := true
      } ignoreOutput
      fail "changing the verify steps on resume should be rejected"
    catch error =>
      assertContains "verification steps changed since the last run" error.toString
        "toggling --test/--lint should require resetting persisted state"
      assertContains "delete .lake/hopscotch/ to start over" error.toString
        "the verify-steps rejection should give the reset instruction"

/-- Scenario: when the downstream project has no test driver configured, the preflight
    (`lake check-test`) aborts the run as a tool error before any probe, rather than letting
    every commit fail at the test stage and reporting a spurious boundary. -/
private def «missing test driver aborts the run before searching» : IO Unit := do
  withTempDir "hopscotch-missing-driver" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\ngood2\n"
    configureMockLake projectDir "missing-driver"
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { runTest := true }
        quiet := true
      } ignoreOutput
      fail "a missing test driver should abort the run, not record a failed hop"
    catch error =>
      assertContains "no test driver configured" error.toString
        "the run should abort with a clear no-driver message"
      assertContains "drop --test" error.toString
        "the no-driver message should suggest configuring a driver or dropping the flag"
    -- The preflight runs before any probe, so no state and no probe calls are recorded.
    let statePath := projectDir / ".lake" / "hopscotch" / "state.json"
    assertTrue (! (← statePath.pathExists))
      "a no-driver abort should happen before any state is persisted"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy
    assertEq "" calls
      "no probe (update/build/test) should run when the driver preflight fails"

/-- Scenario: extra `--build-args` / `--test-args` are appended to the actual `lake`
    invocations (and `lake update` is left untouched). -/
private def «verify-step args are passed to lake» : IO Unit := do
  withTempDir "hopscotch-verify-args" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\n"
    configureMockLake projectDir "success"
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        { runTest := true, buildArgs := #["-Kfoo=bar"], testArgs := #["--", "--verbose"] }
      quiet := true
    } ignoreOutput
    assertEq 0 result.exitCode "run with extra args should complete"
    let argsLog := (← IO.FS.readFile (mockLakeArgsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:", "build:-Kfoo=bar", "test:-- --verbose"] argsLog
      "build/test should receive their configured extra args; lake update gets none"

/-- Scenario: resuming with different verify-step args is rejected, since the args change
    what pass/fail means for already-probed commits. -/
private def «reject changed build-args on resume» : IO Unit := do
  withTempDir "hopscotch-args-change" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { buildArgs := #["-Kfoo=bar"] }
      quiet := true
    } ignoreOutput
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { buildArgs := #["-Kfoo=different"] }
        quiet := true
      } ignoreOutput
      fail "changing build args on resume should be rejected"
    catch error =>
      assertContains "verification steps changed since the last run" error.toString
        "changing --build-args should require resetting persisted state"

/-- Scenario: `hopscotch continue` rebuilds the run config from saved state alone — run
    mode, dependency scope, and the full verify pipeline (including `--test` and build
    args) — without the original flags being re-specified. -/
private def «continue reconstructs the run config from saved state» : IO Unit := do
  withTempDir "hopscotch-continue-reconstruct" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    -- Create a session with a rich verify config (test enabled + build args).
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        { runTest := true, buildArgs := #["-Kfoo=bar"] }
      quiet := true
    } ignoreOutput
    match ← CLI.parseArgs ["continue", "--project-dir", projectDir.toString] with
    | .run cfg =>
        assertEq .linear cfg.runMode "continue should restore the run mode"
        assertEq "batteries" cfg.strategy.scope "continue should restore the dependency scope"
        assertEq #["lake build -Kfoo=bar", "lake test"] (cfg.strategy.verify.map (·.label))
          "continue should restore the verify steps, including --test and build args"
        assertTrue (!cfg.autoFixes.isEmpty) "auto-fix should default to enabled on continue"
        match cfg.itemSource with
        | .range none none none => pure ()
        | other => fail s!"continue should resume from the stored item list, got {repr other}"
    | _ => fail "continue should parse to a run command"
    -- --no-auto-fix is honored on continue.
    match ← CLI.parseArgs ["continue", "--project-dir", projectDir.toString, "--no-auto-fix"] with
    | .run cfg => assertTrue cfg.autoFixes.isEmpty "--no-auto-fix should disable detection on continue"
    | _ => fail "continue should parse to a run command"

/-- Scenario: `continue` reconstructs a toolchain session too — the strategy kind is
    restored from state, and its verify steps are preserved (toolchain runs never strip
    the build step, so neither does continuing one). -/
private def «continue reconstructs a toolchain session» : IO Unit := do
  withTempDir "hopscotch-continue-toolchain" fun dir => do
    let projectDir := dir / "downstream"
    let listPath := dir / "toolchains.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile listPath "good0\nbadbuild1\n"
    configureMockLake projectDir "fail-build-toolchain"
    let _ ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      runMode := .linear
      quiet := true
    } ignoreOutput
    match ← CLI.parseArgs ["continue", "--project-dir", projectDir.toString] with
    | .run cfg =>
        assertEq "toolchain" cfg.strategy.scope "continue should rebuild the toolchain strategy"
        assertEq #["lake build"] (cfg.strategy.verify.map (·.label))
          "a toolchain session's verify steps should be preserved on continue"
    | _ => fail "continue should parse to a run command"

/-- Scenario: the persisted strategy spec round-trips and drives a correct resume — a
    stopped session reconstructed from state alone finishes, re-running the verify steps
    (here `lake test`) it was originally configured with. -/
private def «continue resumes a stopped session to completion» : IO Unit := do
  withTempDir "hopscotch-continue-resume" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    let first ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand) { runTest := true }
      quiet := true
    } ignoreOutput
    assertEq 1 first.exitCode "first run should stop at the failing build"
    -- Read back the persisted spec and rebuild the strategy exactly as `continue` does,
    -- but pointed at the mock lake (production `continue` hardcodes the real `lake`).
    let paths ← State.mkPaths projectDir
    let some persisted ← State.load? paths | fail "session state should exist"
    let some spec := persisted.strategySpec | fail "session should persist a strategy spec"
    assertEq true spec.runTest "persisted spec should record that --test was enabled"
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"
    let continued ← Runner.run {
      itemSource := .range none none none
      projectDir := projectDir
      runMode := persisted.runMode
      strategy := Runner.lakefileStrategy persisted.strategyScope (← mockLakeCommand)
        { runTest := spec.runTest, runLint := spec.runLint
          buildArgs := spec.buildArgs, testArgs := spec.testArgs, lintArgs := spec.lintArgs }
      quiet := true
    } ignoreOutput
    assertEq 0 continued.exitCode "continue should resume and finish"
    let state ← loadState paths.statePath
    assertEq (.fullySuccessful) state.status "resumed run should complete"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy
    assertTrue (calls.contains "test:badbuild")
      "the resumed run should re-run the test step the session was configured with"

/-- Scenario: `continue` with no stored session reports a clear error. -/
private def «continue without a session errors clearly» : IO Unit := do
  withTempDir "hopscotch-continue-nosession" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    try
      let _ ← CLI.parseArgs ["continue", "--project-dir", projectDir.toString]
      fail "continue with no stored session should error"
    catch error =>
      assertContains "no hopscotch session found" error.toString
        "continue should report when there is nothing to resume"

/-- Scenario: `continue` on a session predating the strategy spec reports a clear error
    rather than guessing. -/
private def «continue on a pre-spec session errors clearly» : IO Unit := do
  withTempDir "hopscotch-continue-oldsession" fun dir => do
    let projectDir := dir / "downstream"
    makeDownstreamProject projectDir
    let paths ← State.mkPaths projectDir
    State.save paths {
      projectDir := paths.projectDir
      strategyScope := "batteries"
      verifySteps := some #["lake build"]
      items := #["good1", "good2"]
      runMode := .linear
      nextIndex := 1
      currentCommit := some "good2"
      lastSuccessfulCommit := some "good1"
      status := .running
      stage := none
      lastLogPath := none
      strategySpec := none
      updatedAt := "2026-01-01T00:00:00Z"
    }
    try
      let _ ← CLI.parseArgs ["continue", "--project-dir", projectDir.toString]
      fail "continue on a session without a strategy spec should error"
    catch error =>
      assertContains "older hopscotch" error.toString
        "continue should explain that pre-spec sessions can't be resumed"

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
  test_case «bisect all-pass with keep-last-good»,
  test_case «reject mode switch on resume»,
  test_case «reject changed bisect commit list on resume»,
  test_case «bisect allow dirty workspace is accepted»,
  test_case «fullySuccessful session returns immediately without re-executing»,
  test_case «missing lakefile is rejected with a clear error»,
  test_case «resume with stale schema version is rejected»,
  test_case «range mode rejects a non-GitHub git URL»,
  test_case «quiet := false does not suppress runner progress messages»,
  test_case «bisect default end state pins first bad commit»,
  test_case «bisect keep last good pins last good commit»,
  test_case «linear keep last good pins last good commit»,
  test_case «skip build: build step not invoked»,
  test_case «lake test failure counts as a failed hop»,
  test_case «lake lint failure counts as a failed hop»,
  test_case «build, test, and lint run in order for each commit»,
  test_case «missing test driver aborts the run before searching»,
  test_case «verify-step args are passed to lake»,
  test_case «reject changed build-args on resume»,
  test_case «reject changed verify steps on resume»,
  test_case «continue reconstructs the run config from saved state»,
  test_case «continue reconstructs a toolchain session»,
  test_case «continue resumes a stopped session to completion»,
  test_case «continue without a session errors clearly»,
  test_case «continue on a pre-spec session errors clearly»,
  test_case «strip GITHUB_TOKEN from lake child env»
]

end HopscotchTestLib.IOTests
