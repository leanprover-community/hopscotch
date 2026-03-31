import HopscotchTestLib.TestUtil
import HopscotchTestLib.MockLake

open Hopscotch
open Hopscotch.State

namespace HopscotchTestLib.ToolchainIOTests

/-- Discard runner progress output so test logs stay focused on assertions. -/
private def ignoreOutput (_ : String) : IO Unit :=
  pure ()

/-- The toolchain strategy uses `toolchains.log` (which records the actual `lean-toolchain`
    content at each mock lake invocation) as the primary assertion surface, because the
    strategy never modifies `lakefile.toml` and therefore `calls.log` entries always show
    the initial lakefile rev regardless of which toolchain is being probed. -/
private def readToolchainLog (projectDir : System.FilePath) : IO (List String) := do
  let raw := (← IO.FS.readFile (mockLakeToolchainsPath projectDir)).trimAscii.copy
  if raw.isEmpty then return []
  return raw.splitOn "\n"

/-- Scenario: the toolchain strategy advances through all-success toolchains, writes each
    to `lean-toolchain` before the build step, and leaves the file pinned to the last entry. -/
private def «toolchain strategy steps through all-success toolchain list» : IO Unit := do
  withTempDir "hopscotch-tc-success" fun dir => do
    -- Prepare: a downstream project with an all-success toolchain list.
    let projectDir := dir / "downstream"
    let listPath := dir / "toolchains.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile listPath "good0\ngood1\n"
    configureMockLake projectDir "success"
    -- Act: run the toolchain stepping session to completion.
    let result ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert: run completes and the final lean-toolchain matches the last entry.
    assertEq 0 result.exitCode "all-success toolchain run should succeed"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.completed) state.status "state should record completion"
    assertEq (some "good1") state.lastSuccessfulCommit
      "last successful commit should be the final toolchain"
    let toolchain := (← IO.FS.readFile (projectDir / "lean-toolchain")).trimAscii.copy
    assertEq "good1" toolchain
      "lean-toolchain should be pinned to the last successful entry"
    -- Assert: build was invoked once per toolchain entry.
    let calls ← readToolchainLog projectDir
    assertEq ["build:main:good0", "build:main:good1"] calls
      "build should be invoked once per toolchain"

/-- Scenario: the toolchain strategy stops at the first build failure, records the failing
    toolchain in state, and leaves `lean-toolchain` pinned to it. -/
private def «toolchain strategy stops at first build failure» : IO Unit := do
  withTempDir "hopscotch-tc-fail" fun dir => do
    -- Prepare: a toolchain list where the second entry fails to build.
    let projectDir := dir / "downstream"
    let listPath := dir / "toolchains.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile listPath "good0\nbadbuild1\ngood2\n"
    configureMockLake projectDir "fail-build-toolchain"
    -- Act: run until the first build failure.
    let result ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert: runner stops at the failing toolchain.
    assertEq 1 result.exitCode "runner should stop at the first failing build"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.failed) state.status "state should record failure"
    assertEq (some "badbuild1") state.currentCommit
      "state should record the failing toolchain"
    assertEq (some RunStage.build) state.stage
      "state should record the build stage as the failure point"
    -- Assert: lean-toolchain is left pinned to the failing entry.
    let toolchain := (← IO.FS.readFile (projectDir / "lean-toolchain")).trimAscii.copy
    assertEq "badbuild1" toolchain
      "lean-toolchain should remain pinned to the failing toolchain"
    -- Assert: no later toolchains were attempted.
    let calls ← readToolchainLog projectDir
    assertEq ["build:main:good0", "build:main:badbuild1"] calls
      "runner should not attempt toolchains after the first failure"

/-- Scenario: resuming after a toolchain build failure retries the exact same toolchain before
    advancing, consistent with the linear-mode resume contract. -/
private def «toolchain strategy resume retries the failed toolchain before advancing» : IO Unit := do
  withTempDir "hopscotch-tc-resume" fun dir => do
    -- Prepare: run until the first toolchain build failure.
    let projectDir := dir / "downstream"
    let listPath := dir / "toolchains.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile listPath "good0\nbadbuild1\ngood2\n"
    configureMockLake projectDir "fail-build-toolchain"
    let firstResult ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    assertEq 1 firstResult.exitCode "first run should fail on badbuild1"
    -- Prepare: clear the log and switch to success mode for the resume.
    IO.FS.writeFile (mockLakeToolchainsPath projectDir) ""
    configureMockLake projectDir "success"
    -- Act: resume the session from the failing toolchain.
    let secondResult ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert: the resumed run completes by retrying badbuild1 first.
    assertEq 0 secondResult.exitCode "resumed run should complete"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.completed) state.status "state should record completion after resume"
    assertEq (some "good2") state.lastSuccessfulCommit
      "resume should complete with the final toolchain as the last success"
    let calls ← readToolchainLog projectDir
    assertEq ["build:main:badbuild1", "build:main:good2"] calls
      "resume should retry the failed toolchain before advancing to later entries"

/-- Scenario: bisect mode resolves the exact first failing toolchain with binary probing,
    restoring the original `lean-toolchain` content before each fresh probe. -/
private def «toolchain strategy bisect resolves the boundary toolchain» : IO Unit := do
  withTempDir "hopscotch-tc-bisect" fun dir => do
    -- Prepare: 5 toolchains where the last 3 fail.
    let projectDir := dir / "downstream"
    let listPath := dir / "toolchains.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile listPath "good0\ngood1\nbadbuild2\nbadbuild3\nbadbuild4\n"
    configureMockLake projectDir "fail-build-toolchain"
    -- Act: run bisect mode to resolution.
    let result ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } ignoreOutput
    -- Assert: bisect resolves badbuild2 as the first failing toolchain.
    assertEq 1 result.exitCode "bisect should exit with failure after resolving"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.failed) state.status "bisect should finish with a failed boundary result"
    assertEq (some "badbuild2") state.currentCommit
      "bisect should resolve the exact first failing toolchain"
    assertEq (some "good1") state.lastSuccessfulCommit
      "bisect should record the last known-good toolchain"
    assertEq (some RunStage.build) state.stage
      "bisect should attribute the failure to the build stage"
    -- Assert: only the bad endpoint verification and the required midpoints were probed.
    -- Expected probes: badbuild4 (endpoint), badbuild2 (mid 0..4), good1 (mid 0..2).
    let calls ← readToolchainLog projectDir
    assertEq
      ["build:main:badbuild4", "build:main:badbuild2", "build:main:good1"]
      calls
      "bisect should probe only the endpoint and the required midpoints"

/-- Scenario: re-running after a completed toolchain session returns immediately without
    invoking the build tool again, making completed runs safely idempotent. -/
private def «toolchain strategy completed session returns immediately on re-run» : IO Unit := do
  withTempDir "hopscotch-tc-idempotent" fun dir => do
    -- Prepare: run a successful toolchain session to completion.
    let projectDir := dir / "downstream"
    let listPath := dir / "toolchains.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile listPath "good0\ngood1\n"
    configureMockLake projectDir "success"
    let firstResult ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    assertEq 0 firstResult.exitCode "first run should complete successfully"
    -- Prepare: clear the build log before the second run.
    IO.FS.writeFile (mockLakeToolchainsPath projectDir) ""
    -- Act: re-run the same configuration against the completed state.
    let secondResult ← Runner.run {
      itemSource := .file listPath
      projectDir := projectDir
      strategy := Runner.toolchainStrategy (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert: second run exits 0 immediately with no new build invocations.
    assertEq 0 secondResult.exitCode "re-running a completed session should return 0"
    let calls ← readToolchainLog projectDir
    assertEq ([] : List String) calls
      "re-running a completed session should not invoke the build tool again"

def suite : TestSuite := #[
  test_case «toolchain strategy steps through all-success toolchain list»,
  test_case «toolchain strategy stops at first build failure»,
  test_case «toolchain strategy resume retries the failed toolchain before advancing»,
  test_case «toolchain strategy bisect resolves the boundary toolchain»,
  test_case «toolchain strategy completed session returns immediately on re-run»
]

end HopscotchTestLib.ToolchainIOTests
