import HopscotchTestLib.TestUtil
import HopscotchTestLib.MockLake

open Hopscotch
open Hopscotch.State

namespace HopscotchTestLib.GitIOTests

/-- Discard runner progress output so test logs stay focused on assertions. -/
private def ignoreOutput (_ : String) : IO Unit :=
  pure ()

/-- Capture runner progress output for assertions about warnings and resume blocks. -/
private def captureOutput : IO ((String → IO Unit) × IO (Array String)) := do
  let ref ← IO.mkRef #[]
  let output (line : String) : IO Unit := do
    ref.modify fun lines => lines.push line
  pure (output, ref.get)

/-- Scenario: a fixed commit must be committed before the runner continues past it. -/
private def «block dirty workspace after fix» : IO Unit := do
  withTempDir "hopscotch-git-block" fun dir => do
    -- Prepare a git-backed downstream project with a resumable failing build.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "fix.txt") "baseline\n"
    initializeGitRepo projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"

    -- Act: run the first session until it fails on the target commit.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput

    -- Prepare an uncommitted fix and a successful mock lake for the resume attempt.
    IO.FS.writeFile (projectDir / "fix.txt") "working tree fix\n"
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"
    let (output, getLines) ← captureOutput

    -- Act: resume the session with an uncommitted fix in the worktree.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } output

    -- Assert the git-cleanliness gate blocks continuation and preserves the failing commit state.
    assertEq 1 result.exitCode
      "dirty fixes should block resume after the build starts passing"

    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.stopped) state.status "resume gate should persist as a normal failure"
    assertEq (some RunStage.gitCheck) state.stage "dirty workspace should record the git-check stage"
    assertEq (some "badbuild") state.currentCommit "resume gate should keep the failing commit pinned"
    assertEq 1 state.nextIndex "resume gate should keep the same commit index"

    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:badbuild", "build:badbuild"] calls
      "dirty workspace should rerun the failed commit but never advance to later commits"

    let summary := (← IO.FS.readFile result.summaryPath)
    assertContains "Failure stage: git cleanliness check" summary
      "blocked resumes should mention the git cleanliness gate in the summary"

    let lines ← getLines
    assertTrue (lines.any fun line => line.contains "Resume blocked: commit the fix")
      "blocked resumes should explain how to continue"

/-- Scenario: the dirty-workspace gate can be bypassed explicitly. -/
private def «allow dirty workspace bypass» : IO Unit := do
  withTempDir "hopscotch-git-bypass" fun dir => do
    -- Prepare a git-backed downstream project with a resumable failing build.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "fix.txt") "baseline\n"
    initializeGitRepo projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"

    -- Act: run the first session until it fails on the target commit.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput

    -- Assert: The build failed
    assertEq 1 result.exitCode "build should have failed"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.stopped) state.status "build should have failed"

    -- Prepare an uncommitted fix and a successful mock lake for the bypassed resume.
    IO.FS.writeFile (projectDir / "fix.txt") "working tree fix\n"
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"

    -- Act: resume the session with `--allow-dirty-workspace`.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
      allowDirtyWorkspace := true
    } ignoreOutput

    -- Assert the dirty-workspace bypass allows the session to continue into later commits.
    assertEq 0 result.exitCode "allow-dirty-workspace should bypass the git cleanliness gate"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "bypassed resumes should still complete normally"
    let calls := (← IO.FS.readFile (mockLakeCallsPath projectDir)).trimAscii.copy.splitOn "\n"
    assertEq ["update:badbuild", "build:badbuild", "update:good2", "build:good2"] calls
      "bypassed resumes should continue into later commits"

/-- Scenario: Lean-style `.gitignore` entries for `.lake/` keep the git gate simple. -/
private def «respect ignored .lake state in git gate» : IO Unit := do
  withTempDir "hopscotch-git-tool-state" fun dir => do
    -- Prepare a git-backed downstream project that should rely on ignored `.lake/` state.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "fix.txt") "baseline\n"
    initializeGitRepo projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    -- Act: run the first session until it fails on the target commit.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Prepare a committed fix while leaving tool-owned `.lake/` artifacts in place.
    IO.FS.writeFile (projectDir / "fix.txt") "committed fix\n"
    commitPaths projectDir "fix badbuild" #["lakefile.toml", "fix.txt"]
    -- Assert the repo stays clean because `.lake/` is ignored by git.
    let status ← runGitWithOutput projectDir #["status", "--porcelain"]
    assertEq "" status.stdout ".lake artifacts should stay hidden by the project's gitignore"
    -- Prepare the resumed run after the committed fix.
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"
    -- Act: resume the session after committing the fix.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the ignored `.lake/` state does not block the resumed session.
    assertEq 0 result.exitCode "ignored .lake state should not block a cleanly committed fix"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.fullySuccessful) state.status "committed fixes should let the session complete"

/-- Scenario: a fully clean git repo after the fix commit resumes normally. -/
private def «resume with clean git repo» : IO Unit := do
  withTempDir "hopscotch-git-clean" fun dir => do
    -- Prepare a git-backed downstream project for a clean committed-fix resume.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "fix.txt") "baseline\n"
    initializeGitRepo projectDir [".lake/hopscotch/"]
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    -- Act: run the first session until it fails on the target commit.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Prepare a committed fix and verify the repo is clean before resuming.
    IO.FS.writeFile (projectDir / "fix.txt") "committed fix\n"
    commitPaths projectDir "fix badbuild" #["lakefile.toml", "fix.txt"]
    -- Assert the git worktree is clean before the resumed session starts.
    let status ← runGitWithOutput projectDir #["status", "--porcelain"]
    assertEq "" status.stdout "the repo should be clean before resuming this scenario"
    -- Prepare the resumed run after the clean committed fix.
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"
    -- Act: resume the session after committing the fix in a clean repo.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Assert the clean git repo resumes without tripping the git gate.
    assertEq 0 result.exitCode "clean git repos should resume without hitting the gate"

/-- Scenario: projects without a usable git checkout warn and continue. -/
private def «warn and continue when git unavailable» : IO Unit := do
  withTempDir "hopscotch-no-git" fun dir => do
    -- Prepare a downstream project without a usable git checkout.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\ngood2\n"
    configureMockLake projectDir "fail-build"
    -- Act: run the first session until it fails on the target commit.
    let _ ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } ignoreOutput
    -- Prepare the resume attempt and capture runner output for warnings.
    IO.FS.writeFile (mockLakeCallsPath projectDir) ""
    configureMockLake projectDir "success"
    let (output, getLines) ← captureOutput
    -- Act: resume the session when git status is unavailable.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      quiet := true
    } output
    -- Assert the runner warns and continues instead of blocking on git verification.
    assertEq 0 result.exitCode "missing git should warn and continue instead of blocking"
    let lines ← getLines
    assertTrue (lines.any fun line => line.contains "Warning: git status unavailable")
      "missing git should emit a warning about skipping verification"

/-- Scenario: bisect refuses to start when the working tree has uncommitted changes. -/
private def «bisect blocks dirty worktree at session start» : IO Unit := do
  withTempDir "hopscotch-bisect-git-block" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "fix.txt") "baseline\n"
    initializeGitRepo projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    -- Dirty the worktree with an uncommitted change before starting bisect.
    IO.FS.writeFile (projectDir / "fix.txt") "uncommitted change\n"
    try
      let _ ← Runner.run {
        itemSource := .file commitListPath
        projectDir := projectDir
        strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
        runMode := .bisect
        quiet := true
      } ignoreOutput
      fail "bisect should refuse to start with a dirty worktree"
    catch error =>
      assertContains "bisect requires a clean working tree" error.toString
        "bisect should name the problem and suggest the resolution"

/-- Scenario: bisect proceeds despite a dirty worktree when --allow-dirty-workspace is set. -/
private def «bisect allow dirty workspace bypass at session start» : IO Unit := do
  withTempDir "hopscotch-bisect-git-bypass" fun dir => do
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile (projectDir / "fix.txt") "baseline\n"
    initializeGitRepo projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    -- Dirty the worktree with an uncommitted change.
    IO.FS.writeFile (projectDir / "fix.txt") "uncommitted change\n"
    -- Act: run bisect with the bypass flag.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
      allowDirtyWorkspace := true
    } ignoreOutput
    -- Bisect should resolve normally despite the dirty worktree.
    assertEq 1 result.exitCode
      "bisect with --allow-dirty-workspace should bypass the dirty-worktree check and find the boundary"
    let state ← loadState (projectDir / ".lake" / "hopscotch" / "state.json")
    assertEq (.stopped) state.status "bisect should resolve the boundary normally"
    assertEq (some "badbuild") state.currentCommit "bisect should identify the failing commit"

/-- Scenario: bisect warns and continues when git is unavailable instead of blocking. -/
private def «bisect warns when git unavailable at session start» : IO Unit := do
  withTempDir "hopscotch-bisect-no-git" fun dir => do
    -- Prepare a downstream project without a usable git checkout.
    let projectDir := dir / "downstream"
    let commitListPath := dir / "commits.txt"
    makeDownstreamProject projectDir
    IO.FS.writeFile commitListPath "good1\nbadbuild\n"
    configureMockLake projectDir "fail-build"
    let (output, getLines) ← captureOutput
    -- Act: run bisect when git status is unavailable.
    let result ← Runner.run {
      itemSource := .file commitListPath
      projectDir := projectDir
      strategy := Runner.lakefileStrategy "batteries" (← mockLakeCommand)
      runMode := .bisect
      quiet := true
    } output
    -- The runner should warn about missing git but still resolve the boundary.
    assertEq 1 result.exitCode
      "bisect should resolve normally when git is unavailable"
    let lines ← getLines
    assertTrue (lines.any fun line => line.contains "Warning: git status unavailable")
      "bisect should warn about the missing git check"

def suite : TestSuite := #[
  test_case «block dirty workspace after fix»,
  test_case «allow dirty workspace bypass»,
  test_case «respect ignored .lake state in git gate»,
  test_case «resume with clean git repo»,
  test_case «warn and continue when git unavailable»,
  test_case «bisect blocks dirty worktree at session start»,
  test_case «bisect allow dirty workspace bypass at session start»,
  test_case «bisect warns when git unavailable at session start»
]

end HopscotchTestLib.GitIOTests
