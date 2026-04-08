import Hopscotch.Runner.Types
import Hopscotch.Runner.Command
import Hopscotch.Runner.Summary
import Hopscotch.Runner.StateMachine
import Hopscotch.ItemList
import Hopscotch.GitHub
import Hopscotch.State
import Hopscotch.Util

namespace Hopscotch.Runner

open Hopscotch
open Hopscotch.State

/-- Copy the culprit log file into the designated culprit subfolder for easy discovery.
    Skips silently if the source file does not exist (e.g. interrupted before the step ran). -/
private def copyCulpritLog (paths : Paths) (culpritLogPath : System.FilePath) : IO Unit := do
  if ← culpritLogPath.pathExists then
    IO.FS.createDirAll paths.culpritLogsDir
    let fileName := culpritLogPath.fileName.getD culpritLogPath.toString
    IO.FS.writeFile (paths.culpritLogsDir / fileName) (← IO.FS.readFile culpritLogPath)

/-- Remove the culprit subfolder when the previously-failing commit passes on resume,
    so the folder only exists while the failure is unresolved. -/
private def clearCulpritLogs (paths : Paths) : IO Unit := do
  if ← paths.culpritLogsDir.pathExists then
    IO.FS.removeDirAll paths.culpritLogsDir

/-- Result of one full bump + verify probe. -/
private inductive ProbeRunResult where
  | success
  | failure (stage : RunStage) (logPath : System.FilePath)
  deriving Repr

/--
Run one full bump + verify probe while keeping restartable state current.

The probe runs two phases in order:

1. **Bump** — call `strategy.mkBump commit` and run it. Writes the commit to the
   downstream project (e.g. rewrites `lakefile.toml` or `lean-toolchain`).
2. **Verify** — run each step in `strategy.verify` (e.g. `lake build`).

Before each step, `saveState` is called with `stage = some <step.stage>` so that a
restart can detect where execution was interrupted and skip already-completed steps.

**Resumption logic**: if the stored state points at this exact `(index, commit)` and
`stage` matches a verify step (not the bump step), the bump phase is skipped and
verification restarts from the saved stage. This handles the case where the runner
was interrupted between the bump succeeding and a verify step completing.
-/
private def runProbe (config : Config) (paths : Paths) (base : PersistedState)
    (stepNum : Nat) (index : Nat) (commit : String)
    (emit : ConsoleStyle → String → IO Unit) : IO ProbeRunResult := do
  let bumpStep := config.strategy.mkBump commit
  let namePrefix := config.strategy.logPrefix stepNum index
  -- Determine whether we are resuming at a specific stage of this exact commit.
  let resumeStage? : Option RunStage :=
    if base.status == .running && base.nextIndex == index && base.currentCommit == some commit
    then base.stage
    else none
  -- We are resuming into a verify step when the saved stage is a verify stage
  -- (as opposed to the bump stage or no saved stage at all).
  let resumingVerify :=
    match resumeStage? with
    | none => false
    | some stage =>
        -- bumpStep.stage is always .bump; this rules out resuming mid-bump.
        stage != bumpStep.stage && config.strategy.verify.any (·.stage == stage)
  -- In bisect mode, restore the baseline toolchain before every fresh probe.
  if base.runMode == .bisect && !resumingVerify then
    restoreBisectBaselineToolchain paths base
  let timestamp ← nowUtcString
  emit .attempt s!"[{timestamp}] Attempting commit {commit} ({index + 1}/{base.items.size})"
  -- Bump phase: apply the version then fetch. Skipped when resuming mid-verify.
  if !resumingVerify then
    let _ ← saveState paths <| buildRunningState base index commit (some bumpStep.stage)
    let bumpLogPath := State.logPath paths namePrefix commit bumpStep.stage
    emit .running s!"[{← nowUtcString}] Running {bumpStep.label}"
    let bumpOk ← bumpStep.run paths.projectDir bumpLogPath config.quiet
    emit (if bumpOk then .success else .failure)
      s!"[{← nowUtcString}] Finished {bumpStep.label} (log file: {bumpLogPath})"
    if !bumpOk then
      return .failure bumpStep.stage bumpLogPath
  -- Verify phase: run each step in order, resuming from the saved point if applicable.
  let startAt :=
    match resumeStage? with
    | none => 0
    | some stage =>
        config.strategy.verify.findIdx? (·.stage == stage) |>.getD 0
  for step in config.strategy.verify.extract startAt config.strategy.verify.size do
    let _ ← saveState paths <| buildRunningState base index commit (some step.stage)
    let verifyLogPath := State.logPath paths namePrefix commit step.stage
    emit .running s!"[{← nowUtcString}] Running {step.label}"
    let stepOk ← step.run paths.projectDir verifyLogPath config.quiet
    emit (if stepOk then .success else .failure)
      s!"[{← nowUtcString}] Finished {step.label} (log file: {verifyLogPath})"
    if !stepOk then
      return .failure step.stage verifyLogPath
  return .success

/--
Drive the linear-mode state machine to completion or first failure.

Loads (or creates) the initial state, then iterates `runProbe` over the commit list
from `nextIndex` onward. Each successful probe calls `advanceAfterSuccess`; a failing
probe calls `buildFailureState` and returns immediately. The session is also returned
immediately when the stored status is already `completed`.

On resume after a previously-failed commit, a git cleanliness check is performed
before advancing past that commit (unless `--allow-dirty-workspace` was passed).
-/
private def runAdvance (config : Config) (paths : Paths) (commits : Array String)
    (emit : ConsoleStyle → String → IO Unit) : IO RunResult := do
  let mut state ← loadInitialState paths config commits
  let resumedFailedCommit := state.status == .failed
  let resumeIndex := state.nextIndex
  validateState state commits
  if state.status == .completed then
    let summary ← writeSummary paths state
    return { exitCode := 0, summary := summary, summaryPath := paths.summaryPath }

  -- `lastSummary` captures the most recent summary written by `saveState`.
  -- It is always set before the loop exits normally (status → completed).
  let mut lastSummary : String := ""
  for index in [state.nextIndex:commits.size] do
    let commit := commits[index]!
    match ← runProbe config paths state index index commit emit with
    | .failure stage logPath =>
        let (_, summary) ← saveState paths <| buildFailureState state index commit stage logPath
        copyCulpritLog paths logPath
        return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
    | .success =>
        if resumedFailedCommit && index == resumeIndex && !config.allowDirtyWorkspace then
          match ← checkGitWorktree paths.projectDir with
          | .clean =>
              pure ()
          | .unavailable =>
              emit .running
                s!"[{← nowUtcString}] Warning: git status unavailable; skipping dirty-workspace verification"
          | .dirty =>
              let buildLogPath := State.logPath paths (config.strategy.logPrefix index index) commit .build
              emit .failure
                s!"[{← nowUtcString}] Resume blocked: commit the fix for {commit} or rerun with --allow-dirty-workspace"
              let (_, summary) ← saveState paths <| buildFailureState state index commit .gitCheck buildLogPath
              copyCulpritLog paths buildLogPath
              return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
        if resumedFailedCommit && index == resumeIndex then
          clearCulpritLogs paths
        let saved ← saveState paths <| advanceAfterSuccess state commits index commit
        state := saved.1
        lastSummary := saved.2
  return { exitCode := 0, summary := lastSummary, summaryPath := paths.summaryPath }

/--
Drive the bisect-mode state machine to the exact failure boundary.

Implements the binary search loop described in `Hopscotch.Runner.StateMachine`:

1. Load (or create) the initial state. If already `Failed`, return immediately.
2. Check that the working tree is clean (unless `allowDirtyWorkspace` is set).
   Bisect restores only `lean-toolchain` between probes; uncommitted source-file
   changes persist across the entire search and would corrupt every probe result.
3. Determine the next probe index:
   - If `stage` is set: mid-step resume; reuse the saved `nextIndex`.
   - If `verifiedBad` is false: probe the bad endpoint first.
   - Otherwise: probe the midpoint `nextBisectProbeIndex?`.
   - If no midpoint (bounds adjacent): resolve with `buildBisectResolvedState`.
4. Run `runProbe`. On success narrow from the good side; on failure narrow from
   the bad side. Update `BisectState.probeResults` for the cached result.
5. Recurse via `loop` until the bounds are adjacent.

Declared `partial` because Lean cannot prove termination of the recursive bisect
loop from the type alone; termination is guaranteed by the invariant that each
iteration strictly narrows the search window.
-/
private partial def runBisect (config : Config) (paths : Paths) (commits : Array String)
    (emit : ConsoleStyle → String → IO Unit) : IO RunResult := do
  let state ← loadInitialState paths config commits
  validateState state commits
  if state.status == .failed then
    let summary ← writeSummary paths state
    return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
  -- Bisect restores only `lean-toolchain` between probes, not source files.
  -- Uncommitted changes would therefore silently affect every probe in the search.
  if !config.allowDirtyWorkspace then
    match ← checkGitWorktree paths.projectDir with
    | .dirty =>
        throw <| IO.userError
          "bisect requires a clean working tree; commit or stash your changes first, or pass --allow-dirty-workspace"
    | .unavailable =>
        emit .running
          s!"[{← nowUtcString}] Warning: git status unavailable; skipping dirty-worktree check"
    | .clean => pure ()

  let rec loop (state : PersistedState) : IO RunResult := do
    let some bisect := state.bisect
      | throw <| IO.userError "stored bisect state is missing bisect metadata"
    let probeIndex ←
      if state.stage.isSome then
        pure state.nextIndex          -- resuming mid-stage: reuse the index already saved
      else if !bisect.verifiedBad then
        pure bisect.knownBadIndex     -- bad endpoint not yet confirmed: verify it first
      else
        match nextBisectProbeIndex? (bisectBounds bisect) with
        | some index => pure index    -- normal bisect: probe the midpoint
        | none =>
            match knownBadFailureResult? bisect with
            | some failure =>
                let (_, summary) ← saveState paths <| buildBisectResolvedState state commits bisect failure
                if let some lp := failure.logPath then copyCulpritLog paths lp
                return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
            | none =>
                throw <| IO.userError "stored bisect state is missing the failing probe result"
    let commit := commits[probeIndex]!
    -- After updating bisect bounds: loop with the next midpoint, or resolve if bounds are adjacent.
    let advanceOrResolve (updatedBisect : BisectState) : IO RunResult := do
      match nextBisectProbeIndex? (bisectBounds updatedBisect) with
      | some nextIndex =>
          let saved ← saveState paths <| buildBisectSearchState state commits updatedBisect nextIndex
          loop saved.1
      | none =>
          match knownBadFailureResult? updatedBisect with
          | some failure =>
              let (_, summary) ← saveState paths <| buildBisectResolvedState state commits updatedBisect failure
              if let some lp := failure.logPath then copyCulpritLog paths lp
              return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
          | none =>
              throw <| IO.userError "bisect resolution is missing the failing probe result"
    let stepNum := bisect.probeResults.size
    match ← runProbe config paths state stepNum probeIndex commit emit with
    | .success =>
        if !bisect.verifiedBad then
          throw <| IO.userError
            s!"bisect mode requires a known failing endpoint, but the last commit succeeded during re-verification: {commit}"
        advanceOrResolve { bisect with
          knownGoodIndex := probeIndex
          probeResults := recordProbeResult bisect.probeResults {
            index := probeIndex
            commit := commit
            outcome := .success
          }
        }
    | .failure stage logPath =>
        advanceOrResolve { bisect with
          knownBadIndex := probeIndex
          verifiedBad := true
          probeResults := recordProbeResult bisect.probeResults {
            index := probeIndex
            commit := commit
            outcome := .failure
            stage := some stage
            logPath := some logPath
          }
        }
  loop state

/--
Resolve the commit list from the configured source.

For `.file`, loads commits directly from disk. For `.range`, reads metadata from
the run strategy when not overridden and fetches the ordered SHAs from the
GitHub API.

`warn` is called for non-fatal informational messages: the missing-token rate-limit
warning and the "no --to given, using default branch tip" notice.
-/
private def resolveItems (config : Config) (paths : Paths)
    (warn : String → IO Unit) : IO (Array String) := do
  match config.itemSource with
  | .file path => ItemList.load path
  | .range toRef fromRef gitUrl =>
      let resolvedUrl ←
        match gitUrl with
        | some url => pure url
        | none =>
            match ← config.strategy.defaultGitUrl paths.projectDir with
            | some url => pure url
            | none =>
                throw <| IO.userError
                  s!"no git URL found for '{config.strategy.name}'; \
                     use --git-url to specify it"
      let some (owner, repo) := GitHub.parseRepoId resolvedUrl
        | throw <| IO.userError
            s!"'{resolvedUrl}' is not a recognized GitHub URL; \
               only github.com repositories are supported"
      let token ← IO.getEnv "GITHUB_TOKEN"
      GitHub.warnIfNoToken token warn
      -- Cache the default-branch SHA so we make at most one API call even when
      -- both `fromRef` and `toRef` fall back to the tip of the default branch.
      let defaultBranchCache ← IO.mkRef (none : Option String)
      let fetchDefaultOnce : IO String := do
        if let some sha := ← defaultBranchCache.get then return sha
        let sha ← GitHub.fetchDefaultBranch owner repo token
        defaultBranchCache.set (some sha)
        return sha
      let resolvedFrom ←
        match fromRef with
        | some ref => pure ref
        | none =>
            match ← config.strategy.defaultFromRef paths.projectDir with
            | some rev => pure rev
            | none =>
                let sha ← fetchDefaultOnce
                warn s!"No pinned rev found for '{config.strategy.name}'; \
                  using tip of default branch as lower bound: {shortCommit sha}"
                pure sha
      let resolvedTo ←
        match toRef with
        | some ref => pure ref
        | none =>
            let sha ← fetchDefaultOnce
            warn s!"No --to given; using tip of default branch: {shortCommit sha}"
            pure sha
      let commits ← GitHub.fetchCommitRange owner repo resolvedFrom resolvedTo token
      if commits.isEmpty then
        throw <| IO.userError
          s!"no commits found between {resolvedFrom} and {resolvedTo} in {owner}/{repo}"
      return commits

/--
Run or resume the requested execution mode until it either completes successfully
or records the relevant failure boundary.

`colorMode` controls whether ANSI color codes are applied to progress lines
written via `output`. Callers should detect the appropriate mode for their
output stream (e.g. `detectStdoutColor`) and pass it here; the default is
`.disabled` so library callers get plain text unless they opt in.
-/
def run (config : Config) (output : String → IO Unit := IO.println)
    (colorMode : ColorMode := .disabled) : IO RunResult := do
  let paths ← mkPaths config.projectDir
  let emit (style : ConsoleStyle) (message : String) : IO Unit :=
    output <| colorize colorMode style message
  -- For range-mode resumes, use the commit list already stored in state rather
  -- than re-calling the GitHub API. Re-fetching is both wasteful and incorrect:
  -- if --to was omitted (defaulting to the tip of main/master), the tip will
  -- have advanced since the original run, producing a different commit list and
  -- a spurious "commit list changed; delete state" error.
  -- File-mode sources are always re-read so that changes to the commits file
  -- are detected and rejected on resume, consistent with prior behavior.
  let commits ←
    match config.itemSource with
    | .file _ => resolveItems config paths (emit .running)
    | .range .. =>
        match ← State.load? paths with
        | some existingState => pure existingState.items
        | none => resolveItems config paths (emit .running)

  ensureLakefile paths
  ensureDirs paths

  match config.runMode with
  | .linear => runAdvance config paths commits emit
  | .bisect => runBisect config paths commits emit

end Hopscotch.Runner
