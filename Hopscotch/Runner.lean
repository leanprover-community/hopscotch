import Hopscotch.Runner.Types
import Hopscotch.Runner.Command
import Hopscotch.Runner.Summary
import Hopscotch.Runner.StateMachine
import Hopscotch.ItemList
import Hopscotch.GitHub
import Hopscotch.Results
import Hopscotch.State
import Hopscotch.AutoFix.Framework
import Hopscotch.Util

namespace Hopscotch.Runner

open Hopscotch
open Hopscotch.State
open Hopscotch.AutoFix

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

/-- Delete everything under `<projectDir>/.lake/` except the `hopscotch` subfolder
    (which holds the persisted state we want to keep across probes).  No-op when
    `.lake/` does not exist yet. -/
private def nukeLakeDir (paths : Paths) : IO Unit := do
  let lakeDir := paths.projectDir / ".lake"
  unless ← lakeDir.pathExists do return
  for entry in ← lakeDir.readDir do
    if entry.fileName == "hopscotch" then continue
    if ← entry.path.isDir then
      IO.FS.removeDirAll entry.path
    else
      IO.FS.removeFile entry.path

/-- After the search completes, apply `mkBump` so the local lakefile/toolchain
    reflects the desired endpoint.  Failure is non-fatal: the search result is
    already saved; we just emit a warning.

    Note: the log is written to a fixed path `restore.log` rather than a
    numbered probe log.  This is fine because `restoreTo` is called at most once
    per session, immediately before `run` returns.  If the tool is ever extended
    to perform multiple restorations in one session (e.g. multi-boundary
    detection), each call would overwrite the previous restore log; use a
    timestamped or indexed path instead. -/
private def restoreTo (config : Config) (paths : Paths) (commit : String)
    (emit : ConsoleStyle → String → IO Unit) : IO Unit := do
  let bumpStep := config.strategy.mkBump commit
  -- Fixed log path: safe because restoreTo is called at most once per run (see note above).
  let logPath := paths.logsDir / "restore.log"
  let ok ← bumpStep.run paths.projectDir logPath config.quiet
  unless ok do
    emit .running s!"[{← nowUtcString}] Warning: could not restore to {commit} (see {logPath})"

/-- Result of one full bump + verify probe. Both outcomes carry the `lake build`
    step's log (when the build step ran), so detection can scan it for the
    toolchain's deprecation warnings regardless of which later step (`lake test` /
    `lake lint`) ultimately failed or was last on a green probe — deprecation
    warnings are emitted at build time. On a failure, `logPath` is still the
    failing step's log (used for the culprit copy and `lastLogPath`). -/
private inductive ProbeRunResult where
  | success (buildLog : Option System.FilePath)
  | failure (stage : RunStage) (logPath : System.FilePath) (buildLog : Option System.FilePath)
  deriving Repr

/--
After a run concludes, run the registered fixes' detection once and record the
results on the state: migrations proposed for the failure boundary (only when
the run stopped) and advisories for imports that resolve through live
deprecation shims (on any conclusion — a fully-successful run still surfaces
what will break at the next upstream shim cleanup). Both are surfaced via
`summary.md` and `results.json`; nothing is applied — applying is the consumer's
choice (`hopscotch fix apply`, followed by a re-run to search past the repaired
breakage).
-/
private def attachProposedFixes (config : Config) (paths : Paths)
    (state : PersistedState) (emit : ConsoleStyle → String → IO Unit)
    (buildLog : Option System.FilePath := none) : IO PersistedState := do
  if config.autoFixes.isEmpty then return state
  -- Inspect the boundary commit when stopped; the newest range commit otherwise.
  let some commit := state.currentCommit <|> state.items.back? | return state
  let ctx : FixContext := {
    projectDir := paths.projectDir
    paths := paths
    dependencyName := config.strategy.scope
    items := state.items
    currentCommit := commit
    -- Known-good diff baseline for the fixes: the range's `from` rev when the run
    -- came from a commit range. File-sourced lists carry no lower bound, so use
    -- the first item — lists are oldest→newest by convention, making it the
    -- closest known-good proxy. (`getD commit` is unreachable: a run has items.)
    baseline := state.lowerBoundRef.getD (state.items[0]?.getD commit)
    -- Prefer the build log (where deprecation warnings live); fall back to
    -- `lastLogPath` for paths that don't carry one (e.g. a resumed bisect
    -- resolving from a stored failing probe — there `lastLogPath` is the
    -- boundary's failing-step log).
    buildLogPath := buildLog <|> state.lastLogPath
    quiet := config.quiet }
  let detected ← AutoFix.detectFixes config.autoFixes ctx
    (fun msg => do emit .running s!"[{← nowUtcString}] {msg}")
  if state.status == .stopped then
    return { state with
      proposedFixes := detected.migrations
      deprecatedImports := detected.advisories
      autoFixNotes := detected.notes }
  else
    -- On a green conclusion nothing needs fixing; anything detection still
    -- proposed (e.g. a warned deprecation that did not fail this build) is worth
    -- migrating all the same, so fold it into the advisories.
    return { state with
      proposedFixes := #[]
      deprecatedImports := detected.advisories ++ detected.migrations
      autoFixNotes := detected.notes }

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
  -- INTERNAL: HOPSCOTCH_DEBUG_NUKE_LAKEDIR wipes `<projectDir>/.lake/` (preserving
  -- our own state under `.lake/hopscotch/`) before every probe, forcing a fully
  -- fresh build.  Intended as a paranoia knob for obscure failures suspected to
  -- be caused by stale cached artifacts.  Not part of the public CLI; may change
  -- without notice.
  let nukeLakeDirEnabled ←
    match ← IO.getEnv "HOPSCOTCH_DEBUG_NUKE_LAKEDIR" with
    | some "true" | some "1" => pure true
    | _                      => pure false
  let bumpStep := config.strategy.mkBump commit
  let namePrefix := config.strategy.logPrefix stepNum index
  -- Determine whether we are resuming at a specific stage of this exact commit.
  let resumeStage? : Option RunStage :=
    if base.status == .running && base.nextIndex == index && base.currentCommit == some commit
    then base.stage
    else none
  -- We are resuming into a verify step when the saved stage is a verify stage
  -- (as opposed to the bump stage or no saved stage at all).  When the nuke
  -- knob is on, the previous probe's `.lake/` is gone, so we must always re-run
  -- the bump (which writes `lake-manifest.json` via `lake update`).
  let resumingVerify :=
    if nukeLakeDirEnabled then false
    else
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
  if nukeLakeDirEnabled then
    emit .running s!"[{← nowUtcString}] Wiping .lake/ (HOPSCOTCH_DEBUG_NUKE_LAKEDIR)"
    nukeLakeDir paths
  -- Bump phase: apply the version then fetch. Skipped when resuming mid-verify.
  if !resumingVerify then
    let _ ← saveState paths config.resultsJsonPath <| buildRunningState base index commit (some bumpStep.stage)
    let bumpLogPath := State.logPath paths namePrefix commit bumpStep.stage
    emit .running s!"[{← nowUtcString}] Running {bumpStep.label}"
    let bumpOk ← bumpStep.run paths.projectDir bumpLogPath config.quiet
    emit (if bumpOk then .success else .failure)
      s!"[{← nowUtcString}] Finished {bumpStep.label} (log file: {bumpLogPath})"
    if !bumpOk then
      -- The build step hasn't run yet, so there is no build log to hand to detection.
      return .failure bumpStep.stage bumpLogPath none
  -- Verify phase: run each step in order, resuming from the saved point if applicable.
  let startAt :=
    match resumeStage? with
    | none => 0
    | some stage =>
        config.strategy.verify.findIdx? (·.stage == stage) |>.getD 0
  -- Track the build step's log specifically (it carries deprecation warnings),
  -- separately from whichever step ultimately fails.
  let mut buildLog : Option System.FilePath := none
  for step in config.strategy.verify.extract startAt config.strategy.verify.size do
    let _ ← saveState paths config.resultsJsonPath <| buildRunningState base index commit (some step.stage)
    let verifyLogPath := State.logPath paths namePrefix commit step.stage
    if step.stage == .build then buildLog := some verifyLogPath
    emit .running s!"[{← nowUtcString}] Running {step.label}"
    let stepOk ← step.run paths.projectDir verifyLogPath config.quiet
    emit (if stepOk then .success else .failure)
      s!"[{← nowUtcString}] Finished {step.label} (log file: {verifyLogPath})"
    if !stepOk then
      return .failure step.stage verifyLogPath buildLog
  return .success buildLog

/-- Run each verify step's preflight once before the search begins, so a misconfiguration
    (e.g. a missing `lake test` / `lake lint` driver) aborts immediately with a clear error
    instead of surfacing as a bogus failure boundary. -/
private def runVerifyPreflights (config : Config) (paths : Paths) : IO Unit := do
  for step in config.strategy.verify do
    step.preflight paths.projectDir config.quiet

/--
Drive the linear-mode state machine to completion or first failure.

Loads (or creates) the initial state, then iterates `runProbe` over the commit list
from `nextIndex` onward. Each successful probe calls `advanceAfterSuccess`; a failing
probe calls `buildFailureState` and returns immediately. The session is also returned
immediately when the stored status is already `fullySuccessful`.

On resume after a previously-failed commit, a git cleanliness check is performed
before advancing past that commit (unless `--allow-dirty-workspace` was passed).
-/
private def runAdvance (config : Config) (paths : Paths) (commits : Array String)
    (lowerBoundRef : Option String) (emit : ConsoleStyle → String → IO Unit) : IO RunResult := do
  let mut state ← loadInitialState paths config commits lowerBoundRef
  let resumedFailedCommit := state.status == .stopped
  let resumeIndex := state.nextIndex
  -- A resumed session retries the previously-failed commit; drop the detection
  -- results attached to that conclusion (recomputed when the run concludes again).
  if resumedFailedCommit then
    state := { state with proposedFixes := #[], deprecatedImports := #[], autoFixNotes := #[] }
  validateState state commits
  if state.status == .fullySuccessful then
    let summary ← writeSummary paths state
    Results.writeResults paths config.resultsJsonPath state
    return { exitCode := 0, summary := summary, summaryPath := paths.summaryPath }

  runVerifyPreflights config paths

  -- `lastSummary` captures the most recent summary written by `saveState`.
  -- It is always set before the loop exits with (status → fullySuccessful).
  let mut lastSummary : String := ""
  -- `lake build` log of the most recent successful probe, handed to detection on
  -- a green conclusion (it carries the toolchain's deprecation warnings, which
  -- fire at build time — not in a later `lake test` / `lake lint` log).
  let mut lastGreenLog : Option System.FilePath := none
  for index in [state.nextIndex:commits.size] do
    let commit := commits[index]!
    let probeResult ← runProbe config paths state index index commit emit
    match probeResult with
    | .failure stage logPath buildLog =>
        let stopped ← attachProposedFixes config paths
          (buildFailureState state index commit stage logPath) emit buildLog
        let (_, summary) ← saveState paths config.resultsJsonPath stopped
        copyCulpritLog paths logPath
        return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
    | .success buildLog =>
        lastGreenLog := buildLog <|> lastGreenLog
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
              -- Run detection here too (mirroring the build-failure path above), so a
              -- dirty-block stop preserves advisories/notes in results.json rather than
              -- overwriting them with the cleared resume state. The commit built, so the
              -- boundary-proposal branch finds nothing — only live-shim advisories surface.
              let stopped ← attachProposedFixes config paths
                (buildFailureState state index commit .gitCheck buildLogPath) emit buildLog
              let (_, summary) ← saveState paths config.resultsJsonPath stopped
              copyCulpritLog paths buildLogPath
              return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
        if resumedFailedCommit && index == resumeIndex then
          clearCulpritLogs paths
        let saved ← saveState paths config.resultsJsonPath <| advanceAfterSuccess state commits index commit
        state := saved.1
        lastSummary := saved.2
  -- Run detection on the completed state too: imports that resolve through live
  -- deprecation shims build today but break at the upstream shim cleanup.
  let concluded ← attachProposedFixes config paths state emit lastGreenLog
  unless concluded.deprecatedImports.isEmpty do
    let (_, summary) ← saveState paths config.resultsJsonPath concluded
    return { exitCode := 0, summary := summary, summaryPath := paths.summaryPath }
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
    (lowerBoundRef : Option String) (emit : ConsoleStyle → String → IO Unit) : IO RunResult := do
  let state ← loadInitialState paths config commits lowerBoundRef
  validateState state commits
  if state.status == .fullySuccessful then
    let summary ← writeSummary paths state
    Results.writeResults paths config.resultsJsonPath state
    return { exitCode := 0, summary := summary, summaryPath := paths.summaryPath }
  if state.status == .stopped then
    let summary ← writeSummary paths state
    Results.writeResults paths config.resultsJsonPath state
    return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }

  runVerifyPreflights config paths

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
                let resolved ← attachProposedFixes config paths
                  (buildBisectResolvedState state commits bisect failure) emit failure.buildLog
                let (_, summary) ← saveState paths config.resultsJsonPath resolved
                if let some lp := failure.logPath then copyCulpritLog paths lp
                return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
            | none =>
                throw <| IO.userError "stored bisect state is missing the failing probe result"
    let commit := commits[probeIndex]!
    -- After updating bisect bounds: loop with the next midpoint, or resolve if bounds are adjacent.
    let advanceOrResolve (base : PersistedState) (updatedBisect : BisectState) : IO RunResult := do
      match nextBisectProbeIndex? (bisectBounds updatedBisect) with
      | some nextIndex =>
          let saved ← saveState paths config.resultsJsonPath <| buildBisectSearchState base commits updatedBisect nextIndex
          loop saved.1
      | none =>
          match knownBadFailureResult? updatedBisect with
          | some failure =>
              let resolved ← attachProposedFixes config paths
                (buildBisectResolvedState base commits updatedBisect failure) emit failure.buildLog
              let (_, summary) ← saveState paths config.resultsJsonPath resolved
              if let some lp := failure.logPath then copyCulpritLog paths lp
              return { exitCode := 1, summary := summary, summaryPath := paths.summaryPath }
          | none =>
              throw <| IO.userError "bisect resolution is missing the failing probe result"
    let stepNum := bisect.probeResults.size
    let probeResult ← runProbe config paths state stepNum probeIndex commit emit
    match probeResult with
    | .success buildLog =>
        if !bisect.verifiedBad then
          if config.keepLastGood then
            let allPass ← attachProposedFixes config paths
              (buildBisectAllPassState state bisect commit) emit buildLog
            let (_, summary) ← saveState paths config.resultsJsonPath allPass
            return { exitCode := 0, summary, summaryPath := paths.summaryPath }
          else
            throw <| IO.userError
              s!"bisect mode requires a known failing endpoint, but the last commit succeeded during re-verification: {commit}\nHint: pass --keep-last-good to treat an all-passing range as a completed run with no culprit."
        advanceOrResolve state { bisect with
          knownGoodIndex := probeIndex
          probeResults := recordProbeResult bisect.probeResults {
            index := probeIndex
            commit := commit
            outcome := .success
          }
        }
    | .failure stage logPath buildLog =>
        advanceOrResolve state { bisect with
          knownBadIndex := probeIndex
          verifiedBad := true
          probeResults := recordProbeResult bisect.probeResults {
            index := probeIndex
            commit := commit
            outcome := .failure
            stage := some stage
            logPath := some logPath
            -- Preserve the build log so detection scans it (not the failing
            -- step's log) when this cached failure is later resolved.
            buildLog := buildLog
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
    (warn : String → IO Unit) : IO (Array String × Option String) := do
  match config.itemSource with
  | .file path => return (← ItemList.load path, none)
  | .range toRef fromRef gitUrl =>
      let resolvedUrl ←
        match gitUrl with
        | some url => pure url
        | none =>
            match ← config.strategy.defaultGitUrl paths.projectDir with
            | some url => pure url
            | none =>
                throw <| IO.userError
                  s!"no git URL found for '{config.strategy.scope}'; \
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
                warn s!"No pinned rev found for '{config.strategy.scope}'; \
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
      -- `resolvedFrom` is the known-good lower bound (excluded from `commits`); it is
      -- the natural baseline for diffing the dependency in automated fixes.
      return (commits, some resolvedFrom)

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
  let (commits, lowerBoundRef) ←
    match config.itemSource with
    | .file _ => resolveItems config paths (emit .running)
    | .range .. =>
        match ← State.load? paths with
        | some existingState => pure (existingState.items, existingState.lowerBoundRef)
        | none => resolveItems config paths (emit .running)

  ensureLakefile paths
  ensureDirs paths

  let result ← match config.runMode with
    | .linear => runAdvance config paths commits lowerBoundRef emit
    | .bisect => runBisect config paths commits lowerBoundRef emit
  -- Post-search lakefile/toolchain restoration.
  -- Restore the lakefile/toolchain to a well-defined endpoint once the search
  -- is done so that the caller can immediately reproduce the result with
  -- `lake build`.
  --
  -- The condition `config.runMode == .bisect || config.keepLastGood` is a
  -- deliberate no-op for the linear+default case: `runAdvance` stops as soon
  -- as the failing probe returns and does *not* modify the lakefile afterward,
  -- so it is already pinned to the first bad commit.  If `runAdvance` is ever
  -- changed to touch the lakefile after a failure, this assumption breaks and
  -- an explicit restore call must be added here for that case too.
  --
  -- `currentCommit` is set to the first bad commit by both `buildFailureState`
  -- (linear) and `buildBisectResolvedState` (bisect), so it is always the
  -- correct target for the default end-state.
  --
  -- `lastSuccessfulCommit` is `none` when the very first commit in the range
  -- fails (no good commit was ever observed).  In that case `--keep-last-good`
  -- has no commit to restore to and silently does nothing — the lakefile stays
  -- wherever the failed probe left it.
  if result.exitCode == 1 && (config.runMode == .bisect || config.keepLastGood) then
    if let some savedState ← State.load? paths then
      let targetCommit := if config.keepLastGood
        then savedState.lastSuccessfulCommit   -- none if first commit failed; skipped below
        else savedState.currentCommit          -- always the first bad commit
      if let some c := targetCommit then
        restoreTo config paths c emit
  return result

end Hopscotch.Runner
