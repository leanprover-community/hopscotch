import Hopscotch.Runner.Types
import Hopscotch.Runner.Summary
import Hopscotch.State
import Hopscotch.Util

/-!
# Runner State Machine

This module implements state transitions for Hopscotch's restartable execution engine.
All meaningful state lives in `PersistedState`, serialized to `.lake/hopscotch/state.json`
after every significant step, so that any interruption (crash, Ctrl-C, timeout) can be
cleanly resumed without re-running work that already succeeded.

## Key fields

| Field           | Meaning                                                                |
|-----------------|------------------------------------------------------------------------|
| `status`        | Coarse phase: `running`, `failed`, or `completed`                      |
| `runMode`       | Execution strategy: `linear` or `bisect` (binary search)    |
| `stage`         | Within-probe step: `bump`, `build`, or `gitCheck`; `none` = between   |
| `nextIndex`     | Index into `items` of the commit currently being (or next to be) probed |
| `currentCommit` | Commit SHA / label at `nextIndex`                                      |
| `bisect`        | Extra search-window metadata; `none` in linear mode                  |

## Linear mode

The runner steps linearly through the commit list, probing each one until a failure is
found or all commits pass.

```
  Running(nextIndex=i, stage=none)
       |
       v  bump step (rewrite lakefile / toolchain, run lake update)
  Running(nextIndex=i, stage=bump)
       | fail -----------------------------------------> Failed(stage=bump)
       | ok
       v  verify steps (lake build, …)
  Running(nextIndex=i, stage=build)
       | fail -----------------------------------------> Failed(stage=build)
       | ok
       v  [git check — only when resuming a previously-failed commit]
       | dirty ----------------------------------------> Failed(stage=gitCheck)
       | clean / unavailable
       v
  i+1 == len --------------------------------------------> Completed
  i+1 <  len ---> Running(nextIndex=i+1, stage=none) --.
                  ^                                     |
                  '-------------------------------------'
```

`stage` is written to disk before each step so the runner knows exactly where to
pick up after a restart.

## Bisect mode

The runner binary-searches the commit list to pinpoint the first failing commit.
The search window `[knownGoodIndex, knownBadIndex]` narrows with each probe until
the two indices are adjacent, at which point the exact boundary is known.

```
  Initial: Running(knownGoodIndex=0, knownBadIndex=N-1, verifiedBad=false)
       |
       v  probe commits[N-1]  <-- must fail to confirm the supplied bad endpoint
       | success (--keep-last-good) -----> Completed  (all-pass; no culprit)
       | success (default)          -----> error: "bad endpoint passed"
       | failure
       v  verifiedBad=true
       .
  .------------------------------------------------------------.
  |  BINARY SEARCH LOOP                                        |
  |                                                            |
  |  mid = knownGoodIndex + (knownBadIndex - knownGoodIndex)/2 |
  |                                                            |
  |  knownBadIndex == knownGoodIndex + 1 (adjacent)            |
  |    '----------------------------------------------------> Failed
  |                                                            |  (boundary known)
  |  probe commits[mid]:                                       |
  |    success  -->  knownGoodIndex = mid  -->  loop           |
  |    failure  -->  knownBadIndex  = mid  -->  loop           |
  '------------------------------------------------------------'
```

The terminal `Failed` state satisfies `knownBadIndex == knownGoodIndex + 1`:
`commits[knownBadIndex]` is the first failing commit and
`commits[knownGoodIndex]` is the last passing one.

## Resumability

`saveState` atomically updates the JSON state and rewrites the markdown summary on
every transition. On the next invocation `loadInitialState` reads that snapshot and
`validateState` checks it is internally consistent before handing it to the loop:

- `stage` is set: the probe for `currentCommit` was interrupted mid-step; skip
  completed steps and restart from `stage`.
- `stage` is `none`, `status` is `running`: between probes; start the next commit.
- `status` is `failed`: already resolved; return the stored summary immediately.
- `status` is `completed`: linear mode finished all commits; return immediately.
-/

namespace Hopscotch.Runner

open Hopscotch
open Hopscotch.State

/-- Result of attempting the post-fix git cleanliness verification. -/
inductive GitCheckResult where
  | clean
  | dirty
  | unavailable
  deriving Repr, BEq

/-- Best-effort git cleanliness check for the downstream project root. -/
def checkGitWorktree (projectDir : System.FilePath) : IO GitCheckResult := do
  try
    let output ← IO.Process.output {
      cmd := "git"
      args := #["status", "--porcelain"]
      cwd := projectDir
    }
    if output.exitCode != 0 then
      return .unavailable
    return if output.stdout.trimAscii.isEmpty then .clean else .dirty
  catch _ =>
    return .unavailable

/-- Reject downstream projects that expose neither a `lakefile.toml` nor a `lakefile.lean`. -/
def ensureLakefile (paths : Paths) : IO Unit := do
  let hasToml ← (paths.projectDir / "lakefile.toml").pathExists
  let hasLean ← (paths.projectDir / "lakefile.lean").pathExists
  unless hasToml || hasLean do
    throw <| IO.userError
      s!"missing lakefile.toml or lakefile.lean in project directory: {paths.projectDir}"

/-- Restore the session-start toolchain before a fresh bisect probe begins. -/
def restoreBisectBaselineToolchain (paths : Paths) (state : PersistedState) : IO Unit := do
  match state.bisect with
  | some bisect =>
      IO.FS.writeFile (paths.projectDir / "lean-toolchain") bisect.baselineToolchain
  | none =>
      throw <| IO.userError "stored bisect state is missing bisect metadata"

/-- Replace any previous cached probe result for the same commit index. -/
def recordProbeResult (results : Array ProbeResult) (result : ProbeResult) : Array ProbeResult :=
  (results.filter fun entry => entry.index != result.index) |>.push result

/-- Resolve the cached failing result for the current known-bad bisect endpoint. -/
def knownBadFailureResult? (bisect : BisectState) : Option ProbeResult :=
  bisect.probeResults.findRev? fun result =>
    result.index == bisect.knownBadIndex && result.outcome == .failure

/-- Persist state after refreshing its `updatedAt` timestamp and rewriting the summary.
    Returns the updated state (with new timestamp) and the rendered summary text,
    so callers can return the summary without a redundant second write. -/
def saveState (paths : Paths) (state : PersistedState) : IO (PersistedState × String) := do
  let state := { state with updatedAt := ← nowUtcString }
  State.save paths state
  let summary ← writeSummary paths state
  pure (state, summary)

/-!
## State transition helpers

Each function below constructs the next `PersistedState` for one edge in the
linear or bisect state diagrams above. They are pure computations; the caller
is responsible for persisting the result via `saveState`.
-/

/--
Transition to `Running` at the start of a probe step.

Called once before the bump step (with `stage = some .bump`) and once before
each verify step (with `stage = some .build`, etc.), so that a restart always
has a concrete `stage` to resume from.
-/
def buildRunningState (base : PersistedState) (index : Nat) (commit : String)
    (stage : Option RunStage) : PersistedState :=
  { base with
    nextIndex := index
    currentCommit := some commit
    status := .running
    stage := stage
    lastLogPath := none
  }

/--
Transition to `Failed` after a probe step returns a non-zero exit code.

Captures the failing `stage` and the path to its log file so the summary and CLI
can report exactly where the run stopped.
-/
def buildFailureState (base : PersistedState) (index : Nat) (commit : String)
    (stage : RunStage) (logPath : System.FilePath) : PersistedState :=
  { base with
    nextIndex := index
    currentCommit := some commit
    status := .failed
    stage := some stage
    lastLogPath := some logPath
  }

/--
Transition to `Running(nextIndex=i+1)` or `Completed` after a successful probe.

When `i+1` reaches the end of the commit list the status becomes `Completed`;
otherwise the cursor advances to the next commit and the stage is cleared.
-/
def advanceAfterSuccess (base : PersistedState) (commits : Array String) (index : Nat)
    (commit : String) : PersistedState :=
  let nextIndex := index + 1
  let nextCommit := commits[nextIndex]?
  { base with
    nextIndex := nextIndex
    currentCommit := nextCommit
    lastSuccessfulCommit := some commit
    status := if nextIndex == commits.size then .completed else .running
    stage := none
    lastLogPath := none
  }

/--
Transition to `Running` with updated bisect bounds and the next midpoint selected.

Called after each successful probe (advancing `knownGoodIndex`) or failing probe
(narrowing `knownBadIndex`), before the loop picks the next midpoint.
-/
def buildBisectSearchState (base : PersistedState) (commits : Array String)
    (bisect : BisectState) (nextIndex : Nat) : PersistedState :=
  let nextCommit := commits[nextIndex]!
  { base with
    runMode := .bisect
    bisect := some bisect
    nextIndex := nextIndex
    currentCommit := some nextCommit
    lastSuccessfulCommit := commits[bisect.knownGoodIndex]?
    status := .running
    stage := none
    lastLogPath := none
  }

/--
Transition to `Failed` once the bisect search window collapses to an exact boundary.

Reached when `knownBadIndex == knownGoodIndex + 1`, meaning no midpoint remains to
probe. The stored state records `commits[knownBadIndex]` as the first failing commit,
together with the stage and log path from that commit's cached `ProbeResult`.
-/
def buildBisectResolvedState (base : PersistedState) (commits : Array String)
    (bisect : BisectState) (failure : ProbeResult) : PersistedState :=
  let badIndex := bisect.knownBadIndex
  let badCommit := commits[badIndex]!
  { base with
    runMode := .bisect
    bisect := some bisect
    nextIndex := badIndex
    currentCommit := some badCommit
    lastSuccessfulCommit := commits[bisect.knownGoodIndex]?
    status := .failed
    stage := failure.stage
    lastLogPath := failure.logPath
  }

/--
Transition to `Completed` when the bisect bad endpoint passes with `--keep-last-good`.

The probe for `topCommit` (the upper end of the range) succeeded, so all commits in
the range are good and there is no culprit. The bisect bounds are preserved for
reference but the status reflects the all-pass outcome.
-/
def buildBisectAllPassState (base : PersistedState) (bisect : BisectState)
    (topCommit : String) : PersistedState :=
  { base with
    runMode              := .bisect
    bisect               := some bisect
    nextIndex            := bisect.knownBadIndex
    currentCommit        := none
    lastSuccessfulCommit := some topCommit
    status               := .completed
    stage                := none
    lastLogPath          := none
  }

/-- Create the initial persisted state for a fresh linear-mode session. -/
private def mkInitialAdvanceState (paths : Paths) (strategyName : String)
    (commits : Array String) : IO PersistedState := do
  let updatedAt ← nowUtcString
  return {
    projectDir := paths.projectDir
    strategyName := strategyName
    items := commits
    runMode := .linear
    nextIndex := 0
    currentCommit := commits[0]?
    lastSuccessfulCommit := none
    status := .running
    stage := none
    lastLogPath := none
    updatedAt := updatedAt
  }

/-- Create the initial persisted state for a fresh bisect session. -/
private def mkInitialBisectState (paths : Paths) (strategyName : String)
    (commits : Array String) : IO PersistedState := do
  if commits.size < 2 then
    throw <| IO.userError "bisect mode requires at least 2 commits"
  let updatedAt ← nowUtcString
  let baselineToolchain ← IO.FS.readFile (paths.projectDir / "lean-toolchain")
  let badIndex := commits.size - 1
  let bisect : BisectState := {
    knownGoodIndex := 0
    knownBadIndex := badIndex
    verifiedBad := false
    baselineToolchain := baselineToolchain
  }
  return {
    projectDir := paths.projectDir
    strategyName := strategyName
    items := commits
    runMode := .bisect
    bisect := some bisect
    nextIndex := badIndex
    currentCommit := commits[badIndex]?
    -- No commit has been verified yet; `lastSuccessfulCommit` is updated
    -- to `commits[bisect.knownGoodIndex]` as the search advances.
    lastSuccessfulCommit := none
    status := .running
    stage := none
    lastLogPath := none
    updatedAt := updatedAt
  }

/--
Load persisted state for this checkout or create the initial state for a fresh run.

This also enforces that resuming must use the exact same normalized commit list,
project directory, dependency name, state schema, and execution mode as the
original run.
-/
def loadInitialState (paths : Paths) (config : Config)
    (commits : Array String) : IO PersistedState := do
  match ← State.load? paths with
  | none =>
      match config.runMode with
      | .linear => mkInitialAdvanceState paths config.strategy.name commits
      | .bisect => mkInitialBisectState paths config.strategy.name commits
  | some state =>
      if state.schemaVersion != currentSchemaVersion then
        throw <| IO.userError
          "stored state schema is incompatible; delete .lake/hopscotch/ to start over"
      if state.items != commits then
        throw <| IO.userError
          "commit list changed since the last run; delete .lake/hopscotch/ to start over"
      if state.projectDir != paths.projectDir then
        throw <| IO.userError
          "stored project directory does not match this checkout; delete .lake/hopscotch/ to start over"
      if state.strategyName != config.strategy.name then
        throw <| IO.userError
          "strategy changed since the last run; delete .lake/hopscotch/ to start over"
      if state.runMode != config.runMode then
        throw <| IO.userError
          "run mode changed since the last run; delete .lake/hopscotch/ to start over"
      return state

/-!
## Invariant validation

`validateState` is called once at session start, after `loadInitialState`, to confirm
that the persisted snapshot satisfies the structural invariants implied by the state
diagrams above. This guards against file corruption, manual edits, or schema drift
that the schema-version check alone cannot catch.
-/

/--
Assert the structural invariants of a persisted linear-mode state.

Checks:
- Commit list is non-empty and `bisect` is absent.
- `nextIndex` is in range.
- `currentCommit` matches `commits[nextIndex]` whenever the status is `running` or `failed`.
-/
private def validateLinearState (state : PersistedState) (commits : Array String) : IO Unit := do
  if commits.isEmpty then
    throw <| IO.userError "stored linear state has an empty commit list"
  if state.bisect.isSome then
    throw <| IO.userError "stored linear state unexpectedly contains bisect data"
  if state.nextIndex > commits.size then
    throw <| IO.userError "stored state has an invalid nextIndex"
  match commits[state.nextIndex]? with
  | none =>
      if state.status != .completed then
        throw <| IO.userError "stored state points past the end of the commit list"
  | some commit =>
      if state.status == .failed || state.status == .running then
        if state.currentCommit != some commit then
          throw <| IO.userError "stored currentCommit does not match nextIndex"

/--
Assert the structural invariants of a persisted bisect-mode state.

Checks all three reachable statuses against the bisect diagram:
- `failed`: bounds must be exactly adjacent (`knownBadIndex == knownGoodIndex + 1`)
  and `nextIndex`/`currentCommit` must point at `knownBadIndex`.
- `running`: `nextIndex` must equal the expected probe index, which is determined by
  the stage of the loop:
    - `stage` is set → mid-step resume; `nextIndex` was already written.
    - `verifiedBad` is false → still verifying the bad endpoint; expect `knownBadIndex`.
    - Otherwise → normal midpoint; expect `nextBisectProbeIndex?`.
- `completed`: never valid in bisect mode (the loop always ends in `failed`).
-/
private def validateBisectState (state : PersistedState) (commits : Array String) : IO Unit := do
  let some bisect := state.bisect
    | throw <| IO.userError "stored bisect state is missing bisect metadata"
  if bisect.baselineToolchain.isEmpty then
    throw <| IO.userError "stored bisect baseline toolchain is empty"
  if commits.size < 2 then
    throw <| IO.userError "stored bisect state requires at least 2 commits"
  if bisect.knownGoodIndex >= commits.size || bisect.knownBadIndex >= commits.size then
    throw <| IO.userError "stored bisect bounds point outside the commit list"
  if bisect.knownGoodIndex >= bisect.knownBadIndex then
    throw <| IO.userError "stored bisect bounds are invalid"
  let bounds := bisectBounds bisect
  match state.status with
  | .completed =>
      if state.lastSuccessfulCommit.isNone then
        throw <| IO.userError "stored bisect all-pass state is missing lastSuccessfulCommit"
  | .failed =>
      if bisect.knownBadIndex != bisect.knownGoodIndex + 1 then
        throw <| IO.userError "stored bisect failure is missing an exact boundary"
      if state.nextIndex != bisect.knownBadIndex then
        throw <| IO.userError "stored bisect failure nextIndex does not match knownBadIndex"
      if state.currentCommit != commits[bisect.knownBadIndex]? then
        throw <| IO.userError "stored bisect failure currentCommit does not match knownBadIndex"
  | .running =>
      let expectedIndex ←
        if state.stage.isSome then
          pure state.nextIndex          -- resuming mid-stage
        else if !bisect.verifiedBad then
          pure bisect.knownBadIndex     -- bad endpoint not yet confirmed
        else
          match nextBisectProbeIndex? bounds with
          | some index => pure index    -- normal bisect midpoint
          | none =>
              throw <| IO.userError
                "stored bisect state is running even though the boundary is already resolved"
      if expectedIndex >= commits.size then
        throw <| IO.userError "stored bisect nextIndex points past the commit list"
      if state.nextIndex != expectedIndex then
        throw <| IO.userError "stored bisect nextIndex does not match the expected probe"
      if state.currentCommit != commits[expectedIndex]? then
        throw <| IO.userError "stored bisect currentCommit does not match nextIndex"

/-- Dispatch to the appropriate mode-specific validator. -/
def validateState (state : PersistedState) (commits : Array String) : IO Unit := do
  match state.runMode with
  | .linear => validateLinearState state commits
  | .bisect => validateBisectState state commits

end Hopscotch.Runner
