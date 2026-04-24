import Lean
import Lean.Data.Json.FromToJson
import Hopscotch.Util

namespace Hopscotch.State

open Hopscotch
open Lean

/--
High-level progress of one local stepping session.

- `running`: a probe is in progress or the next one is queued.
- `failed`: a probe step returned a non-zero exit code (or a git check blocked a
  dirty workspace). Terminal for bisect; resumable in linear mode after the user fixes
  the failing commit.
- `completed`: all commits passed (linear mode only). Terminal.
-/
inductive RunStatus where
  | running
  | failed
  | completed
  deriving Repr, Inhabited, BEq, DecidableEq, ToJson, FromJson

/-- The execution strategy for one persisted runner session. -/
inductive RunMode where
  | linear
  | bisect
  deriving Repr, Inhabited, BEq, DecidableEq, ToJson, FromJson

/--
The last or current within-probe step associated with a persisted run state.

Within one probe the steps run in order: `bump` → `build` (and any further verify
steps). `gitCheck` is a virtual stage used only when a resume after a previously-
failed commit is blocked because the worktree is dirty.

When `PersistedState.stage` is `none` the runner is between probes.
-/
inductive RunStage where
  | bump
  | build
  | gitCheck
  deriving Repr, Inhabited, BEq, DecidableEq, ToJson, FromJson

/-- The persisted outcome of one completed bisect probe. -/
inductive ProbeOutcome where
  | success
  | failure
  deriving Repr, Inhabited, BEq, DecidableEq, ToJson, FromJson

/-- Sparse cached result for one tested commit in bisect mode. -/
structure ProbeResult where
  index : Nat
  commit : String
  outcome : ProbeOutcome
  stage : Option RunStage := none
  logPath : Option System.FilePath := none
  deriving Repr, Inhabited, BEq, ToJson, FromJson

/--
Extra search-window metadata for automated bisect sessions.

Invariant (maintained by the state machine):
- `knownGoodIndex < knownBadIndex` while the search is running.
- `knownBadIndex == knownGoodIndex + 1` when the session reaches `Failed`,
  meaning `items[knownBadIndex]` is the first failing commit.
- `verifiedBad` becomes `true` after `items[knownBadIndex]` is probed and
  confirmed to fail; the binary search loop does not start until then.
- `baselineToolchain` is the contents of `lean-toolchain` at session start.
  It is restored before every fresh probe so that a strategy's bump step
  (which may rewrite the toolchain) does not bleed into the next probe.
-/
structure BisectState where
  knownGoodIndex : Nat
  knownBadIndex : Nat
  /-- `true` after the initial bad endpoint has been probed and confirmed to fail. -/
  verifiedBad : Bool := false
  baselineToolchain : String
  probeResults : Array ProbeResult := #[]
  deriving Repr, Inhabited, BEq, ToJson, FromJson

/-- On-disk state schema version; increment whenever `PersistedState` or its nested
    types change in a backward-incompatible way. A mismatch on resume is fatal. -/
def currentSchemaVersion : Nat := 7

/--
Minimal restartable state persisted under `.lake/hopscotch/state.json`.

This is the single source of truth for the runner's state machine. See
`Hopscotch.Runner.StateMachine` for the full state diagram and transition
semantics.
-/
structure PersistedState where
  schemaVersion : Nat := currentSchemaVersion
  projectDir : System.FilePath
  strategyScope : String
  items : Array String
  runMode : RunMode := .linear
  bisect : Option BisectState := none
  nextIndex : Nat
  currentCommit : Option String
  lastSuccessfulCommit : Option String
  status : RunStatus
  stage : Option RunStage
  lastLogPath : Option System.FilePath
  updatedAt : String
  deriving Repr, Inhabited, ToJson, FromJson

/-- Derived filesystem paths for the downstream checkout and tool-owned artifacts. -/
structure Paths where
  projectDir : System.FilePath
  stateRoot : System.FilePath
  statePath : System.FilePath
  summaryPath : System.FilePath
  resultsPath : System.FilePath
  logsDir : System.FilePath
  culpritLogsDir : System.FilePath
  deriving Repr

/-- Build the standard `.lake/hopscotch` path layout for a downstream project. -/
def mkPaths (projectDir : System.FilePath) : IO Paths := do
  let projectDir ← realPathNormalized projectDir
  let stateRoot := projectDir / ".lake" / "hopscotch"
  return {
    projectDir := projectDir
    stateRoot := stateRoot
    statePath := stateRoot / "state.json"
    summaryPath := stateRoot / "summary.md"
    resultsPath := stateRoot / "results.json"
    logsDir := stateRoot / "logs"
    culpritLogsDir := stateRoot / "logs" / "culprit"
  }

/-- Ensure the tool-owned directory tree exists before writing state or logs. -/
def ensureDirs (paths : Paths) : IO Unit := do
  IO.FS.createDirAll paths.logsDir

/-- Load persisted run state when resuming an existing stepping session. -/
def load? (paths : Paths) : IO (Option PersistedState) := do
  if !(← paths.statePath.pathExists) then
    return none
  return some (← readJsonFile (α := PersistedState) paths.statePath)

/-- Persist the current stepping state as formatted JSON. -/
def save (paths : Paths) (state : PersistedState) : IO Unit := do
  ensureDirs paths
  writeJsonFile paths.statePath state

/-- Compute the deterministic log path for one commit/stage pair.
    `namePrefix` is the log file name prefix produced by the run strategy (e.g. "3" in linear mode,
    "1-5" in bisect mode where step 1 probes position 5). -/
def logPath (paths : Paths) (namePrefix : String) (commit : String) (stage : RunStage) : System.FilePath :=
  let stageName :=
    match stage with
    | .bump => "bump"
    | .build => "build"
    | .gitCheck => "git-check"
  paths.logsDir / s!"{namePrefix}-{sanitizeForFileName (shortCommit commit)}-{stageName}.log"

end Hopscotch.State
