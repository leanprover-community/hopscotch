import Lean
import Lean.Data.Json.FromToJson
import Hopscotch.State
import Hopscotch.Util

/-!
# Structured results output

Writes a curated, versioned `results.json` file alongside `summary.md` on every
state transition. This is the public, consumer-facing view of a run (in contrast
to `state.json`, which is the runner's own resumption snapshot).

The schema is documented by `ResultsJson` below and is independent of
`PersistedState.schemaVersion`: bumping one does not force bumping the other.
-/

namespace Hopscotch.Results

open Hopscotch
open Hopscotch.State
open Lean

/-- Public schema version for `results.json`. Bump on any
    backward-incompatible change to the fields below. -/
def resultsSchemaVersion : Nat := 2

/-- One automated fix *proposed* for the failure boundary, as surfaced in
    `results.json`. Never applied by the run; `hopscotch fix apply` opts in. -/
structure AutoFixJson where
  fixId               : String
  oldModule           : String
  newModules          : Array String
  /-- The source shim also defines declarations (compat aliases) the rewrite
      drops: the migration may be partial. See `ModuleMigration.shimHasDeclarations`. -/
  shimHasDeclarations : Bool
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Encode a proposed migration for `results.json`. Inverse of `toMigration`. -/
def AutoFixJson.ofMigration (m : AutoFix.ModuleMigration) : AutoFixJson :=
  { fixId := m.fixId, oldModule := m.oldModule, newModules := m.newModules
    shimHasDeclarations := m.shimHasDeclarations }

/-- Decode a `results.json` entry back into the runner's migration record, e.g.
    for application by `hopscotch fix`. Inverse of `ofMigration`. -/
def AutoFixJson.toMigration (a : AutoFixJson) : AutoFix.ModuleMigration :=
  { fixId := a.fixId, oldModule := a.oldModule, newModules := a.newModules
    shimHasDeclarations := a.shimHasDeclarations }

/-- One cached bisect probe outcome, as surfaced in `results.json`. -/
structure ProbeResultJson where
  index   : Nat
  commit  : String
  outcome : String
  stage   : Option String := none
  logPath : Option String := none
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Bisect-specific bounds and probe history. Present iff `mode == "bisect"`. -/
structure BisectJson where
  knownGoodIndex     : Nat
  knownBadIndex      : Nat
  knownGoodCommit    : Option String
  knownBadCommit     : Option String
  knownGoodWasProbed : Bool
  verifiedBad        : Bool
  probeResults       : Array ProbeResultJson
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Curated public view of one run's results. -/
structure ResultsJson where
  schemaVersion        : Nat
  status               : String
  mode                 : String
  exitCode             : Nat
  strategyScope         : String
  projectDir           : String
  items                : Array String
  firstFailingCommit   : Option String
  lastSuccessfulCommit : Option String
  currentCommit        : Option String
  failureStage         : Option String
  nextIndex            : Nat
  culpritLogPath       : Option String
  lastLogPath          : Option String
  logsDir              : String
  summaryPath          : String
  bisect               : Option BisectJson
  /-- Automated fixes proposed for the failure boundary (empty when none, or when
      the run did not stop). Apply with `hopscotch fix apply`. An entry with empty
      `newModules` means: remove the import. -/
  proposedFixes        : Array AutoFixJson
  /-- Advisories: downstream imports that resolve through a live deprecation shim
      in the dependency — they build today but break when the shim is deleted
      upstream. Informational; not applied by `hopscotch fix apply`. -/
  deprecatedImports    : Array AutoFixJson
  /-- Notes from automated-fix detection, prefixed with the owning fix's id —
      e.g. a module that was deleted upstream with no replacement shim, so no
      fix could be proposed. -/
  detectionNotes       : Array String
  updatedAt            : String
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Public label for a `RunStatus`. -/
private def statusLabel : RunStatus → String
  | .running   => "running"
  | .stopped    => "stopped"
  | .fullySuccessful => "fullySuccessful"

/-- Public label for a `RunMode`. -/
private def modeLabel : RunMode → String
  | .linear => "linear"
  | .bisect => "bisect"

/-- Public label for a `RunStage`. Used for `failureStage` in the JSON. -/
private def stageLabel : RunStage → String
  | .bump     => "lake update"
  | .build    => "lake build"
  | .gitCheck => "git cleanliness check"

/-- Public label for a `ProbeOutcome`. -/
private def outcomeLabel : ProbeOutcome → String
  | .success => "success"
  | .failure => "failure"

private def toProbeResultJson (r : ProbeResult) : ProbeResultJson :=
  { index   := r.index
    commit  := r.commit
    outcome := outcomeLabel r.outcome
    stage   := r.stage.map stageLabel
    logPath := r.logPath.map (·.toString) }

/-- Build the `BisectJson` view for one persisted bisect state. -/
private def toBisectJson (items : Array String) (b : BisectState) : BisectJson :=
  let knownGoodWasProbed :=
    b.probeResults.any fun r => r.index == b.knownGoodIndex && r.outcome == .success
  { knownGoodIndex     := b.knownGoodIndex
    knownBadIndex      := b.knownBadIndex
    knownGoodCommit    := items[b.knownGoodIndex]?
    knownBadCommit     := items[b.knownBadIndex]?
    knownGoodWasProbed := knownGoodWasProbed
    verifiedBad        := b.verifiedBad
    probeResults       := b.probeResults.map toProbeResultJson }

/-- Pure derivation of the `results.json` payload from the persisted state. -/
def fromState (paths : Paths) (state : PersistedState) : ResultsJson :=
  let exitCode : Nat := match state.status with
    | .stopped => 1
    | _       => 0
  let firstFailingCommit : Option String := match state.status with
    | .stopped => state.currentCommit
    | _       => none
  let failureStage : Option String := match state.status with
    | .stopped => state.stage.map stageLabel
    | _       => none
  let culpritLogPath : Option String := match state.status, state.lastLogPath with
    | .stopped, some lp =>
        let fileName := lp.fileName.getD lp.toString
        some ((paths.culpritLogsDir / fileName).toString)
    | _, _ => none
  { schemaVersion        := resultsSchemaVersion
    status               := statusLabel state.status
    mode                 := modeLabel state.runMode
    exitCode             := exitCode
    strategyScope         := state.strategyScope
    projectDir           := state.projectDir.toString
    items                := state.items
    firstFailingCommit   := firstFailingCommit
    lastSuccessfulCommit := state.lastSuccessfulCommit
    currentCommit        := state.currentCommit
    failureStage         := failureStage
    nextIndex            := state.nextIndex
    culpritLogPath       := culpritLogPath
    lastLogPath          := state.lastLogPath.map (·.toString)
    logsDir              := paths.logsDir.toString
    summaryPath          := paths.summaryPath.toString
    bisect               := state.bisect.map (toBisectJson state.items)
    proposedFixes        := state.proposedFixes.map AutoFixJson.ofMigration
    deprecatedImports    := state.deprecatedImports.map AutoFixJson.ofMigration
    detectionNotes       := state.autoFixNotes
    updatedAt            := state.updatedAt }

/-- Write `results.json` for the current state to the internal path, and also
    to `extraPath` if one was provided via `--results-json`. Both writes are
    atomic (temp-file + rename) via `Hopscotch.writeJsonFile`. -/
def writeResults (paths : Paths) (extraPath : Option System.FilePath)
    (state : PersistedState) : IO Unit := do
  let payload := fromState paths state
  writeJsonFile paths.resultsPath payload
  match extraPath with
  | some p => writeJsonFile p payload
  | none   => pure ()

end Hopscotch.Results
