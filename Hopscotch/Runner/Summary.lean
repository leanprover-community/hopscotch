import Hopscotch.Runner.Types
import Hopscotch.State
import Hopscotch.Util

namespace Hopscotch.Runner

open Hopscotch
open Hopscotch.State

/-- Render the failure-stage line shown in the persisted markdown summary. -/
private def failureStageLine (stage : Option RunStage) : String :=
  match stage with
  | some .bump => "Failure stage: lake update"
  | some .build => "Failure stage: lake build"
  | some .gitCheck => "Failure stage: git cleanliness check"
  | none => "Failure stage: unknown"

/-- Resolve the known-good bisect commit label for summary rendering.
    When the good endpoint was never actually probed (it was assumed good because
    it is the first item in the range), append a caveat so the user knows. -/
private def bisectKnownGoodCommit (state : PersistedState) : String :=
  match state.bisect with
  | some bisect =>
      let commit := (state.items[bisect.knownGoodIndex]?).getD "unknown"
      let wasProbed := bisect.probeResults.any fun r =>
        r.index == bisect.knownGoodIndex && r.outcome == .success
      if wasProbed then commit else s!"{commit} (assumed, not verified)"
  | none => state.lastSuccessfulCommit.getD "unknown"

/-- Resolve the known-bad bisect commit label for summary rendering. -/
private def bisectKnownBadCommit (state : PersistedState) : String :=
  match state.bisect with
  | some bisect => (state.items[bisect.knownBadIndex]?).getD "unknown"
  | none => "unknown"

/-- Render the persisted run state as the user-facing markdown summary body. -/
def summaryText (state : PersistedState) : String :=
  let header := "# Summary"
  let modeLine := s!"Mode: {runModeLabel state.runMode}"
  let statusLines :=
    match state.status with
    | .completed =>
        match state.lastSuccessfulCommit with
        | some commit => [s!"Status: completed", modeLine, s!"Last successful commit: {commit}"]
        | none => [s!"Status: completed", modeLine]
    | .failed =>
        match state.runMode with
        | .linear =>
            [
              "Status: failed",
              modeLine,
              s!"First failing commit: {state.currentCommit.getD "unknown"}",
              failureStageLine state.stage,
              s!"Log file: {state.lastLogPath.getD "unknown"}"
            ]
        | .bisect =>
            [
              "Status: failed",
              modeLine,
              s!"First failing commit: {state.currentCommit.getD "unknown"}",
              s!"Previous known good commit: {bisectKnownGoodCommit state}",
              failureStageLine state.stage,
              s!"Log file: {state.lastLogPath.getD "unknown"}"
            ]
    | .running =>
        let stageLine :=
          match state.stage with
          | some .bump => "Stage: lake update"
          | some .build => "Stage: lake build"
          | some .gitCheck => "Stage: git cleanliness check"
          | none => "Stage: pending"
        match state.runMode with
        | .linear =>
            [
              "Status: running",
              modeLine,
              s!"Next commit: {state.currentCommit.getD "unknown"}",
              stageLine
            ]
        | .bisect =>
            [
              "Status: running",
              modeLine,
              s!"Known good commit: {bisectKnownGoodCommit state}",
              s!"Known bad commit: {bisectKnownBadCommit state}",
              s!"Next probe commit: {state.currentCommit.getD "unknown"}",
              stageLine
            ]
  String.intercalate "\n" <| [header, ""] ++ statusLines ++ [""]

/-- Apply terminal colors to the plain-text summary shown directly to users. -/
def renderSummaryForTerminal (mode : ColorMode) (summary : String) : String :=
  colorize mode .info summary

/-- Write the markdown summary for the current persisted state. -/
def writeSummary (paths : Paths) (state : PersistedState) : IO String := do
  ensureDirs paths
  let summary := summaryText state
  IO.FS.writeFile paths.summaryPath summary
  return summary

end Hopscotch.Runner
