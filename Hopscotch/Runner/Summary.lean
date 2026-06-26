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
  | some .test => "Failure stage: lake test"
  | some .lint => "Failure stage: lake lint"
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

/-- Render the "Proposed fixes" section listing migrations detected at the failure
    boundary. Empty when none. The run never applies them — the note points the
    user at `hopscotch fix apply` for the opt-in. -/
private def autoFixLines (state : PersistedState) : List String :=
  if state.proposedFixes.isEmpty then []
  else
    let entries := state.proposedFixes.toList.map fun m =>
      s!"- [{m.fixId}] `{m.describe}`"
    -- A stopped bisect session is terminal, so "re-run" needs a `clean` first;
    -- linear sessions resume in place.
    let followUp := match state.runMode with
      | .linear => "`hopscotch fix apply` (or manually), then re-run to search past it:"
      | .bisect => "`hopscotch fix apply` (or manually), then `hopscotch clean` and re-run \
                    to search past it:"
    [ "", "## Proposed fixes",
      "The failure at this commit looks repairable. Apply these import rewrites with",
      followUp ]
      ++ entries

/-- Render detection notes: findings recorded without a fix, e.g. a module that
    was deleted upstream with no replacement shim. Empty when none. -/
private def autoFixNoteLines (state : PersistedState) : List String :=
  if state.autoFixNotes.isEmpty then []
  else
    [ "", "## Automated fix notes" ] ++ state.autoFixNotes.toList.map (s!"- {·}")

/-- Render the "Deprecated imports" advisory section: imports that resolve through
    live dependency deprecation shims. Empty when none. -/
private def deprecatedImportLines (state : PersistedState) : List String :=
  if state.deprecatedImports.isEmpty then []
  else
    let entries := state.deprecatedImports.toList.map fun m =>
      s!"- [{m.fixId}] `{m.describe}`"
    [ "", "## Deprecated imports",
      "These imports currently resolve through dependency deprecation shims; they keep",
      "building until the shims are deleted upstream. Consider migrating now:" ]
      ++ entries

/-- Render the persisted run state as the user-facing markdown summary body. -/
def summaryText (state : PersistedState) : String :=
  let header := "# Summary"
  let modeLine := s!"Mode: {runModeLabel state.runMode}"
  let statusLines :=
    match state.status with
    | .fullySuccessful =>
        match state.runMode with
        | .linear =>
            match state.lastSuccessfulCommit with
            | some commit => [s!"Status: fullySuccessful", modeLine, s!"Last successful commit: {commit}"]
            | none => [s!"Status: fullySuccessful", modeLine]
        | .bisect =>
            let last := state.lastSuccessfulCommit.getD "unknown"
            [ s!"Status: fullySuccessful", modeLine,
              "All commits passed — no culprit found.",
              s!"Last passing commit: {last}" ]
    | .stopped =>
        match state.runMode with
        | .linear =>
            [
              "Status: stopped",
              modeLine,
              s!"First failing commit: {state.currentCommit.getD "unknown"}",
              failureStageLine state.stage,
              s!"Culprit log directory: {state.projectDir / ".lake" / "hopscotch" / "logs" / "culprit"}"
            ]
        | .bisect =>
            [
              "Status: stopped",
              modeLine,
              s!"First failing commit: {state.currentCommit.getD "unknown"}",
              s!"Previous known good commit: {bisectKnownGoodCommit state}",
              failureStageLine state.stage,
              s!"Culprit log directory: {state.projectDir / ".lake" / "hopscotch" / "logs" / "culprit"}"
            ]
    | .running =>
        let stageLine :=
          match state.stage with
          | some .bump => "Stage: lake update"
          | some .build => "Stage: lake build"
          | some .test => "Stage: lake test"
          | some .lint => "Stage: lake lint"
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
  String.intercalate "\n" <|
    [header, ""] ++ statusLines ++ autoFixLines state ++ autoFixNoteLines state
      ++ deprecatedImportLines state ++ [""]

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
