import Hopscotch.State
import Hopscotch.Util

namespace Hopscotch.Runner

open Hopscotch
open Hopscotch.State

/-- How the item list is supplied to one `hopscotch` execution. -/
inductive ItemSource where
  /-- Load items from a plain-text file (one entry per line). -/
  | file (path : System.FilePath)
  /-- Fetch commits from GitHub between `fromRef` (exclusive) and `toRef` (inclusive).
      `toRef` defaults to the tip of `main` or `master` when `none`.
      `fromRef` defaults to the downstream lakefile's pinned rev when `none`.
      `gitUrl` defaults to the `git` field in the downstream's `[[require]]` block when `none`. -/
  | range (toRef : Option String := none) (fromRef : Option String := none) (gitUrl : Option String := none)
  deriving Repr

/-- One executable step in the probe pipeline (bump phase or verify phase). -/
structure ProbeStep where
  stage : RunStage
  label : String
  run : (projectDir logPath : System.FilePath) → (quiet : Bool) → IO Bool

/-- Strategy for applying a version and verifying the result.
    Each probe runs `mkBump` first; on success, each step in `verify` runs in order.
    A failure at any step aborts the probe immediately. -/
structure RunStrategy where
  /-- Identity string used for resume validation and state persistence. Should roughly correspond to 'what is being bumped' -/
  scope : String
  /-- Construct the bump step for a specific version. Called once per probe. -/
  mkBump : String → ProbeStep
  /-- Ordered steps to verify the project after bumping. -/
  verify : Array ProbeStep
  /-- Default lower-bound ref for range-mode commit resolution. -/
  defaultFromRef : System.FilePath → IO (Option String) := fun _ => pure none
  /-- Default git URL for range-mode commit resolution. -/
  defaultGitUrl : System.FilePath → IO (Option String) := fun _ => pure none
  /-- Compute the log file name prefix for one probe.
      `stepNum` is the zero-based count of probes completed so far;
      `posIndex` is the item's position in the search list.
      Default: single number when they coincide (linear), "{N}-{M}" when they differ (bisect). -/
  logPrefix : (stepNum posIndex : Nat) → String :=
    fun stepNum posIndex =>
      if stepNum == posIndex then toString posIndex
      else s!"{stepNum}-{posIndex}"

/-- User-facing configuration for one `hopscotch` execution. -/
structure Config where
  itemSource : ItemSource
  projectDir : System.FilePath := "."
  runMode : RunMode := .linear
  quiet : Bool := false
  allowDirtyWorkspace : Bool := false
  /-- When true, restore the lakefile/toolchain to the last *passing* commit
      after the search completes, rather than the first *failing* one.
      Has no effect when no commit passed (i.e. the very first commit failed),
      because there is no known-good commit to restore to. -/
  keepLastGood : Bool := false
  /-- Optional additional path to mirror `.lake/hopscotch/results.json` to
      after every state transition. The internal copy is always written. -/
  resultsJsonPath : Option System.FilePath := none
  strategy : RunStrategy

/-- Final outcome returned to the CLI after a run or resume attempt. -/
structure RunResult where
  exitCode : Nat
  summary : String
  summaryPath : System.FilePath
  deriving Repr

/-- Pure view of the current bisect search window. -/
structure BisectBounds where
  knownGoodIndex : Nat
  knownBadIndex : Nat
  deriving Repr, Inhabited, BEq

/-- Render the CLI/internal label for one run mode. -/
def runModeLabel (mode : RunMode) : String :=
  match mode with
  | .linear => "linear"
  | .bisect => "bisect"

/-- Return the deterministic lower midpoint for the current bisect window. -/
def nextBisectProbeIndex? (bounds : BisectBounds) : Option Nat :=
  if bounds.knownBadIndex <= bounds.knownGoodIndex + 1 then
    none
  else
    some <| bounds.knownGoodIndex + (bounds.knownBadIndex - bounds.knownGoodIndex) / 2

/-- Update the bisect window after one probe result. -/
def advanceBisectBounds (bounds : BisectBounds) (probeIndex : Nat)
    (outcome : ProbeOutcome) : BisectBounds :=
  match outcome with
  | .success => { bounds with knownGoodIndex := probeIndex }
  | .failure => { bounds with knownBadIndex := probeIndex }

/-- Extract the current search window from a persisted bisect state. -/
def bisectBounds (b : BisectState) : BisectBounds :=
  { knownGoodIndex := b.knownGoodIndex, knownBadIndex := b.knownBadIndex }

end Hopscotch.Runner
