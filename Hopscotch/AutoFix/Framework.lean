import Hopscotch.AutoFix.Migration
import Hopscotch.State
import Hopscotch.Util

/-!
# Automated fixes: framework

An extensible framework for automated fixes that recognize repairable build
failures in a bisected/scanned downstream project and propose mechanical
repairs. The concrete fixes live in sibling modules (e.g.
`Hopscotch.AutoFix.Mathlib.ModuleDeprecation`).

A run never modifies the downstream sources. After a bisect/scan concludes at a
failure boundary, the registered fixes' detection runs once against that
boundary commit; any resulting `ImportMigration`s are recorded as proposals (in
`PersistedState.proposedFixes`, surfaced via `summary.md` and `results.json`).
Applying them is the consumer's choice: `hopscotch fix apply` rewrites the
workspace, after which a re-run can search past the repaired breakage.
Multi-breakage windows are handled iteratively, one reproducible boundary (plus
its fix, when known) per run.

Detection can also produce advisories: migrations for imports that still build
(they resolve through a live deprecation shim) but will break when the
dependency deletes the shim. Advisories are recorded in
`PersistedState.deprecatedImports`, including on fully-successful runs, but are
never applied by `hopscotch fix apply`.

A fix contributes two functions:

- `detect`   — inspect the concluded run (and the dependency history) and propose
  migrations/advisories. Detection only: it does not touch the workspace.
- `apply`    — apply the recorded migrations to the workspace in one pass,
  idempotently, returning the project-relative files it changed. Used by
  `hopscotch fix apply`, never by a run.

The framework (`detectFixes`) stamps the owning fix's id on every record.
-/

namespace Hopscotch.AutoFix

open Hopscotch
open Hopscotch.State

/-! ## Framework types -/

/-- Inputs available to a fix hook. -/
structure FixContext where
  /-- Normalized downstream project root. -/
  projectDir : System.FilePath
  /-- Tool-owned path layout (state root, logs, …). -/
  paths : Paths
  /-- Dependency identity (the `require` name / strategy scope). -/
  dependencyName : String
  /-- The full ordered commit list of the run. -/
  items : Array String
  /-- The commit currently being probed. -/
  currentCommit : String
  /-- Known-good diff baseline, supplied by the runner: the range's `from` rev
      when the run came from a commit range, otherwise the oldest item. -/
  baseline : String
  /-- Path to the failing build log of the boundary commit, when available. -/
  buildLogPath : Option System.FilePath
  /-- Suppress console mirroring of fix progress. -/
  quiet : Bool

/-- Result of a fix's `detect` hook: proposed migrations (not yet applied),
    advisories for deprecated-but-still-working imports, and diagnostic notes
    for the console/log. -/
structure DetectResult where
  /-- Migrations that repair the failure boundary. -/
  migrations : Array ImportMigration := #[]
  /-- Migrations for imports that resolve through a live deprecation shim:
      they build today but break when the dependency deletes the shim. -/
  advisories : Array ImportMigration := #[]
  notes : Array String := #[]

/-- One automated fix in the registry. (Named `Fix` rather than `AutoFix` to avoid
    colliding with the enclosing `Hopscotch.AutoFix` namespace.) -/
structure Fix where
  /-- Stable identifier; the framework stamps it on every record the fix produces. -/
  id : String
  /-- One-line human description. -/
  description : String
  /-- Inspect the concluded failure boundary and propose migrations. Detection
      only — nothing is applied. Default: detects nothing. -/
  detect : FixContext → IO DetectResult := fun _ => pure {}
  /-- Apply the recorded migrations to the workspace in one pass (idempotent),
      walking the workspace a single time rather than once per migration. Returns
      the project-relative files it changed. Used by `hopscotch fix apply`, never
      by a run. -/
  apply : Paths → System.FilePath → Array ImportMigration → IO (Array String)
  /-- One-line human rendering of one of this fix's migrations. -/
  render : ImportMigration → String := ImportMigration.describe

/-! ## Orchestration -/

/-- Run every fix's detection against a concluded run, stamping each record with
    its owning fix's id (so a fix cannot mislabel one). Nothing is applied — the
    results are surfaced in `results.json` / `summary.md`, and the user opts in
    via `hopscotch fix apply`. Returns the aggregated `DetectResult`: migrations,
    advisories, and notes (e.g. "deleted with no replacement"), the latter
    prefixed with the owning fix's id. -/
def detectFixes (fixes : Array Fix) (ctx : FixContext)
    (emit : String → IO Unit) : IO DetectResult := do
  let mut out : DetectResult := {}
  for fix in fixes do
    let detected ← fix.detect ctx
    for note in detected.notes do
      emit s!"[auto-fix:{fix.id}] {note}"
      out := { out with notes := out.notes.push s!"[{fix.id}] {note}" }
    for proposal in detected.migrations do
      let m := { proposal with fixId := fix.id }
      out := { out with migrations := out.migrations.push m }
      emit s!"[auto-fix:{fix.id}] proposed fix: {fix.render m}"
    for advisory in detected.advisories do
      let m := { advisory with fixId := fix.id }
      out := { out with advisories := out.advisories.push m }
      emit s!"[auto-fix:{fix.id}] deprecated import: {fix.render m}"
  return out

/-! ## Backup store

Before its first rewrite of a downstream source file, `hopscotch fix apply`
stashes the original under `.lake/hopscotch/autofix-backups/`, mirroring the
file's project-relative path. The store is itself the record of what was
touched, so `hopscotch fix revert` needs no migration metadata. -/

/-- Root directory of the backup store. -/
def backupRoot (paths : Paths) : System.FilePath :=
  paths.stateRoot / "autofix-backups"

/-- Backup location for the project-relative `relPath`. -/
def backupPath (paths : Paths) (relPath : String) : System.FilePath :=
  backupRoot paths / relPath

/-- Stash `contents` as the backup for `relPath` unless one already exists — the
    first backup wins, since it holds the original. -/
def backupFileOnce (paths : Paths) (relPath : String) (contents : String) : IO Unit := do
  let backup := backupPath paths relPath
  unless ← backup.pathExists do
    -- Atomic: a torn backup write would leave a truncated original as the only
    -- recoverable copy, and `fix revert` would restore that corruption.
    writeFileAtomic backup contents

/-- Project-relative paths of every file in the backup store. -/
private partial def collectBackupFiles (dir : System.FilePath) (relPrefix : String)
    : IO (Array String) := do
  let mut acc : Array String := #[]
  unless ← dir.pathExists do return acc
  for entry in ← dir.readDir do
    let rel := if relPrefix.isEmpty then entry.fileName else s!"{relPrefix}/{entry.fileName}"
    if ← entry.path.isDir then
      acc := acc ++ (← collectBackupFiles entry.path rel)
    else
      acc := acc.push rel
  return acc

/-- Restore every file in the backup store into the workspace, undoing earlier
    rewrites by `hopscotch fix apply`. Returns the restored files. -/
def restoreAllBackups (paths : Paths) (projectDir : System.FilePath)
    : IO (Array String) := do
  let mut restored : Array String := #[]
  for rel in ← collectBackupFiles (backupRoot paths) "" do
    let target := projectDir / rel
    writeFileAtomic target (← IO.FS.readFile (backupPath paths rel))
    restored := restored.push rel
  return restored

end Hopscotch.AutoFix
