import Hopscotch.AutoFix
import Hopscotch.Results
import Hopscotch.State
import Hopscotch.Util

/-!
# `hopscotch fix` — apply the fixes a run proposed

A run that stops at a repairable failure boundary records the proposed fixes in
`results.json` (`proposedFixes`) without touching the workspace. That list is a
portable, declarative encoding of the repair: `import <oldModule>` should be
rewritten to `<newModules>`. This module is the consumer's opt-in: `apply`
performs the rewrites (backing originals up), `list` inspects them, `revert`
restores the backups — including on a teammate's checkout via
`--from <their results.json>`.
-/

namespace Hopscotch.FixCommand

open Hopscotch
open Hopscotch.State
open Hopscotch.AutoFix

/-- What `hopscotch fix` should do with the recorded migrations. -/
inductive Action where
  | apply
  | revert
  | list
  deriving Repr, BEq

/-- Parsed configuration for one `hopscotch fix` invocation. -/
structure Config where
  action : Action
  projectDir : System.FilePath := "."
  /-- Optional results.json to read migrations from; defaults to the project's own. -/
  fromPath : Option System.FilePath := none
  /-- Whether `apply` also performs the deprecated-import advisory migrations (the
      green-bump hygiene pass). On by default — `fix apply` repairs everything it
      safely can; `--no-advisories` restricts it to the boundary proposals.
      Partial advisories (`partialFix`) are skipped regardless, since rewriting
      those could regress a working build. -/
  includeAdvisories : Bool := true

/-- Read the proposed migrations and the deprecated-import advisories out of a
    `results.json` document. -/
private def loadMigrations (resultsPath : System.FilePath)
    : IO (Array ImportMigration × Array ImportMigration) := do
  let results ← readJsonFile (α := Results.ResultsJson) resultsPath
  return (results.proposedFixes.map Results.AutoFixJson.toMigration,
          results.deprecatedImports.map Results.AutoFixJson.toMigration)

/-- Execute a `hopscotch fix` command. Returns the process exit code (0 success,
    2 on a user-facing error such as a missing results file). -/
def run (config : Config) (output : String → IO Unit := IO.println) : IO UInt32 := do
  let paths ← mkPaths config.projectDir
  let resultsPath := config.fromPath.getD paths.resultsPath
  unless ← resultsPath.pathExists do
    output s!"no results file at {resultsPath}; run a `hopscotch dep` bisection first, \
              or pass --from PATH"
    return 2
  let (migrations, advisories) ← loadMigrations resultsPath
  match config.action with
  | .list =>
      if migrations.isEmpty && advisories.isEmpty then
        output "No automated fixes were proposed for this run."
        return 0
      unless migrations.isEmpty do
        output s!"{migrations.size} proposed fix(es) recorded:"
        for m in migrations do
          output s!"  [{m.fixId}] {m.describe}"
      unless advisories.isEmpty do
        output s!"{advisories.size} deprecated-import advisory(ies) (informational; not applied by `fix apply`):"
        for m in advisories do
          output s!"  [{m.fixId}] {m.describe}"
      return 0
  | .apply =>
      -- Boundary-fixing proposals always apply (the build is already broken).
      -- Deprecated-import advisories apply too by default (`--no-advisories`
      -- restricts apply to the proposals), except partial ones (shim defines
      -- compat aliases): rewriting those can regress a working build, so they
      -- are surfaced and left to a human.
      let mut toApply := migrations
      if config.includeAdvisories then
        for a in advisories do
          if a.partialFix then
            let reason := if a.note.isEmpty then
                "applying it may be incomplete; migrate the referencing code manually"
              else a.note
            output s!"skipping advisory [{a.fixId}] {a.oldModule}: {reason}"
          else
            toApply := toApply.push a
      if toApply.isEmpty then
        output "No applicable automated fixes were recorded for this run."
        return 0
      -- Dispatch each migration to its owning fix via the registry; a results.json
      -- written by a newer hopscotch could carry fix types this build does not
      -- know, which are skipped loudly rather than misread.
      let mut applied := 0
      let mut changed : Array String := #[]
      for m in toApply do
        match standardAutoFixes.find? (·.id == m.fixId) with
        | some fix =>
            changed := changed ++ (← fix.applyOne paths config.projectDir m)
            applied := applied + 1
        | none =>
            output s!"skipping [{m.fixId}] {m.oldModule}: unknown fix type for this hopscotch version"
      output s!"Applied {applied} migration(s); rewrote {changed.size} file(s) in {config.projectDir}."
      return 0
  | .revert =>
      -- The backup store mirrors project-relative paths and is itself the record
      -- of what `apply` touched, so revert needs no migration metadata.
      let restored ← restoreAllBackups paths config.projectDir
      if restored.isEmpty then
        output "No backups found to restore (originals already in place, or backups were cleaned)."
      else
        output s!"Restored {restored.size} file(s) from backups: {String.intercalate ", " restored.toList}"
      return 0

end Hopscotch.FixCommand
