import Lean
import Lean.Data.Json.FromToJson

/-!
# Import migrations

The serializable record the automated-fix framework deals in: a proposed rewrite
of one downstream `import`. It is the framework's shared currency — every `Fix`
expresses its repair as an `ImportMigration`, and it is what gets persisted in
`PersistedState` and `results.json`.

It lives in its own leaf module (rather than in `Hopscotch.AutoFix.Framework`) so
that `Hopscotch.State` can embed it in `PersistedState` without importing the
IO-heavy fix machinery — the framework imports `Hopscotch.State` for its path
layout, so the record cannot live alongside it.

Proposals are never applied by a run: they are surfaced in `summary.md` and
`results.json`, and applying them is the consumer's choice (`hopscotch fix
apply`). See `Hopscotch.AutoFix.Framework` for the detection that produces them.
-/

namespace Hopscotch.AutoFix

open Lean

/--
One proposed import rewrite: replace `import oldModule` with imports of
`newModules` (or remove it, when `newModules` is empty).

The module-deprecation fix is the canonical producer — when a dependency deletes
a module and re-adds it as a `deprecated_module` shim, rewriting the import to
the modules the shim re-exports repairs the breakage — but the record itself is
fix-agnostic: any fix that repairs a build by editing imports uses it.
-/
structure ImportMigration where
  /-- Identifier of the fix that produced this migration (e.g. `"module-deprecation"`). -/
  fixId : String
  /-- The import to rewrite, as a dotted module name
      (e.g. `"Mathlib.Topology.Algebra.Module.LinearMap"`). -/
  oldModule : String
  /-- The module(s) to import instead, in order. Empty means the import should be
      removed (e.g. the source re-exports nothing). -/
  newModules : Array String
  /-- `true` when applying this migration may be incomplete: the rewrite repairs
      the import, but referencing code might still need manual changes (e.g. the
      source also defined declarations the rewrite drops). `fix apply` skips
      partial advisories and renders a marker; see `note` for the reason. -/
  partialFix : Bool := false
  /-- Human-readable reason a partial migration is incomplete, supplied by the
      producing fix. Empty when the migration is not partial. -/
  note : String := ""
  deriving Repr, Inhabited, BEq, ToJson, FromJson

/-- One-line human rendering of a migration: `import Old → New1, New2`, or
    `remove import Old` when there is no replacement, with a `[partial]` marker
    (and the fix's note) when applying it may be incomplete. Shared by console
    notes, `summary.md`, and `hopscotch fix list`. -/
def ImportMigration.describe (m : ImportMigration) : String :=
  let base :=
    if m.newModules.isEmpty then
      s!"remove import {m.oldModule}"
    else
      s!"import {m.oldModule} → {String.intercalate ", " m.newModules.toList}"
  if m.partialFix then
    let reason := if m.note.isEmpty then "applying this may be incomplete" else m.note
    base ++ s!" [partial: {reason}]"
  else
    base

end Hopscotch.AutoFix
