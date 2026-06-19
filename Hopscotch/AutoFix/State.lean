import Lean
import Lean.Data.Json.FromToJson

/-!
# Automated-fix records

The serializable records describing automated fixes that Hopscotch proposes
after a run concludes at a failure boundary. They live in their own module
(rather than in `Hopscotch.AutoFix`) so that `Hopscotch.State` can embed them in
`PersistedState` without importing the IO-heavy fix machinery.

Proposals are never applied by a run: they are surfaced in `summary.md` and
`results.json`, and applying them is the consumer's choice (`hopscotch fix
apply`). See `Hopscotch.AutoFix` for the detection that produces them.
-/

namespace Hopscotch.AutoFix

open Lean

/--
One proposed module-deprecation migration.

When the dependency deletes a module file (e.g. mathlib's
`Topology.Algebra.Module.LinearMap`) and only later re-adds it as a
`deprecated_module` shim, any downstream `import` of the old module breaks for
every commit in the deletion window. When a run stops at such a commit,
Hopscotch records here that rewriting the import to the module(s) the shim
points at would repair the failure.
-/
structure ModuleMigration where
  /-- Identifier of the fix that produced this migration (e.g. `"module-deprecation"`). -/
  fixId : String
  /-- The deleted/deprecated module, dotted (e.g. `"Mathlib.Topology.Algebra.Module.LinearMap"`). -/
  oldModule : String
  /-- The replacement module(s) the deprecation shim re-exports, in import order.
      Empty means the import should be removed — the shim re-exports nothing
      (e.g. mathlib's "Upstreamed to core" shims). -/
  newModules : Array String
  /-- `true` when the shim this migration was derived from also defines
      declarations (typically `@[deprecated]` compatibility aliases). Rewriting
      the import then drops those aliases, so the migration may be partial:
      downstream code referencing them needs renaming too. -/
  shimHasDeclarations : Bool := false
  deriving Repr, Inhabited, BEq, ToJson, FromJson

/-- One-line human rendering of a migration: `import Old → New1, New2`, or
    `remove import Old` when there is no replacement, with a partial-fix marker
    when the shim also defines declarations. Shared by console notes,
    `summary.md`, and `hopscotch fix list`. -/
def ModuleMigration.describe (m : ModuleMigration) : String :=
  let base :=
    if m.newModules.isEmpty then
      s!"remove import {m.oldModule}"
    else
      s!"import {m.oldModule} → {String.intercalate ", " m.newModules.toList}"
  if m.shimHasDeclarations then
    base ++ " [partial: the shim also defines declarations; referencing code needs renaming]"
  else
    base

end Hopscotch.AutoFix
