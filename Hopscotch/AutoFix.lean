import Hopscotch.AutoFix.Framework
import Hopscotch.AutoFix.Migration

/-!
# Automated fixes

Umbrella for hopscotch's **dependency-agnostic** automated-fix machinery: the
generic framework (`Hopscotch.AutoFix.Framework`) and the import-migration
primitive (`Hopscotch.AutoFix.Migration`).

Concrete fixes are not bundled here. A fix such as the mathlib `deprecated_module`
detector (`Hopscotch.AutoFix.Mathlib.ModuleDeprecation`) is injected into the CLI
by the composition root (`Main`), so this core library stays free of any specific
dependency's conventions.

A planned follow-up is a two-package split — a `hopscotch` core library plus a
`hopscotch-mathlib` binary — turning this convention into a hard boundary where
the core *package* cannot see mathlib-specific code at all. Lean has no runtime
plugin loading, so composition-root injection is the pragmatic form of that
boundary within a single package.
-/
