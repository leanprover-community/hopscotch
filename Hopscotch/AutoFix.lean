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
-/
