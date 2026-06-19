import Hopscotch.AutoFix.Framework
import Hopscotch.AutoFix.ModuleDeprecation

/-!
# Automated fixes

Umbrella module for hopscotch's automated-fix machinery. It re-exports the
generic framework (`Hopscotch.AutoFix.Framework`) and the concrete fixes
(`Hopscotch.AutoFix.ModuleDeprecation`), and defines the default registry the
CLI installs on a `dep` run.
-/

namespace Hopscotch.AutoFix

/-- The default registry of automated fixes, in application order. -/
def standardAutoFixes : Array Fix := #[moduleDeprecationFix]

end Hopscotch.AutoFix
