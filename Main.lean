import Hopscotch
import Hopscotch.AutoFix.Mathlib.ModuleDeprecation

open Hopscotch

/-- The automated fixes this binary ships with — the composition root where
    dependency-specific fixes are plugged in. The core library and CLI are
    dependency-agnostic and take the fix registry as input; here we hook in
    mathlib's `deprecated_module` fix because mathlib dominates the ecosystem.
    A different binary could inject a different set (or none). -/
def hopscotchFixes : Array AutoFix.Fix := #[AutoFix.Mathlib.moduleDeprecationFix]

/-- CLI entrypoint.
    Exit 0: session completed with no failures.
    Exit 1: a failure boundary was found (the tool ran successfully; the downstream failed).
    Exit 2: an unexpected error in the tool itself. -/
def main (args : List String) : IO UInt32 := do
  let command ← CLI.parseArgs hopscotchFixes args
  CLI.dispatchCommand command hopscotchFixes IO.println
