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
  try
    match ← CLI.parseArgs hopscotchFixes args with
    | .run config =>
        let stdoutColor ← detectStdoutColor
        let result ← Runner.run config IO.println stdoutColor
        IO.println <| colorize stdoutColor .info result.summary
        return UInt32.ofNat result.exitCode
    | .fix config =>
        return ← FixCommand.run hopscotchFixes config IO.println
    | .clean projectDir =>
        let stateRoot := projectDir / ".lake" / "hopscotch"
        if ← stateRoot.pathExists then
          IO.FS.removeDirAll stateRoot
          IO.println s!"Removed {stateRoot}"
        else
          IO.println s!"Nothing to clean ({stateRoot} does not exist)"
        return 0
    | .version =>
        IO.println CLI.versionString
        return 0
    | .help =>
        IO.println CLI.helpText
        return 0
  catch error =>
    let stderrColor ← detectStderrColor
    IO.eprintln <| colorize stderrColor .failure error.toString
    return 2
