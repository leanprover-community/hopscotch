import Hopscotch.CLI
import Hopscotch.Runner
import Hopscotch.FixCommand
import Hopscotch.Util

namespace Hopscotch.CLI

open Hopscotch
open Hopscotch.State

/--
Dispatch the parsed CLI command and return the exit code.

This function handles the command dispatch logic that the CLI entry point uses.
It can be imported and tested independently of Main, allowing tests to assert
on the exact exit codes (0 for success, 1 for failure boundary, 2 for errors).

Exit codes:
- 0: session completed with no failures
- 1: failure boundary was found (the tool ran successfully; the downstream failed)
- 2: an unexpected error in the tool itself
-/
def dispatchCommand (command : Command) (fixes : Array AutoFix.Fix)
    (output : String → IO Unit := IO.println) : IO UInt32 := do
  try
    match command with
    | .run config =>
        let stdoutColor ← detectStdoutColor
        let result ← Runner.run config output stdoutColor
        output <| colorize stdoutColor .info result.summary
        return UInt32.ofNat result.exitCode
    | .fix config =>
        return ← FixCommand.run fixes config output
    | .clean projectDir =>
        let stateRoot := projectDir / ".lake" / "hopscotch"
        if ← stateRoot.pathExists then
          IO.FS.removeDirAll stateRoot
          output s!"Removed {stateRoot}"
        else
          output s!"Nothing to clean ({stateRoot} does not exist)"
        return 0
    | .version =>
        output CLI.versionString
        return 0
    | .help =>
        output CLI.helpText
        return 0
  catch error =>
    let stderrColor ← detectStderrColor
    IO.eprintln <| colorize stderrColor .failure error.toString
    return 2

end Hopscotch.CLI
