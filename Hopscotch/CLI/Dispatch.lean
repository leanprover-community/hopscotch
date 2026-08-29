import Hopscotch.CLI
import Hopscotch.Runner
import Hopscotch.FixCommand
import Hopscotch.Util

namespace Hopscotch.CLI

open Hopscotch
open Hopscotch.State

/--
Dispatch the parsed CLI command and return the exit code.

This is the dispatch function after parsing. Errors during dispatch
(runner failures, I/O errors, etc.) are caught and returned as exit code 2.

Exit codes:
- 0: session completed with no failures
- 1: failure boundary was found (the tool ran successfully; the downstream failed)
- 2: an unexpected error in the tool itself
-/
private def dispatchCommand (command : Command) (fixes : Array AutoFix.Fix)
    (output : String → IO Unit := IO.println) : IO UInt32 := do
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

/--
Run the CLI entry point with raw arguments.

This is the root entry point that parses arguments and dispatches the command.
Both parsing errors (bad args, unknown flags) and dispatch errors (runner
failures, I/O errors, infrastructure errors) are caught and returned as exit
code 2. This ensures all tool errors are distinguished from downstream
failures (exit 1).

Exit codes:
- 0: session completed with no failures
- 1: failure boundary was found (the tool ran successfully; the downstream failed)
- 2: an unexpected error (parse error, I/O failure, infrastructure error, etc.)
-/
def runCli (fixes : Array AutoFix.Fix) (args : List String)
    (output : String → IO Unit := IO.println) : IO UInt32 := do
  try
    let command ← CLI.parseArgs fixes args
    return ← dispatchCommand command fixes output
  catch error =>
    let stderrColor ← detectStderrColor
    IO.eprintln <| colorize stderrColor .failure error.toString
    return 2

end Hopscotch.CLI
