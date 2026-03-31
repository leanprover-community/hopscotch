import Hopscotch

open Hopscotch

/-- CLI entrypoint.
    Exit 0: session completed with no failures.
    Exit 1: a failure boundary was found (the tool ran successfully; the downstream failed).
    Exit 2: an unexpected error in the tool itself. -/
def main (args : List String) : IO UInt32 := do
  try
    let config ← CLI.parseArgs args
    let stdoutColor ← detectStdoutColor
    let result ← Runner.run config IO.println stdoutColor
    IO.println <| colorize stdoutColor .info result.summary
    return UInt32.ofNat result.exitCode
  catch error =>
    let stderrColor ← detectStderrColor
    IO.eprintln <| colorize stderrColor .failure error.toString
    return 2
