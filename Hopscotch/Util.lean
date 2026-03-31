import Lean
import Lean.Data.Json.FromToJson

namespace Hopscotch

open Lean

/-- Whether terminal coloring is enabled for a given output stream. -/
inductive ColorMode where
  | enabled
  | disabled
  deriving BEq, Repr

/-- Small set of terminal styles used by the CLI's own status lines. -/
inductive ConsoleStyle where
  | attempt
  | info
  | running
  | success
  | failure
  | path
  | header
  deriving BEq, Repr

/-- Decide whether color should be enabled from environment and TTY facts. -/
def decideColorMode (noColorEnv : Option String) (isTty : Bool) : ColorMode :=
  if noColorEnv.isSome || !isTty then .disabled else .enabled

/-- ANSI escape prefix used for terminal styling. -/
private def ansiEscape : String := "\x1b["

/-- Render one ANSI SGR wrapper for a string. -/
private def wrapAnsi (code text : String) : String :=
  s!"{ansiEscape}{code}m{text}{ansiEscape}0m"

/-- ANSI SGR code for one of the tool's small fixed console styles. -/
private def styleCode (style : ConsoleStyle) : String :=
  match style with
  | .attempt => "1;36"  -- bold cyan
  | .info    => "0;36"  -- cyan
  | .running => "1;33"  -- bold yellow
  | .success => "1;32"  -- bold green
  | .failure => "1;31"  -- bold red
  | .path    => "0;34"  -- blue
  | .header  => "1;35"  -- bold magenta

/-- Wrap a string in ANSI escapes when terminal color is enabled. -/
def colorize (mode : ColorMode) (style : ConsoleStyle) (text : String) : String :=
  match mode with
  | .enabled => wrapAnsi (styleCode style) text
  | .disabled => text

/-- Detect color support for a specific file descriptor using `test -t`. -/
private def detectColorForFd (fd : String) : IO ColorMode := do
  let noColorEnv ← IO.getEnv "NO_COLOR"
  let ttyCheck ← IO.Process.spawn {
    cmd := "sh"
    args := #["-c", s!"test -t {fd}"]
  }
  return decideColorMode noColorEnv ((← ttyCheck.wait) == 0)

/-- Detect whether stdout should use ANSI color. -/
def detectStdoutColor : IO ColorMode :=
  detectColorForFd "1"

/-- Detect whether stderr should use ANSI color. -/
def detectStderrColor : IO ColorMode :=
  detectColorForFd "2"

/-- Shorten a full commit hash for filenames and human-readable summaries. -/
def shortCommit (commit : String) : String :=
  commit.take 12 |>.copy

/-- Replace non-portable filename characters with underscores. -/
def sanitizeForFileName (value : String) : String :=
  String.map (fun ch => if ch.isAlphanum || ch == '-' then ch else '_') value

/-- Return a UTC timestamp string suitable for persisted state. -/
def nowUtcString : IO String := do
  let output ← IO.Process.output {
    cmd := "date"
    args := #["-u", "+%Y-%m-%dT%H:%M:%SZ"]
  }
  if output.exitCode != 0 then
    throw <| IO.userError s!"date command failed: {output.stderr.trimAscii}"
  return output.stdout.trimAscii.copy

/-- Create the parent directory of `path` when one exists. -/
def ensureParentDir (path : System.FilePath) : IO Unit := do
  match path.parent with
  | some parent => IO.FS.createDirAll parent
  | none => pure ()

/-- Read and decode a JSON file using Lean's deriving-based `FromJson` support. -/
def readJsonFile {α : Type _} [Lean.FromJson α] (path : System.FilePath) : IO α := do
  let contents ← IO.FS.readFile path
  let json ← IO.ofExcept <| Lean.Json.parse contents
  IO.ofExcept <| Lean.fromJson? json

/-- Write pretty-printed JSON to disk atomically (write-to-temp then rename).
    Creating parent directories as needed. -/
def writeJsonFile {α : Type _} [Lean.ToJson α] (path : System.FilePath) (value : α) : IO Unit := do
  ensureParentDir path
  let tmp := System.FilePath.mk (path.toString ++ ".tmp")
  IO.FS.writeFile tmp (Lean.Json.pretty (Lean.toJson value))
  IO.FS.rename tmp path

/-- Resolve a path to a normalized absolute path for stable persisted state. -/
def realPathNormalized (path : System.FilePath) : IO System.FilePath := do
  return (← IO.FS.realPath path).normalize

end Hopscotch
