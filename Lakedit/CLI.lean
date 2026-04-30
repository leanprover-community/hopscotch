namespace Lakedit.CLI

def versionString : String := "lakedit 0.1.0"

def usageText : String :=
  "usage: lakedit set <dep-name> (--path PATH | --rev SHA) [--project-dir DIR] [--quiet]"

def helpText : String :=
  "lakedit — rewrite a lakefile require block\n\n" ++
  usageText ++ "\n\n" ++
  "Subcommands:\n" ++
  "  set <dep-name>   Rewrite the named dependency\n\n" ++
  "Options:\n" ++
  "  --path PATH        Rewrite to a local filesystem path\n" ++
  "  --rev SHA          Rewrite the pinned revision (git SHA, branch, or tag)\n" ++
  "  --project-dir DIR  Directory containing the lakefile (default: .)\n" ++
  "  --quiet            Suppress output\n" ++
  "  --help, -h         Show this help text\n" ++
  "  --version          Print version and exit\n"

inductive SetTarget where
  | path (p : System.FilePath)
  | rev (sha : String)
  -- | git (url : String)   -- deferred

inductive Command where
  | set (depName : String) (target : SetTarget) (projectDir : System.FilePath) (quiet : Bool)
  | help
  | version

private structure SetParseState where
  depName : String
  target : Option SetTarget := none
  projectDir : System.FilePath := "."
  quiet : Bool := false

private def parseSetOptions (state : SetParseState) (args : List String) : IO SetParseState := do
  match args with
  | [] => return state
  | "--path" :: p :: rest =>
      parseSetOptions { state with target := some (.path (System.FilePath.mk p)) } rest
  | "--rev" :: sha :: rest =>
      parseSetOptions { state with target := some (.rev sha) } rest
  | "--project-dir" :: dir :: rest =>
      parseSetOptions { state with projectDir := System.FilePath.mk dir } rest
  | "--quiet" :: rest =>
      parseSetOptions { state with quiet := true } rest
  | _ =>
      throw <| IO.userError usageText

def parse (args : List String) : IO Command := do
  match args with
  | [] | ["--help"] | ["-h"] => return .help
  | ["--version"] => return .version
  | "set" :: depName :: rest =>
      let state ← parseSetOptions { depName } rest
      match state.target with
      | none => throw <| IO.userError s!"lakedit set: one of --path or --rev is required\n{usageText}"
      | some target => return .set state.depName target state.projectDir state.quiet
  | ["set"] =>
      throw <| IO.userError s!"lakedit set: missing dependency name\n{usageText}"
  | _ =>
      throw <| IO.userError usageText

end Lakedit.CLI
