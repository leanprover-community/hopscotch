namespace Hopscotch.ItemList

/-- Normalize an item-list file by trimming lines and dropping blanks/comments.
    Handles LF, CRLF, and CR-only line endings. -/
def normalizeLines (contents : String) : Array String :=
  -- Replace \r\n before \r: the reverse order would turn \r\n into \n\n (a spurious blank line).
  let lf := (contents.replace "\r\n" "\n").replace "\r" "\n"
  (lf.splitOn "\n" |>.filterMap fun line =>
    let trimmed := line.trimAscii.copy
    if trimmed.isEmpty || trimmed.startsWith "#" then none else some trimmed).toArray

/-- Load an item list from disk and reject files that normalize to no items. -/
def load (path : System.FilePath) : IO (Array String) := do
  let items := normalizeLines (← IO.FS.readFile path)
  if items.isEmpty then
    throw <| IO.userError s!"item list is empty after normalization: {path}"
  return items

end Hopscotch.ItemList
