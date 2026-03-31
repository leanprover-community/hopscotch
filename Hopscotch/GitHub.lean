import Lean
import Lean.Data.Json.FromToJson
import Hopscotch.Util

namespace Hopscotch.GitHub

open Lean

/--
Parse a GitHub HTTPS or SSH remote URL into `(owner, repo)`.

Handles:
- `https://github.com/owner/repo`
- `https://github.com/owner/repo.git`
- `git@github.com:owner/repo`
- `git@github.com:owner/repo.git`
-/
def parseRepoId (url : String) : Option (String × String) :=
  let stripGit (s : String) : String :=
    if s.endsWith ".git" then s.take (s.length - 4) |>.copy else s
  let parse (pfx : String) : Option (String × String) :=
    if !url.startsWith pfx then none
    else match (stripGit (url.drop pfx.length |>.copy)).splitOn "/" with
         | [owner, repo] => some (owner, repo)
         | _ => none
  parse "git@github.com:" <|> parse "https://github.com/"

/--
Emit a warning when `GITHUB_TOKEN` is absent and suggest how to set it if the
`gh` CLI appears to be available.
-/
def warnIfNoToken (token : Option String) (warn : String → IO Unit) : IO Unit := do
  if token.isNone then
    warn "Warning: GITHUB_TOKEN is not set; GitHub API rate limit is 60 requests/hour."
    let ghCheck ← IO.Process.output {
      cmd := "sh"
      args := #["-c", "command -v gh >/dev/null 2>&1"]
    }
    if ghCheck.exitCode == 0 then
      warn "  Tip: export GITHUB_TOKEN=$(gh auth token)"
    else
      warn "  Tip: set GITHUB_TOKEN to a GitHub personal access token to increase the limit."

-- Internal JSON structures for Reservoir package lookup.

private structure ReservoirSource where
  gitUrl : Option String := none
  host : Option String := none
  repoUrl : Option String := none
  deriving FromJson

private structure ReservoirPkg where
  sources : Array ReservoirSource := #[]
  deriving FromJson

/--
Look up a package's git URL from the Reservoir package registry.

When the package is on GitHub, returns the `repoUrl` (the HTML GitHub URL);
otherwise returns the plain `gitUrl`. Returns `none` when the package is not
found (HTTP 404). Throws on network or parse errors.
-/
def fetchReservoirGitUrl (scope name : String) : IO (Option String) := do
  let url := s!"https://reservoir.lean-lang.org/api/v1/packages/{scope}/{name}"
  let result ← IO.Process.output {
    cmd := "curl"
    args := #["-s", "-H", "X-Reservoir-Api-Version:1.0.0", url]
  }
  if result.exitCode != 0 then
    throw <| IO.userError
      s!"curl failed (exit {result.exitCode}) fetching Reservoir package \
         {scope}/{name}: {result.stderr.trimAscii}"
  let json ← IO.ofExcept <| Json.parse result.stdout
  if let some errJson := (json.getObjVal? "error").toOption then
    if ((errJson.getObjVal? "status").toOption >>= fun x => (Json.getNat? x).toOption) == some 404 then
      return none
    let msg :=
      ((errJson.getObjVal? "message").toOption >>= fun x => (Json.getStr? x).toOption).getD "(unknown)"
    throw <| IO.userError s!"{scope}/{name}: Reservoir error: {msg}"
  let payload := (json.getObjVal? "data").toOption.getD json
  let pkg ← IO.ofExcept <| fromJson? (α := ReservoirPkg) payload
  for src in pkg.sources do
    if let some gitUrl := src.gitUrl then
      -- Prefer repoUrl for GitHub packages: it's an HTML github.com URL that parseRepoId can parse;
      -- gitUrl may be an SSH URL or use a non-GitHub host that parseRepoId does not handle.
      return some <| if src.host == some "github" then src.repoUrl.getD gitUrl else gitUrl
  return none

-- Internal JSON structures for GitHub API responses.

/-- One entry from the `commits` array in a compare or commits-list response. -/
private structure CommitItem where
  sha : String
  deriving FromJson

/-- Subset of the `GET /repos/{owner}/{repo}/compare/{base}...{head}` response. -/
private structure CompareResponse where
  total_commits : Nat
  commits : Array CommitItem
  deriving FromJson

/-- Minimal structure for detecting GitHub API error responses.
    Both fields must be present to avoid misidentifying normal responses
    that happen to contain a `message` key. -/
private structure GitHubApiError where
  message : String
  documentation_url : Option String := none
  deriving FromJson

/-- The `object` field inside a `GET /repos/{owner}/{repo}/git/ref/heads/{branch}` response. -/
private structure GitRefObject where
  sha : String
  deriving FromJson

/-- Subset of the `GET /repos/{owner}/{repo}/git/ref/heads/{branch}` response. -/
private structure GitRef where
  object : GitRefObject
  deriving FromJson

/-- Invoke `curl -s` with GitHub API headers and an optional auth header.
    Returns `(exitCode, stdout, stderr)`. -/
private def curlRaw (url : String) (authHeader : Option String)
    : IO (UInt32 × String × String) := do
  let authArgs : Array String :=
    match authHeader with
    | some h => #["-H", h]
    | none => #[]
  let result ← IO.Process.output {
    cmd := "curl"
    args := #["-s", "-H", "Accept: application/vnd.github+json",
              "-H", "X-GitHub-Api-Version: 2022-11-28"] ++ authArgs ++ #[url]
  }
  return (result.exitCode, result.stdout, result.stderr)

/-- Detect a real GitHub API error by requiring both `message` and `documentation_url`. -/
def parseApiError? (json : Json) : Option GitHubApiError :=
  match fromJson? (α := GitHubApiError) json with
  | .ok err => if err.documentation_url.isSome then some err else none
  | .error _ => none

/--
Run a `curl` GET against `url`, appending `authHeader` when provided.

Throws on network errors or when the GitHub API returns an error response.
-/
private def curlGet (url : String) (authHeader : Option String) : IO String := do
  let (exitCode, stdout, stderr) ← curlRaw url authHeader
  if exitCode != 0 then
    throw <| IO.userError s!"curl failed (exit {exitCode}): {stderr.trimAscii}"
  -- Detect GitHub API error objects: {"message":"...", "documentation_url":"..."}
  if let .ok json := Json.parse stdout then
    if let some err := parseApiError? json then
      throw <| IO.userError s!"GitHub API error for {url}: {err.message}"
  return stdout

/--
Like `curlGet` but returns `none` instead of throwing when the GitHub API
responds with "Not Found" (e.g. a branch that does not exist).

Throws on connection-level curl failures so network errors are not silently
swallowed as "branch not found".
-/
private def tryGetRef (url : String) (authHeader : Option String) : IO (Option String) := do
  let (exitCode, stdout, stderr) ← curlRaw url authHeader
  if exitCode != 0 then
    throw <| IO.userError s!"curl failed (exit {exitCode}): {stderr.trimAscii}"
  if let .ok json := Json.parse stdout then
    if let some err := parseApiError? json then
      -- "Not Found" means the branch simply does not exist; any other message is
      -- a real API error worth surfacing.
      if err.message == "Not Found" then return none
      throw <| IO.userError s!"GitHub API error for {url}: {err.message}"
    if let .ok ref := (fromJson? (α := GitRef) json) then
      return some ref.object.sha
  -- A response that is neither a recognised error nor a GitRef is unexpected;
  -- treat it as a missing ref rather than crashing.
  return none

/--
Return the tip commit SHA of the first of `main` or `master` that exists in
`owner/repo`. Throws when neither branch is found.
-/
def fetchDefaultBranch (owner repo : String) (token : Option String) : IO String := do
  let authHeader := token.map fun t => s!"Authorization: Bearer {t}"
  for branch in (["main", "master"] : List String) do
    let url :=
      s!"https://api.github.com/repos/{owner}/{repo}/git/ref/heads/{branch}"
    if let some sha := ← tryGetRef url authHeader then
      return sha
  throw <| IO.userError
    s!"neither 'main' nor 'master' branch found in {owner}/{repo}; \
       pass --to REF to specify the target commit"

/--
Collect commit SHAs from the paginated `/commits` endpoint, starting at `toRef`
and stopping (exclusive) at `fromRef`. Returns SHAs oldest-first.

Iterates up to `maxPages` pages of 100 commits each (default 100 pages = up to
10 000 commits). Throws if `fromRef` is not found within those pages.
-/
private def fetchPaginatedCommits
    (owner repo fromRef toRef : String)
    (authHeader : Option String)
    (maxPages : Nat := 100) : IO (Array String) := do
  let base := s!"https://api.github.com/repos/{owner}/{repo}/commits"
  let mut collected : Array String := #[]
  let mut foundFrom := false
  for pageIdx in List.range maxPages do
    if foundFrom then break
    let url := s!"{base}?sha={toRef}&per_page=100&page={pageIdx + 1}"
    let body ← curlGet url authHeader
    let json ← IO.ofExcept <| Json.parse body
    let page ← IO.ofExcept <| fromJson? (α := Array CommitItem) json
    if page.isEmpty then break
    for item in page do
      if foundFrom then break
      -- Match both full SHAs and abbreviated prefixes: the lakefile may store a
      -- short rev, while the API always returns full 40-character SHAs.
      if item.sha == fromRef || item.sha.startsWith fromRef || fromRef.startsWith item.sha then
        foundFrom := true
      else
        collected := collected.push item.sha
  if !foundFrom then
    throw <| IO.userError
      s!"could not find commit {fromRef} while paging through {owner}/{repo}; \
         the range may exceed the search limit or the ref may be invalid"
  return collected.reverse  -- /commits returns newest-first; reverse to oldest-first.

/--
Fetch the ordered list of commit SHAs from `fromRef` (exclusive) to `toRef`
(inclusive), oldest-first, using the GitHub REST API.

Uses the compare endpoint for ranges ≤ 250 commits and falls back to paginated
commit listing for larger ranges.
-/
def fetchCommitRange
    (owner repo fromRef toRef : String)
    (token : Option String) : IO (Array String) := do
  let authHeader := token.map fun t => s!"Authorization: Bearer {t}"
  -- Try the compare endpoint first; it returns up to 250 commits in one call.
  -- The three-dot syntax ({fromRef}...{toRef}) is GitHub's way of requesting
  -- commits reachable from toRef but not from fromRef, i.e. the two-dot range
  -- fromRef..toRef in standard git terminology.
  let compareUrl :=
    s!"https://api.github.com/repos/{owner}/{repo}/compare/{fromRef}...{toRef}"
  let compareBody ← curlGet compareUrl authHeader
  let compareJson ← IO.ofExcept <| Json.parse compareBody
  let response ← IO.ofExcept <| fromJson? (α := CompareResponse) compareJson
  if response.total_commits <= 250 then
    return response.commits.map (·.sha)
  -- Fall back to paginated commit listing for large ranges. Unlike the compare endpoint
  -- (oldest-first), the paginated endpoint returns newest-first; fetchPaginatedCommits reverses.
  fetchPaginatedCommits owner repo fromRef toRef authHeader

end Hopscotch.GitHub
