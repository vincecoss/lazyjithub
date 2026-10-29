# lazyjithub

A lazygit-style terminal UI for GitHub Projects, in F# with [Terminal.Gui](https://github.com/gui-cs/Terminal.Gui) v2.
The GitHub sibling of lazyjira: same panels, same keys, same look.

```
╭[1] Projects - Searches · Rum & Code roadmap───╮╭[0] rumandcode/api#101──────────────────────────────╮
│● Rum & Code roadmap  rumandcode #7            ││ rumandcode/api#101  ○ Issue · Bug  In progress      │
╰────────────────────────────────────── 2 of 3 ─╯│ Login fails with SSO when the session expires       │
╭[2] Status─────────────────────────────────────╮│                                                     │
│  All 10                                       ││ Status      In progress                             │
│  Backlog 2                                    ││ State       ○ Open                                  │
│  In progress 2                                ││ Assignees   vcossette                               │
╰────────────────────────────────────── 1 of 7 ─╯│ Parent      ○ rumandcode/api#100 Q4 reliability epic│
╭[3] Repositories───────────────────────────────╮│ Sub-issues  ▓▓▓░░░░░░░ 1/3                          │
│  All 10                                       ││ Priority    P0                                      │
╰────────────────────────────────────── 1 of 3 ─╯│ Sprint      Sprint 3                                │
╭[4] None - Assignee - Iteration - Milestone - …╮│                                                     │
│All 10                                         ││ Description                                         │
╰────────────────────────────────────── 1 of 1 ─╯│                                                     │
╭[5] Board - Iteration - All · by status, repo…─╮│ When the SSO session expires, login fails with a    │
│ api#102 ○ MO  19d Ready Rate limit the webho… ││ 401 and the user is stuck.                          │
│ api#101 ○ VC   1d In progress Login fails wi… ││                                                     │
│ web#104 ⇄ JD   2d In review Add CSV export t… ││ ▎ Note                                              │
╰───────────────────────────────────── 3 of 10 ─╯╰─────────────────────────────────────────────────────╯
```

## Read-only

lazyjithub changes nothing on GitHub. Every HTTP request goes through a guard (`src/LazyJithub/ReadOnly.fs`)
that lets GraphQL queries and REST `GET` through, and refuses mutations, subscriptions and any other write
before it leaves the machine. Tests cover the guard.

## Install

With the .NET 10 SDK:

```sh
dotnet pack src/LazyJithub -o nupkg
dotnet tool install -g LazyJithub --add-source nupkg
```

With Nix, from the git repository:

```sh
nix profile install github:<owner>/lazyjithub   # private repo: git+ssh://git@github.com/<owner>/lazyjithub
nix run github:<owner>/lazyjithub               # run once without installing
```

## Run

```sh
nix run .                                  # build, test and run
nix develop                                # shell with the .NET 10 SDK, fsautocomplete and fantomas
dotnet run --project src/LazyJithub -- --project rumandcode/7
dotnet run --project src/LazyJithub -- --search "is:open assignee:@me"
dotnet test
```

Options: `--project OWNER/NUMBER` (or the project URL), `--search QUERY`, `--profile NAME`, `--check`.
Without `--project` or `--search`, lazyjithub reopens what you had open last.
`--check` tests the configuration and the connection, then exits.

## Configure

lazyjithub needs a token with the `read:project` scope, on top of `repo` for private repositories.
Without a config file, it reads `GITHUB_TOKEN` or `GH_TOKEN`, then asks the GitHub CLI with `gh auth token`.
The CLI token lacks `read:project` by default; add it once with:

```sh
gh auth refresh -s read:project
```

Organizations with SAML single sign-on also want the token authorized for them; until then, their projects
are missing and lazyjithub says so.

Settings go in `~/.config/lazyjithub/config.toml`:

```toml
# token_cmd = "gh auth token"            # or: token = "ghp_..."

# url = "https://api.github.com"         # GitHub Enterprise Server: https://ghe.example.com/api
# max_items = 1000                       # cap per project or search load
# done_days = 14                         # on the Board tab, hide items closed longer ago than this
# refresh_seconds = 120                  # reload the open project in the background; 0 turns it off

# [status_colors]                        # by status name, any case, as #rrggbb
# "in review" = "#ffab00"

# default_profile = "work"
# [profiles.work]
# url = "https://ghe.example.com/api"
# token_cmd = "gh auth token --hostname ghe.example.com"
```

Statuses take the color set in the project unless `status_colors` names one.
GraphQL requests count against a budget of 5,000 points an hour: a 1,000-item project costs about
10 requests per load, so keep `refresh_seconds` at a minute or more.

## Panels

| # | Panel | What it does |
|---|-------|--------------|
| 1 | Projects / Searches | `enter` opens a project of yours or of your organizations, a saved search or a recent one. |
| 2 | Status | The columns of the project with counts: the options of its Status field, and No Status. The highlighted status filters the items; `space` selects several. For a search: Open, Draft, Merged, Closed. |
| 3 | Repositories | The repositories of the items, with counts. Same selection rules as Status. |
| 4 | Group by | None, Assignee, Iteration, Milestone, Parent, Type, then each single-select field of the project, such as Priority or Size. The highlighted group filters the items. |
| 5 | Items | Tabs: Board, Iteration (the current one, when the project has an iteration field) and All; Results for a search. |
| 0 | Item | Project fields, state, assignees, labels, milestone, parent, sub-issues; for a pull request, branch, review, checks and size; then the description, sub-issues, linked pull requests and the last 30 comments. |

Issue bodies and comments render GitHub Markdown: headings, lists, task lists, tables, code, quotes and
alerts (`> [!NOTE]`). HTML comments from issue templates are hidden, `<details>` shows its summary,
images show as `▣ name`, and `#12`, `owner/repo#12` and `@login` stand out.

Each item row shows the days since its last update: grey under 7 days, yellow up to 13, red from 14,
and grey for closed items. A yellow `•` marks items updated since you last opened that project.

The Searches tab holds five saved searches (assigned to me, my pull requests, review requested, created by me,
mentioning me) and your recent ones. A search without `is:issue` or `is:pr` runs once for each and merges them.

## Keys

| Key | Action |
|-----|--------|
| `1`-`5`, `0` | jump to a panel |
| `h` / `l`, `←` / `→` | previous / next panel |
| `j` / `k`, `↓` / `↑` | move in a list, scroll the item |
| `}` / `{` | in the item: next / previous section (`]` / `[` too) |
| `ctrl+d` / `ctrl+u` | page down / up |
| `tab` / `shift+tab` | next / previous tab in a panel (`]` / `[` in lists too) |
| `enter` | open a project or search, or move to the next panel |
| `J` / `K` | next / previous item from anywhere |
| `space` | add or remove a status or repository from the selection |
| `esc` | clear the panel filter, or reset the panel to All |
| `/` | filter the focused list, or search the item |
| `n` / `N` | next / previous match in the item |
| `X` | clear all filters |
| `s` | sort by status, repository, rank, or last update |
| `m` | only items assigned to you |
| `:` | GitHub search; `↑` / `↓` in the prompt browse recent searches |
| `v` | board view: the items as columns side by side, a block per group of panel 4; `f` turns the filters off or on |
| `S` | statistics |
| `g` | go to an issue: `owner/repo#12`, `repo#12`, `#12` or its URL |
| `L` | parent, sub-issues, linked pull requests, and issues and URLs mentioned in the text; `enter` goes to one |
| `backspace` | back to the item before the last jump |
| `o` | open the item in the browser, or the project when no item is shown |
| `y` / `Y` / `c` | copy the key / the URL / a Markdown link |
| `C` | copy the whole item as Markdown, comments included |
| `r` | reload |
| `+` | layout: normal, wide item, full item |
| `?` | help |
| `q` | quit |

## Saved state

`~/.local/state/lazyjithub/state.json` (follows `$XDG_STATE_HOME`) keeps, per GitHub host:
the last project or search and its tab, the status, repository and group selection per source,
the sort and group-by modes, whether the board view uses the filters, recent searches,
and when you last opened each source.

## Development

| Path | Role |
|------|------|
| `src/LazyJithub/ReadOnly.fs` | the HTTP guard: GraphQL queries and REST reads only |
| `src/LazyJithub/GitHub.fs` | GraphQL client: projects, items, search, issue and pull request details |
| `src/LazyJithub/Model.fs` | items, project fields, filters, scopes, sorting, groups |
| `src/LazyJithub/Markdown.fs` | GitHub Markdown to styled blocks, with Markdig; issue references and links |
| `src/LazyJithub/Text.fs`, `Rich.fs` | styled text, wrapping, layout; the same as lazyjira |
| `src/LazyJithub/Render.fs` | list rows and the item pane, without Terminal.Gui |
| `src/LazyJithub/Ui/` | frames, panels, popups and the controller |

After a NuGet dependency change, refresh the Nix lock file:

```sh
nix build .#default.fetch-deps && ./result deps.json
```

Flakes only see files tracked by git once the folder is a git repository, so `git add` new files before `nix build`.

## Limits

- Read-only: no assigning, moving or closing yet.
- Project views, with their own filters and layouts, are not read; tabs and groups stand in for them.
- No history: GitHub's API does not give when an item entered its column, hence days since the last update.
- Loads stop at `max_items` items, with a warning; archived items are left out.
- The item pane shows the last 30 comments and the first 50 sub-issues; `o` opens the page for the rest.
- GitHub Enterprise Server versions without issue types or sub-issues reject the queries.
