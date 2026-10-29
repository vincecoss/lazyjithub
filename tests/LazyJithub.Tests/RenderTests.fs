module LazyJithub.Tests.RenderTests

open System
open Xunit
open LazyJithub
open LazyJithub.Model
open LazyJithub.Render
open LazyJithub.Text
open LazyJithub.Tests.FakeGitHub

let private now = DateTimeOffset(2026, 10, 29, 12, 0, 0, TimeSpan.Zero)
let private fields = Json.parse fieldsJson |> Json.items |> List.map Field.ofJson
let private colorOf _ _ = "#ffffff"

let private item (status: string option) (content: string) =
    Item.ofProjectItem (Some "Status") (Json.parse (itemJson "PVTI_1" status content)) |> Option.get

let private lines (v: ItemView) =
    Rich.render 100 v.Blocks |> List.map Rich.lineText

[<Fact>]
let ``a row shows key, state, assignee, age, status and title`` () =
    let i = item (Some "Todo") (issueJson "acme/api" 12 "OPEN")
    let row = itemRow i 8 true colorOf now false |> segmentsText
    Assert.Equal(" api#12   ○ AL  24d Todo Title 12", row)
    let quiet = itemRow i 6 false colorOf now true |> segmentsText
    Assert.Equal("•api#12 ○ AL  24d Title 12", quiet)

[<Fact>]
let ``glyphs follow the state`` () =
    Assert.Equal("○", (stateGlyph IssueKind "OPEN" None false).Text)
    Assert.Equal("✓", (stateGlyph IssueKind "CLOSED" (Some "COMPLETED") false).Text)
    Assert.Equal("⊘", (stateGlyph IssueKind "CLOSED" (Some "NOT_PLANNED") false).Text)
    Assert.Equal("⇄", (stateGlyph PullKind "MERGED" None false).Text)
    Assert.Equal("◌", (stateGlyph DraftKind "OPEN" None false).Text)

let private detail =
    """{"__typename":"Issue","id":"I_12","number":12,"title":"Crash on save","state":"OPEN","stateReason":null,
    "url":"https://github.com/acme/api/issues/12","createdAt":"2026-10-01T10:00:00Z","updatedAt":"2026-10-20T10:00:00Z","closedAt":null,
    "repository":{"nameWithOwner":"acme/api"},"author":{"login":"bob"},
    "assignees":{"nodes":[{"login":"alice"},{"login":"carl"}]},
    "labels":{"nodes":[{"name":"bug","color":"d73a4a"}]},
    "milestone":{"title":"v1","dueOn":"2026-10-15T00:00:00Z"},"issueType":{"name":"Bug"},
    "parent":{"number":1,"title":"Epic","state":"OPEN","url":"u","repository":{"nameWithOwner":"acme/api"}},
    "subIssuesSummary":{"total":2,"completed":1},
    "subIssues":{"totalCount":2,"nodes":[
      {"number":13,"title":"Part A","state":"CLOSED","stateReason":"COMPLETED","url":"u","repository":{"nameWithOwner":"acme/api"}},
      {"number":14,"title":"Part B","state":"OPEN","stateReason":null,"url":"u","repository":{"nameWithOwner":"acme/api"}}]},
    "closedByPullRequestsReferences":{"nodes":[{"number":20,"title":"Fix save","state":"OPEN","isDraft":false,"url":"u","repository":{"nameWithOwner":"acme/web"}}]},
    "body":"Steps:\n\n1. Save\n\nSee #30 and https://example.com/log",
    "comments":{"totalCount":3,"nodes":[{"author":{"login":"carl"},"body":"Same here, see o/r#2","createdAt":"2026-10-21T10:00:00Z"}]}}"""

[<Fact>]
let ``the item view lists fields, sections and links`` () =
    let row = item (Some "In Progress") (issueJson "acme/api" 12 "OPEN")
    let v = itemView (Json.parse detail) (Some row) fields colorOf now
    Assert.Equal("acme/api#12", v.Header.Key)
    Assert.Equal("Crash on save", v.Header.Title)
    let text = lines v

    let field (name: string) =
        text |> List.tryFind (fun l -> l.StartsWith(name + " ")) |> Option.map (fun l -> l.Substring(name.Length).Trim())

    Assert.Equal(Some "In Progress", field "Status")
    Assert.Equal(Some "○ Open", field "State")
    Assert.Equal(Some "alice, carl", field "Assignees")
    Assert.Equal(Some "v1 due 2026-10-15", field "Milestone")
    Assert.Equal(Some "○ acme/api#1 Epic", field "Parent")
    Assert.Equal(Some "▓▓▓▓▓░░░░░ 1/2", field "Sub-issues")
    Assert.Equal(Some "High", field "Priority")
    Assert.Contains("Description", text)
    Assert.Contains("Sub-issues (2)", text)
    Assert.Contains("✓ acme/api#13 Part A", text)
    Assert.Contains("Linked pull requests", text)
    Assert.Contains("Comments (3)", text)
    Assert.Contains("2 older comments not loaded; o opens the page.", text)
    Assert.Equal<string list>([ "parent"; "sub-issue"; "sub-issue"; "closed by" ], v.Links |> List.map _.Relation)
    Assert.Equal<string list>([ "acme/api#30"; "o/r#2" ], v.Mentions)
    Assert.Equal<string list>([ "https://example.com/log" ], v.Urls)
    Assert.Contains("## Comments", v.Markdown)
    Assert.Contains("| Status | In Progress |", v.Markdown)

[<Fact>]
let ``a pull request shows its branch, review and checks`` () =
    let raw =
        """{"__typename":"PullRequest","id":"PR_1","number":5,"title":"Add cache","state":"OPEN","isDraft":false,
        "url":"https://github.com/acme/api/pull/5","createdAt":"2026-10-01T10:00:00Z","updatedAt":"2026-10-20T10:00:00Z",
        "repository":{"nameWithOwner":"acme/api"},"author":{"login":"bob"},"assignees":{"nodes":[]},"labels":{"nodes":[]},
        "headRefName":"cache","baseRefName":"main","additions":10,"deletions":2,"changedFiles":3,"reviewDecision":"CHANGES_REQUESTED",
        "latestReviews":{"nodes":[{"author":{"login":"alice"},"state":"CHANGES_REQUESTED"}]},
        "reviewRequests":{"nodes":[{"requestedReviewer":{"name":"core"}}]},
        "commits":{"nodes":[{"commit":{"statusCheckRollup":{"state":"SUCCESS"}}}]},
        "closingIssuesReferences":{"nodes":[{"number":12,"title":"Crash","state":"OPEN","url":"u","repository":{"nameWithOwner":"acme/api"}}]},
        "body":"","comments":{"totalCount":0,"nodes":[]}}"""

    let v = itemView (Json.parse raw) None [] colorOf now
    let text = lines v
    Assert.Contains("Branch       cache → main", text)
    Assert.Contains("Review       changes requested", text)
    Assert.Contains("Reviewed by  alice", text)
    Assert.Contains("Requested    core", text)
    Assert.Contains("Checks       passing", text)
    Assert.Contains("Changes      +10 −2 in 3 files", text)
    Assert.Contains("Assignees    Unassigned", text)
    Assert.Contains("No description.", text)
    Assert.Contains("Closes", text)
    Assert.Equal<string list>([ "closes" ], v.Links |> List.map _.Relation)
