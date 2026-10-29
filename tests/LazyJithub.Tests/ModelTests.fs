module LazyJithub.Tests.ModelTests

open System
open Xunit
open LazyJithub
open LazyJithub.Model
open LazyJithub.Tests.FakeGitHub

let private fields = Json.parse fieldsJson |> Json.items |> List.map Field.ofJson

let private parse (json: string) =
    Item.ofProjectItem (Some "Status") (Json.parse json) |> Option.get

let private item (id: string) (status: string option) (repo: string) (number: int) (state: string) =
    parse (itemJson id status (issueJson repo number state))

let private board (items: Item list) =
    { Source =
        { Kind = ProjectSource
          Id = "PVT_1"
          Name = "Roadmap"
          Owner = "acme"
          Number = 1
          Url = "" }
      Columns = projectColumns fields items
      Fields = fields
      Items = items
      Total = items.Length
      Truncated = false
      Cutoff = DateTimeOffset(2026, 10, 1, 0, 0, 0, TimeSpan.Zero)
      Today = DateTime(2026, 10, 29) }

[<Fact>]
let ``a project item reads its content and its fields`` () =
    let i = item "PVTI_1" (Some "In Progress") "acme/api" 12 "OPEN"
    Assert.Equal("acme/api#12", i.Key)
    Assert.Equal("api#12", i.ShortKey)
    Assert.Equal(Some "PVTI_1", i.ItemId)
    Assert.Equal("I_12", i.ContentId)
    Assert.Equal("In Progress", i.Status)
    Assert.Equal("opt-In Progress", i.StatusId)
    Assert.Equal(InProgress, i.Category)
    Assert.Equal<string list>([ "alice" ], i.Assignees)
    Assert.Equal(Some "Bug", i.IssueType)
    Assert.Equal(Some(Choice("p1", "High")), Map.tryFind "Priority" i.Fields)
    Assert.False(i.Fields.ContainsKey "Title")
    Assert.Equal(Some(DateTimeOffset(2026, 10, 5, 10, 0, 0, TimeSpan.Zero)), i.Updated)

[<Fact>]
let ``an item without status goes to No Status, and a closed one is done`` () =
    let i = item "PVTI_2" None "acme/api" 3 "CLOSED"
    Assert.Equal(noStatus, i.Status)
    Assert.Equal("", i.StatusId)
    Assert.Equal(Done, i.Category)

[<Fact>]
let ``archived and redacted items are dropped`` () =
    let archived = (itemJson "A" None (issueJson "acme/api" 1 "OPEN")).Replace("\"isArchived\":false", "\"isArchived\":true")
    Assert.True((Item.ofProjectItem (Some "Status") (Json.parse archived)).IsNone)
    let redacted = itemJson "R" None "null"
    Assert.True((Item.ofProjectItem (Some "Status") (Json.parse redacted)).IsNone)

[<Fact>]
let ``a draft issue has a key of its own`` () =
    let draft =
        """{"__typename":"DraftIssue","id":"DI_1","title":"Idea","createdAt":"2026-10-01T00:00:00Z","updatedAt":"2026-10-01T00:00:00Z","creator":{"login":"bob"},"assignees":{"nodes":[]}}"""

    let i = parse (itemJson "PVTI_9" (Some "Todo") draft)
    Assert.Equal("draft:PVTI_9", i.Key)
    Assert.Equal("draft", i.ShortKey)
    Assert.Equal(DraftKind, i.Kind)
    Assert.Equal(Some "bob", i.Author)
    Assert.True i.IsOpen

[<Fact>]
let ``a search result takes its state as status`` () =
    let pull =
        (issueJson "acme/web" 7 "MERGED").Replace("\"__typename\":\"Issue\"", "\"__typename\":\"PullRequest\"")

    let i = Item.ofContent (Json.parse pull) None None |> Option.get
    Assert.Equal(PullKind, i.Kind)
    Assert.Equal("Merged", i.Status)
    Assert.Equal("state:merged", i.StatusId)
    Assert.Equal(Done, i.Category)
    Assert.Equal<string list>([ "Merged" ], searchColumns [ i ] |> List.map _.Name)

[<Fact>]
let ``fields read options, colors and iterations`` () =
    let status = Field.status fields |> Option.get
    Assert.Equal("Status", status.Name)
    Assert.Equal<string list>([ "Todo"; "In Progress"; "Done" ], status.Options |> List.map _.Name)
    Assert.Equal(Some "#d29922", status.Options.[1].Color)
    let sprint = fields |> List.find Field.isIteration
    Assert.Equal<string list>([ "Sprint 1"; "Sprint 2" ], sprint.Iterations |> List.map _.Title)

[<Fact>]
let ``No Status is a column only while an item has no status`` () =
    let withStatus = [ item "1" (Some "Todo") "acme/api" 1 "OPEN" ]
    Assert.Equal<string list>([ "Todo"; "In Progress"; "Done" ], projectColumns fields withStatus |> List.map _.Name)
    let without = item "2" None "acme/api" 2 "OPEN" :: withStatus
    Assert.Equal<string list>([ noStatus; "Todo"; "In Progress"; "Done" ], projectColumns fields without |> List.map _.Name)

[<Fact>]
let ``references resolve against the known repositories`` () =
    let repos = [ "acme/api"; "acme/web" ]
    Assert.Equal(Some "acme/web#4", Item.parseRef repos None "web#4")
    Assert.Equal(Some "acme/web#4", Item.parseRef repos None "WEB#4")
    Assert.Equal(Some "other/thing#9", Item.parseRef repos None "other/thing#9")
    Assert.Equal(Some "acme/api#5", Item.parseRef repos (Some "acme/api") "#5")
    Assert.Equal(Some "o/r#77", Item.parseRef repos None "https://github.com/o/r/pull/77/files")
    Assert.Equal(Some "o/r#8", Item.parseRef repos None " https://github.com/o/r/issues/8 ")
    Assert.Equal(None, Item.parseRef repos None "nope#4")
    Assert.Equal(None, Item.parseRef repos None "#5")
    Assert.Equal(None, Item.parseRef repos None "hello")

let private sample () =
    [ item "1" (Some "Todo") "acme/api" 1 "OPEN"
      item "2" (Some "In Progress") "acme/web" 2 "OPEN"
      item "3" (Some "Done") "acme/api" 3 "CLOSED"
      item "4" None "acme/web" 4 "OPEN" ]

[<Fact>]
let ``the Board scope hides items closed before the cutoff`` () =
    let items = sample ()
    let old = { items.[2] with Closed = Some(DateTimeOffset(2026, 9, 1, 0, 0, 0, TimeSpan.Zero)) }
    let d = board [ items.[0]; old ]
    Assert.Equal<string list>([ "acme/api#1" ], BoardData.filtered d Filters.empty ByRank |> List.map _.Key)
    Assert.Equal(2, BoardData.filtered d { Filters.empty with Scope = All } ByRank |> List.length)

[<Fact>]
let ``the Iteration scope keeps the items of the current iteration`` () =
    let items = sample ()
    let inSprint = { items.[0] with Fields = items.[0].Fields.Add("Sprint", IterationValue("it2", "Sprint 2")) }
    let lastSprint = { items.[1] with Fields = items.[1].Fields.Add("Sprint", IterationValue("it1", "Sprint 1")) }
    let d = board [ inSprint; lastSprint ]
    Assert.Equal(Some "Sprint 2", BoardData.currentIteration d |> Option.map _.Title)
    Assert.Equal<Scope list>([ Board; Iteration; All ], BoardData.scopes d)
    Assert.Equal<string list>([ "acme/api#1" ], BoardData.filtered d { Filters.empty with Scope = Iteration } ByRank |> List.map _.Key)

[<Fact>]
let ``filters combine status, repository and assignee`` () =
    let d = board (sample ())
    let columns = d.Columns |> List.map _.Name
    let inProgress = List.findIndex ((=) "In Progress") columns
    let byStatus = { Filters.empty with Columns = set [ inProgress ] }
    Assert.Equal<string list>([ "acme/web#2" ], BoardData.filtered d byStatus ByRank |> List.map _.Key)
    let byRepo = { Filters.empty with Repos = set [ "acme/api" ] }
    Assert.Equal<string list>([ "acme/api#1"; "acme/api#3" ], BoardData.filtered d byRepo ByRank |> List.map _.Key)
    let nobody = { Filters.empty with Assignee = Some "zoe" }
    Assert.Empty(BoardData.filtered d nobody ByRank)
    Assert.Equal<int list>([ 1; 1; 1; 1 ], BoardData.columnCounts d Filters.empty)
    Assert.Equal(Some 2, Map.tryFind "acme/web" (BoardData.repoCounts d Filters.empty))

[<Fact>]
let ``sorting by status follows the columns, then the repository`` () =
    let d = board (sample ())
    let keys = BoardData.filtered d Filters.empty ByStatus |> List.map _.Key
    Assert.Equal<string list>([ "acme/web#4"; "acme/api#1"; "acme/web#2"; "acme/api#3" ], keys)
    let byRepo = BoardData.filtered d Filters.empty ByRepo |> List.map _.Key
    Assert.Equal<string list>([ "acme/api#1"; "acme/api#3"; "acme/web#4"; "acme/web#2" ], byRepo)

[<Fact>]
let ``lanes by a single-select field follow its options, empty last`` () =
    let items = sample ()
    let low = { items.[1] with Fields = items.[1].Fields.Add("Priority", Choice("p2", "Low")) }
    let none = { items.[3] with Fields = items.[3].Fields.Remove "Priority" }
    let d = board [ items.[0]; low; none ]
    Assert.Contains(ByField "Priority", BoardData.laneModes d)
    Assert.DoesNotContain(ByField "Status", BoardData.laneModes d)
    let lanes = BoardData.lanes d { Filters.empty with LaneMode = ByField "Priority" }
    Assert.Equal<string list>([ "High"; "Low"; "No Priority" ], lanes |> List.map _.Label)
    let mine = BoardData.filtered d { Filters.empty with LaneMode = ByField "Priority"; Lane = Some "Low" } ByRank
    Assert.Equal<string list>([ "acme/web#2" ], mine |> List.map _.Key)

[<Fact>]
let ``status names give a category when GitHub gives none`` () =
    Assert.Equal(Done, StatusCategory.ofName "Done")
    Assert.Equal(InProgress, StatusCategory.ofName "In review")
    Assert.Equal(ToDo, StatusCategory.ofName "Backlog")

[<Fact>]
let ``a source survives the state file`` () =
    let s = Source.search "is:open assignee:@me"
    Assert.Equal(Some s, Source.ofState (Source.toState s))
    Assert.Equal(None, Source.ofState [| "board"; "1" |])
