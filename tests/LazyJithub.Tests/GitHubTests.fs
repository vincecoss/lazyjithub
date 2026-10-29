module LazyJithub.Tests.GitHubTests

open System.Net
open Xunit
open LazyJithub
open LazyJithub.GitHub
open LazyJithub.Model
open LazyJithub.Tests.FakeGitHub

let private run (t: System.Threading.Tasks.Task<'T>) = t.GetAwaiter().GetResult()

let private project (id: string) (owner: string) =
    """{"id":"<id>","number":1,"title":"Board <id>","url":"https://github.com/orgs/<owner>/projects/1","closed":false,"updatedAt":"2026-10-01T00:00:00Z","owner":{"login":"<owner>"}}"""
    |> fill [ "id", id; "owner", owner ]

[<Fact>]
let ``projects come from the viewer and its organizations, with the errors of the others`` () =
    let answer =
        """{"data":{"viewer":{"login":"me","projectsV2":{"nodes":[<p1>]},
        "organizations":{"nodes":[{"login":"acme","projectsV2":{"nodes":[<p2>,<p1>]}},
        {"login":"sso","projectsV2":null}]}}},
        "errors":[{"message":"Resource protected by organization SAML enforcement."}]}"""
        |> fill [ "p1", project "P1" "me"; "p2", project "P2" "acme" ]

    let fake = new FakeGitHub([ asks "viewer", ok answer ])
    use github = client fake
    let login, projects, problems = run (github.Projects())
    Assert.Equal("me", login)
    Assert.Equal<string list>([ "P1"; "P2" ], projects |> List.map _.Id)
    Assert.Equal("acme", projects.[1].Owner)
    Assert.Equal<string list>([ "Resource protected by organization SAML enforcement." ], problems)

[<Fact>]
let ``a missing scope says how to add it`` () =
    let answer =
        """{"errors":[{"type":"INSUFFICIENT_SCOPES","message":"Your token has not been granted the required scopes to execute this query. The 'projectsV2' field requires one of the following scopes: ['read:project']"}]}"""

    let fake = new FakeGitHub([ asks "viewer", ok answer ])
    use github = client fake
    let e = Assert.Throws<GitHubError>(fun () -> run (github.Projects()) |> ignore)
    Assert.Contains("refresh -s read:project", e.Data0)

[<Fact>]
let ``bad credentials`` () =
    let fake = new FakeGitHub([ (fun _ -> true), (fun _ -> HttpStatusCode.Unauthorized, """{"message":"Bad credentials"}""") ])
    use github = client fake
    let e = Assert.Throws<GitHubError>(fun () -> run (github.Viewer()) |> ignore)
    Assert.StartsWith("401", e.Data0)

[<Fact>]
let ``a project board loads its fields once and every page of items`` () =
    let page (after: string option) =
        match after with
        | None ->
            """{"data":{"node":{"fields":{"nodes":<fields>},"items":{"totalCount":3,
            "pageInfo":{"hasNextPage":true,"endCursor":"c1"},
            "nodes":[<i1>,<i2>]}}}}"""
            |> fill
                [ "fields", fieldsJson
                  "i1", itemJson "I1" (Some "Todo") (issueJson "acme/api" 1 "OPEN")
                  "i2", itemJson "I2" (Some "Done") (issueJson "acme/api" 2 "CLOSED") ]
        | Some _ ->
            """{"data":{"node":{"items":{"totalCount":3,"pageInfo":{"hasNextPage":false,"endCursor":"c2"},
            "nodes":[<i3>]}}}}"""
            |> fill [ "i3", itemJson "I3" None (issueJson "acme/web" 3 "OPEN") ]

    let fake =
        new FakeGitHub([ asks "items(first", (fun r -> HttpStatusCode.OK, page (variable "after" r)) ])

    use github = client fake
    let board = run (github.Board("PVT_1", 100))
    Assert.Equal<string list>([ "acme/api#1"; "acme/api#2"; "acme/web#3" ], board.Items |> List.map _.Key)
    Assert.Equal(3, board.Total)
    Assert.Equal(4, board.Fields.Length)
    Assert.Equal<string list>([ "Todo"; "Done"; noStatus ], board.Items |> List.map _.Status)

    let withFields =
        fake.Requests |> List.map (fun r -> Json.prop "withFields" r.Variables |> Option.map (fun v -> v.GetBoolean()))

    Assert.Equal<bool option list>([ Some true; Some false ], withFields)
    Assert.Equal<string option list>([ None; Some "c1" ], fake.Requests |> List.map (variable "after"))

[<Fact>]
let ``a board stops at the item limit`` () =
    let answer =
        """{"data":{"node":{"fields":{"nodes":[]},"items":{"totalCount":50,"pageInfo":{"hasNextPage":true,"endCursor":"c"},
        "nodes":[<i1>,<i2>]}}}}"""
        |> fill
            [ "i1", itemJson "I1" None (issueJson "acme/api" 1 "OPEN")
              "i2", itemJson "I2" None (issueJson "acme/api" 2 "OPEN") ]

    let fake = new FakeGitHub([ asks "items(first", ok answer ])
    use github = client fake
    let board = run (github.Board("PVT_1", 2))
    Assert.Equal(2, board.Items.Length)
    Assert.Equal(50, board.Total)
    Assert.Equal(1, fake.Requests.Length)

[<Fact>]
let ``a search without a type searches issues and pull requests`` () =
    let answer (q: string) =
        let number = if q.Contains "is:pr" then 2 else 1

        """{"data":{"search":{"issueCount":1,"pageInfo":{"hasNextPage":false,"endCursor":null},"nodes":[<issue>]}}}"""
        |> fill [ "issue", issueJson "acme/api" number "OPEN" ]

    let fake =
        new FakeGitHub([ asks "search(", (fun r -> HttpStatusCode.OK, answer (variable "query" r |> Option.defaultValue "")) ])

    use github = client fake
    let items, total = run (github.Search("assignee:@me", 100))
    Assert.Equal(2, total)
    Assert.Equal(2, items.Length)
    Assert.Equal<string option list>([ Some "assignee:@me is:issue"; Some "assignee:@me is:pr" ], fake.Requests |> List.map (variable "query"))

    let typed = new FakeGitHub([ asks "search(", ok (answer "") ])
    use github = client typed
    run (github.Search("is:pr review-requested:@me", 100)) |> ignore
    Assert.Equal(1, typed.Requests.Length)

[<Fact>]
let ``every request the client sends is a query`` () =
    let fake = new FakeGitHub([ (fun _ -> true), ok """{"data":{"node":null,"viewer":{"login":"me"}}}""" ])
    use github = client fake
    run (github.Viewer()) |> ignore
    Assert.Throws<GitHubError>(fun () -> run (github.Content "I_1") |> ignore) |> ignore
    Assert.Throws<GitHubError>(fun () -> run (github.ContentByNumber("acme/api", 1)) |> ignore) |> ignore

    for r in fake.Requests do
        Assert.Equal("/graphql", r.Path)
        Assert.Equal<string list>([ "query" ], ReadOnly.operations r.Query)

[<Fact>]
let ``an issue opens by number`` () =
    let answer =
        """{"data":{"repository":{"issueOrPullRequest":<issue>}}}"""
        |> fill [ "issue", issueJson "acme/api" 9 "OPEN" ]
    let fake = new FakeGitHub([ asks "issueOrPullRequest", ok answer ])
    use github = client fake
    let raw = run (github.ContentByNumber("acme/api", 9))
    Assert.Equal(Some "Title 9", Json.str "title" raw)
    let r = fake.Requests.Head
    Assert.Equal(Some "acme", variable "owner" r)
    Assert.Equal(Some "api", variable "name" r)
