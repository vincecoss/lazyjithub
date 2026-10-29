module LazyJithub.Tests.FakeGitHub

open System
open System.Collections.Concurrent
open System.Net
open System.Net.Http
open System.Text
open System.Text.Json

type Recorded =
    { Method: HttpMethod
      Path: string
      Query: string
      Variables: JsonElement }

/// An in-memory GitHub GraphQL API: answers with the first route whose test matches the request,
/// and records every request that reaches the network.
type FakeGitHub(routes: ((Recorded -> bool) * (Recorded -> HttpStatusCode * string)) list) =
    inherit HttpMessageHandler()

    let requests = ConcurrentQueue<Recorded>()

    member _.Requests = requests |> Seq.toList

    override _.SendAsync(request, _) =
        task {
            let body =
                match request.Content with
                | null -> "{}"
                | content -> content.ReadAsStringAsync().Result

            let json = LazyJithub.Json.parse body

            let recorded =
                { Method = request.Method
                  Path = request.RequestUri.AbsolutePath
                  Query = LazyJithub.Json.strOr "" "query" json
                  Variables = LazyJithub.Json.prop "variables" json |> Option.defaultValue (LazyJithub.Json.parse "{}") }

            requests.Enqueue recorded

            let status, answer =
                match routes |> List.tryFind (fun (test, _) -> test recorded) with
                | Some(_, respond) -> respond recorded
                | None -> HttpStatusCode.OK, """{"errors":[{"message":"no route"}]}"""

            return new HttpResponseMessage(status, Content = new StringContent(answer, Encoding.UTF8, "application/json"))
        }

let ok (json: string) = fun (_: Recorded) -> HttpStatusCode.OK, json
let asks (text: string) = fun (r: Recorded) -> r.Query.Contains text

let variable (name: string) (r: Recorded) = LazyJithub.Json.str name r.Variables

let client (fake: FakeGitHub) =
    new LazyJithub.GitHub.GitHubClient("https://api.github.com", "token", fake, 2)

/// Replaces each `<name>` of the template; JSON braces make interpolated strings unreadable.
let fill (pairs: (string * string) list) (template: string) =
    pairs |> List.fold (fun (t: string) (k, v) -> t.Replace($"<{k}>", v)) template

let issueJson (repo: string) (number: int) (state: string) =
    """{"__typename":"Issue","id":"I_<n>","number":<n>,"title":"Title <n>",
    "state":"<state>","stateReason":null,"url":"https://github.com/<repo>/issues/<n>",
    "createdAt":"2026-09-01T10:00:00Z","updatedAt":"2026-10-01T10:00:00Z","closedAt":<closed>,
    "repository":{"nameWithOwner":"<repo>"},"author":{"login":"alice"},
    "assignees":{"nodes":[{"login":"alice"}]},
    "labels":{"nodes":[{"name":"bug","color":"d73a4a"}]},
    "milestone":{"title":"v1"},"issueType":{"name":"Bug"},
    "parent":null,"subIssuesSummary":{"total":0,"completed":0}}"""
    |> fill
        [ "n", string number
          "repo", repo
          "state", state
          "closed", (if state = "CLOSED" then "\"2026-10-02T10:00:00Z\"" else "null") ]

let itemJson (id: string) (status: string option) (content: string) =
    let statusValue =
        match status with
        | Some s ->
            """,{"__typename":"ProjectV2ItemFieldSingleSelectValue","name":"<s>","optionId":"opt-<s>","field":{"name":"Status"}}"""
            |> fill [ "s", s ]
        | None -> ""

    """{"id":"<id>","type":"ISSUE","isArchived":false,"createdAt":"2026-09-01T10:00:00Z","updatedAt":"2026-10-05T10:00:00Z",
    "fieldValues":{"nodes":[{"__typename":"ProjectV2ItemFieldTextValue","text":"Title","field":{"name":"Title"}},
    {"__typename":"ProjectV2ItemFieldSingleSelectValue","name":"High","optionId":"p1","field":{"name":"Priority"}}<status>]},
    "content":<content>}"""
    |> fill [ "id", id; "status", statusValue; "content", content ]

let fieldsJson =
    """[{"id":"F1","name":"Status","dataType":"SINGLE_SELECT","options":[
        {"id":"opt-Todo","name":"Todo","color":"GRAY"},
        {"id":"opt-In Progress","name":"In Progress","color":"YELLOW"},
        {"id":"opt-Done","name":"Done","color":"GREEN"}]},
      {"id":"F2","name":"Priority","dataType":"SINGLE_SELECT","options":[{"id":"p1","name":"High","color":"RED"},{"id":"p2","name":"Low","color":"BLUE"}]},
      {"id":"F3","name":"Sprint","dataType":"ITERATION","configuration":{
        "iterations":[{"id":"it2","title":"Sprint 2","startDate":"2026-10-26","duration":14}],
        "completedIterations":[{"id":"it1","title":"Sprint 1","startDate":"2026-10-12","duration":14}]}},
      {"id":"F4","name":"Title","dataType":"TITLE"}]"""
