module LazyJithub.GitHub

open System
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.Json
open System.Threading.Tasks
open LazyJithub.Json
open LazyJithub.Model
open LazyJithub.ReadOnly

exception GitHubError of string

let private projectFields =
    """
fragment ProjectFields on ProjectV2 {
  id number title url closed updatedAt
  owner { ... on User { login } ... on Organization { login } }
}"""

/// What the item list shows; the detail query asks for the rest.
let private listFields =
    """
fragment IssueRow on Issue {
  id number title state stateReason url createdAt updatedAt closedAt
  repository { nameWithOwner }
  author { login }
  assignees(first: 10) { nodes { login } }
  labels(first: 20) { nodes { name color } }
  milestone { title }
  issueType { name }
  parent { number title repository { nameWithOwner } }
  subIssuesSummary { total completed }
}
fragment PullRow on PullRequest {
  id number title state isDraft url createdAt updatedAt closedAt mergedAt
  repository { nameWithOwner }
  author { login }
  assignees(first: 10) { nodes { login } }
  labels(first: 20) { nodes { name color } }
  milestone { title }
}"""

/// Spreads the row fragments, so it repeats none of their fields with other arguments: GraphQL refuses that.
let private detailFields =
    """
fragment IssueDetail on Issue {
  ...IssueRow
  body
  milestone { title dueOn }
  parent { number title state url repository { nameWithOwner } }
  subIssues(first: 50) {
    totalCount
    nodes { number title state stateReason url repository { nameWithOwner } assignees(first: 3) { nodes { login } } }
  }
  closedByPullRequestsReferences(first: 10, includeClosedPrs: true) {
    nodes { number title state isDraft url repository { nameWithOwner } }
  }
  comments(last: 30) { totalCount nodes { author { login } body createdAt } }
}
fragment PullDetail on PullRequest {
  ...PullRow
  body
  milestone { title dueOn }
  baseRefName headRefName additions deletions changedFiles reviewDecision
  reviewRequests(first: 10) { nodes { requestedReviewer { ... on User { login } ... on Team { name } } } }
  latestReviews(first: 10) { nodes { author { login } state } }
  closingIssuesReferences(first: 10) { nodes { number title state url repository { nameWithOwner } } }
  commits(last: 1) { nodes { commit { statusCheckRollup { state } } } }
  comments(last: 30) { totalCount nodes { author { login } body createdAt } }
}"""

let private draftFields =
    """
fragment DraftDetail on DraftIssue {
  id title body createdAt updatedAt
  creator { login }
  assignees(first: 20) { nodes { login } }
}"""

let viewerQuery =
    """query {
  viewer {
    login
    projectsV2(first: 100, orderBy: { field: UPDATED_AT, direction: DESC }) { nodes { ...ProjectFields } }
    organizations(first: 100) {
      nodes { login projectsV2(first: 100, orderBy: { field: UPDATED_AT, direction: DESC }) { nodes { ...ProjectFields } } }
    }
  }
}"""
    + projectFields

let projectQuery =
    """query($owner: String!, $number: Int!) {
  organization(login: $owner) { projectV2(number: $number) { ...ProjectFields } }
  user(login: $owner) { projectV2(number: $number) { ...ProjectFields } }
}"""
    + projectFields

let itemsQuery =
    """query($id: ID!, $first: Int!, $after: String, $withFields: Boolean!) {
  node(id: $id) {
    ... on ProjectV2 {
      fields(first: 50) @include(if: $withFields) {
        nodes {
          ... on ProjectV2FieldCommon { id name dataType }
          ... on ProjectV2SingleSelectField { options { id name color } }
          ... on ProjectV2IterationField {
            configuration {
              iterations { id title startDate duration }
              completedIterations { id title startDate duration }
            }
          }
        }
      }
      items(first: $first, after: $after) {
        totalCount
        pageInfo { hasNextPage endCursor }
        nodes {
          id type isArchived createdAt updatedAt
          fieldValues(first: 20) {
            nodes {
              __typename
              ... on ProjectV2ItemFieldSingleSelectValue { name optionId field { ... on ProjectV2FieldCommon { name } } }
              ... on ProjectV2ItemFieldIterationValue { title iterationId field { ... on ProjectV2FieldCommon { name } } }
              ... on ProjectV2ItemFieldTextValue { text field { ... on ProjectV2FieldCommon { name } } }
              ... on ProjectV2ItemFieldNumberValue { number field { ... on ProjectV2FieldCommon { name } } }
              ... on ProjectV2ItemFieldDateValue { date field { ... on ProjectV2FieldCommon { name } } }
            }
          }
          content {
            __typename
            ...IssueRow
            ...PullRow
            ... on DraftIssue { id title createdAt updatedAt creator { login } assignees(first: 5) { nodes { login } } }
          }
        }
      }
    }
  }
}"""
    + listFields

let searchQuery =
    """query($query: String!, $first: Int!, $after: String) {
  search(query: $query, type: ISSUE, first: $first, after: $after) {
    issueCount
    pageInfo { hasNextPage endCursor }
    nodes { __typename ...IssueRow ...PullRow }
  }
}"""
    + listFields

let nodeQuery =
    """query($id: ID!) {
  node(id: $id) { __typename ...IssueDetail ...PullDetail ...DraftDetail }
}"""
    + listFields
    + detailFields
    + draftFields

let numberQuery =
    """query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    issueOrPullRequest(number: $number) { __typename ...IssueDetail ...PullDetail }
  }
}"""
    + listFields
    + detailFields

let private scopeHint (message: string) =
    if message.Contains "read:project" && not (message.Contains "gh auth refresh") then
        message + " Run: gh auth refresh -s read:project"
    else
        message

let private errorMessages (body: JsonElement) =
    arr "errors" body |> List.choose (str "message") |> List.map scopeHint

/// GitHub search wants to know whether it looks for issues or pull requests.
let hasType (query: string) =
    query.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.exists (fun w ->
        let w = w.ToLowerInvariant()
        w = "is:issue" || w = "is:pr" || w = "is:pull-request" || w.StartsWith "type:")

type Board =
    { Fields: Field list
      Items: Item list
      Total: int }

let private noVariables: Collections.Generic.IDictionary<string, obj> = dict []

type GitHubClient(apiUrl: string, token: string, inner: HttpMessageHandler, ?pageSize: int) =
    let apiUrl = apiUrl.TrimEnd('/')
    let pageSize = defaultArg pageSize 100

    let http =
        let client = new HttpClient(new ReadOnlyGuard(inner), BaseAddress = Uri(apiUrl + "/"))
        client.Timeout <- TimeSpan.FromSeconds 60.0
        client.DefaultRequestHeaders.Accept.Add(MediaTypeWithQualityHeaderValue "application/json")
        client.DefaultRequestHeaders.UserAgent.ParseAdd "lazyjithub/0.1"
        client.DefaultRequestHeaders.Authorization <- AuthenticationHeaderValue("Bearer", token)
        client

    let post (document: string) (variables: obj) : Task<JsonElement> =
        task {
            let request = new HttpRequestMessage(HttpMethod.Post, "graphql")
            let body = JsonSerializer.Serialize(dict [ "query", box document; "variables", variables ])
            request.Content <- new StringContent(body, Encoding.UTF8, "application/json")

            let! response =
                task {
                    try
                        return! http.SendAsync request
                    with
                    | ReadOnlyViolation msg -> return raise (GitHubError msg)
                    | :? HttpRequestException as e -> return raise (GitHubError $"{apiUrl}: {e.Message}")
                    | :? TaskCanceledException -> return raise (GitHubError $"{apiUrl}: timed out")
                }

            use response = response
            let! text = response.Content.ReadAsStringAsync()
            let status = int response.StatusCode

            let parsed =
                try
                    Some(Json.parse text)
                with _ ->
                    None

            if status = 401 then
                raise (GitHubError "401: bad credentials; check the token, or run: gh auth login")
            elif status >= 400 then
                let message =
                    parsed
                    |> Option.bind (str "message")
                    |> Option.defaultValue (if text.Length > 200 then text.Substring(0, 200) else text)

                raise (GitHubError $"{status}: {scopeHint message}")

            match parsed with
            | Some body -> return body
            | None -> return raise (GitHubError $"{apiUrl}: the answer is not JSON")
        }

    /// The `data` of the answer and the errors that came with it; fails when there is no data at all.
    let queryPartial (document: string) (variables: obj) : Task<JsonElement * string list> =
        task {
            let! body = post document variables
            let errors = errorMessages body

            match prop "data" body with
            | Some data -> return data, errors
            | None ->
                let message = if errors.IsEmpty then "no data in the answer" else String.Join("; ", errors)
                return raise (GitHubError message)
        }

    /// Fails on any error.
    let query (document: string) (variables: obj) : Task<JsonElement> =
        task {
            let! data, errors = queryPartial document variables

            if not errors.IsEmpty then
                raise (GitHubError(String.Join("; ", errors)))

            return data
        }

    new(apiUrl, token) = new GitHubClient(apiUrl, token, new SocketsHttpHandler())

    member _.Query(document, variables) = query document variables

    /// The login of the token owner.
    member _.Viewer() : Task<string> =
        task {
            let! data = query "query { viewer { login } }" noVariables
            return pathStr [ "viewer"; "login" ] data |> Option.defaultValue ""
        }

    /// The projects of the token owner and of their organizations, with a warning per organization
    /// GitHub refused to list, such as one that wants SAML single sign-on first.
    member _.Projects() : Task<string * Project list * string list> =
        task {
            let! data, errors = queryPartial viewerQuery noVariables
            let viewer = prop "viewer" data

            let own =
                viewer |> Option.map (nodes "projectsV2") |> Option.defaultValue []

            let orgs =
                viewer
                |> Option.map (nodes "organizations")
                |> Option.defaultValue []
                |> List.collect (nodes "projectsV2")

            let projects =
                own @ orgs
                |> List.map Project.ofJson
                |> List.filter (fun p -> p.Id <> "")
                |> List.distinctBy _.Id

            let login = viewer |> Option.bind (str "login") |> Option.defaultValue ""
            return login, projects, errors |> List.distinct
        }

    member _.Project(owner: string, number: int) : Task<Project> =
        task {
            let! data, errors = queryPartial projectQuery (dict [ "owner", box owner; "number", box number ])

            let found =
                [ "organization"; "user" ]
                |> List.tryPick (fun k -> path [ k; "projectV2" ] data)
                |> Option.map Project.ofJson

            match found with
            | Some p -> return p
            | None ->
                let reason = if errors.IsEmpty then "" else ": " + String.Join("; ", errors |> List.distinct)
                return raise (GitHubError $"project {owner}/{number} not found{reason}")
        }

    /// The fields and the items of a project, at most `limit` items, in project order.
    member _.Board(projectId: string, limit: int) : Task<Board> =
        task {
            let mutable fields = []
            let found = ResizeArray<JsonElement>()
            let mutable after: string = null
            let mutable total = 0
            let mutable more = true

            while more do
                let! data =
                    query
                        itemsQuery
                        (dict
                            [ "id", box projectId
                              "first", box (min pageSize (max 1 (limit - found.Count)))
                              "after", box after
                              "withFields", box (found.Count = 0) ])

                match prop "node" data with
                | None -> raise (GitHubError "project not found, or the token cannot see it")
                | Some node ->
                    if found.Count = 0 then
                        fields <- nodes "fields" node |> List.map Field.ofJson |> List.filter (fun f -> f.Id <> "")

                    let items = prop "items" node
                    let page = items |> Option.map (arr "nodes") |> Option.defaultValue []
                    found.AddRange page
                    total <- items |> Option.bind (num "totalCount") |> Option.defaultValue found.Count
                    after <- items |> Option.bind (pathStr [ "pageInfo"; "endCursor" ]) |> Option.toObj

                    more <-
                        found.Count < limit
                        && not page.IsEmpty
                        && items |> Option.bind (path [ "pageInfo"; "hasNextPage" ]) |> Option.exists (fun v -> v.ValueKind = JsonValueKind.True)

            let status = Field.status fields |> Option.map _.Name

            return
                { Fields = fields
                  Items = found |> Seq.choose (Item.ofProjectItem status) |> List.ofSeq
                  Total = total }
        }

    /// Issues and pull requests matching a GitHub search; without is:issue or is:pr, both are searched.
    member _.Search(text: string, limit: int) : Task<Item list * int> =
        task {
            let one (q: string) =
                task {
                    let found = ResizeArray<JsonElement>()
                    let mutable after: string = null
                    let mutable total = 0
                    let mutable more = true

                    while more do
                        let! data =
                            query
                                searchQuery
                                (dict
                                    [ "query", box q
                                      "first", box (min pageSize (max 1 (limit - found.Count)))
                                      "after", box after ])

                        let search = prop "search" data
                        let page = search |> Option.map (arr "nodes") |> Option.defaultValue []
                        found.AddRange page
                        total <- search |> Option.bind (num "issueCount") |> Option.defaultValue found.Count
                        after <- search |> Option.bind (pathStr [ "pageInfo"; "endCursor" ]) |> Option.toObj

                        more <-
                            found.Count < limit
                            && not page.IsEmpty
                            && search |> Option.bind (path [ "pageInfo"; "hasNextPage" ]) |> Option.exists (fun v -> v.ValueKind = JsonValueKind.True)

                    return found |> Seq.choose (fun c -> Item.ofContent c None None) |> List.ofSeq, total
                }

            if hasType text then
                return! one text
            else
                let! issues, issueCount = one (text + " is:issue")
                let! pulls, pullCount = one (text + " is:pr")

                let merged =
                    issues @ pulls |> List.sortByDescending _.Updated |> List.truncate limit

                return merged, issueCount + pullCount
        }

    /// The issue, pull request or draft issue with this node id, with its body and comments.
    member _.Content(id: string) : Task<JsonElement> =
        task {
            let! data = query nodeQuery (dict [ "id", box id ])

            match prop "node" data with
            | Some node -> return node
            | None -> return raise (GitHubError "not found")
        }

    member _.ContentByNumber(repo: string, number: int) : Task<JsonElement> =
        task {
            match repo.Split('/') with
            | [| owner; name |] ->
                let! data, errors =
                    queryPartial numberQuery (dict [ "owner", box owner; "name", box name; "number", box number ])

                match path [ "repository"; "issueOrPullRequest" ] data with
                | Some node -> return node
                | None ->
                    let reason = if errors.IsEmpty then "not found" else String.Join("; ", errors)
                    return raise (GitHubError $"{repo}#{number}: {reason}")
            | _ -> return raise (GitHubError $"{repo} is not owner/name")
        }

    interface IDisposable with
        member _.Dispose() = http.Dispose()
