module LazyJithub.Tests.ReadOnlyTests

open System
open System.Net.Http
open Xunit
open LazyJithub.ReadOnly
open LazyJithub.Tests.FakeGitHub

let private graphql = Uri "https://api.github.com/graphql"
let private body (query: string) = Text.Json.JsonSerializer.Serialize(dict [ "query", query ])

[<Theory>]
[<InlineData("query { viewer { login } }")>]
[<InlineData("{ viewer { login } }")>]
[<InlineData("query Q($id: ID!) { node(id: $id) { id } } fragment F on Issue { title }")>]
[<InlineData("query { search(query: \"mutation { x }\", type: ISSUE, first: 1) { issueCount } }")>]
[<InlineData("query { viewer { login } } # mutation { deleteIssue }")>]
[<InlineData("query($mutation: String) { viewer { login } }")>]
[<InlineData("query($x: In = {a: 1}) { viewer { mutation: login } }")>]
let ``queries are allowed`` (query: string) =
    Assert.True(isAllowed HttpMethod.Post graphql (body query))

[<Theory>]
[<InlineData("mutation { addComment(input: {}) { clientMutationId } }")>]
[<InlineData("mutation M { closeIssue(input: {}) { clientMutationId } }")>]
[<InlineData("query { viewer { login } } mutation { closeIssue(input: {}) { clientMutationId } }")>]
[<InlineData("subscription { x }")>]
[<InlineData("# just a comment")>]
[<InlineData("")>]
let ``mutations and subscriptions are refused`` (query: string) =
    Assert.False(isAllowed HttpMethod.Post graphql (body query))

[<Fact>]
let ``a body that is not a GraphQL request is refused`` () =
    Assert.False(isAllowed HttpMethod.Post graphql "not json")
    Assert.False(isAllowed HttpMethod.Post graphql """{"query": 12}""")
    Assert.False(isAllowed HttpMethod.Post graphql "")

[<Fact>]
let ``REST writes are refused, REST reads are allowed`` () =
    let issue = Uri "https://api.github.com/repos/o/r/issues/1"
    Assert.True(isAllowed HttpMethod.Get issue "")
    Assert.False(isAllowed HttpMethod.Patch issue """{"state":"closed"}""")
    Assert.False(isAllowed HttpMethod.Post (Uri "https://api.github.com/repos/o/r/issues") (body "query { x }"))
    Assert.False(isAllowed HttpMethod.Delete issue "")
    Assert.False(isAllowed HttpMethod.Put graphql (body "query { x }"))

[<Fact>]
let ``a mutation never reaches the network`` () =
    let fake = new FakeGitHub([ (fun _ -> true), ok """{"data":{}}""" ])
    use github = client fake

    let e =
        Assert.ThrowsAny<exn>(fun () ->
            github.Query("mutation { closeIssue(input: {issueId: \"x\"}) { clientMutationId } }", dict [])
                .GetAwaiter()
                .GetResult()
            |> ignore)

    Assert.Contains("only reads", e.Message)
    Assert.Empty fake.Requests
