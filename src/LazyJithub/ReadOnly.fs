/// lazyjithub changes nothing on GitHub. Every request goes through this guard, which lets
/// GraphQL queries and REST reads through and refuses the rest before it leaves the machine.
module LazyJithub.ReadOnly

open System
open System.Net.Http
open System.Text.Json
open System.Threading.Tasks

exception ReadOnlyViolation of string

/// The keyword of each operation in a GraphQL document: "query", "mutation" or "subscription".
/// Strings and comments are skipped, and only names outside any braces or parentheses count,
/// so a field or argument named "mutation" is not taken for one.
let operations (document: string) : string list =
    let found = ResizeArray<string>()
    let mutable i = 0
    let mutable braces = 0
    let mutable parens = 0
    let n = document.Length
    let isName (c: char) = Char.IsLetterOrDigit c || c = '_'

    while i < n do
        let c = document.[i]

        if c = '#' then
            while i < n && document.[i] <> '\n' do
                i <- i + 1
        elif c = '"' then
            if i + 2 < n && document.[i + 1] = '"' && document.[i + 2] = '"' then
                let close = document.IndexOf("\"\"\"", i + 3, StringComparison.Ordinal)
                i <- if close < 0 then n else close + 3
            else
                i <- i + 1

                while i < n && document.[i] <> '"' do
                    i <- i + (if document.[i] = '\\' then 2 else 1)

                i <- i + 1
        elif c = '{' then
            if braces = 0 && parens = 0 && (found.Count = 0 || found.[found.Count - 1] = "}") then
                found.Add "query"

            braces <- braces + 1
            i <- i + 1
        elif c = '}' then
            braces <- braces - 1

            if braces = 0 && parens = 0 then
                found.Add "}"

            i <- i + 1
        elif c = '(' then
            parens <- parens + 1
            i <- i + 1
        elif c = ')' then
            parens <- parens - 1
            i <- i + 1
        elif c = '$' then
            i <- i + 1

            while i < n && isName document.[i] do
                i <- i + 1
        elif isName c then
            let start = i

            while i < n && isName document.[i] do
                i <- i + 1

            let word = document.Substring(start, i - start)

            if braces = 0 && parens = 0 then
                match word with
                | "query"
                | "mutation"
                | "subscription"
                | "fragment" -> found.Add word
                | _ -> ()
        else
            i <- i + 1

    found |> Seq.filter (fun w -> w <> "}" && w <> "fragment") |> List.ofSeq

/// A GraphQL request body that holds only queries.
let isQuery (body: string) : bool =
    try
        use doc = JsonDocument.Parse body
        let root = doc.RootElement

        match root.ValueKind, root.TryGetProperty "query" with
        | JsonValueKind.Object, (true, q) when q.ValueKind = JsonValueKind.String ->
            match operations (q.GetString()) with
            | [] -> false
            | kinds -> kinds |> List.forall ((=) "query")
        | _ -> false
    with _ ->
        false

let isAllowed (method: HttpMethod) (uri: Uri) (body: string) : bool =
    if method = HttpMethod.Get || method = HttpMethod.Head || method = HttpMethod.Options then
        true
    elif method = HttpMethod.Post && uri.AbsolutePath.TrimEnd('/').EndsWith "/graphql" then
        isQuery body
    else
        false

type ReadOnlyGuard(inner: HttpMessageHandler) =
    inherit DelegatingHandler(inner)

    override _.SendAsync(request, cancellationToken) : Task<HttpResponseMessage> =
        let body =
            match request.Content with
            | null -> ""
            | content -> content.ReadAsStringAsync().GetAwaiter().GetResult()

        if isAllowed request.Method request.RequestUri body then
            base.SendAsync(request, cancellationToken)
        else
            Task.FromException<HttpResponseMessage>(
                ReadOnlyViolation
                    $"refused {request.Method} {request.RequestUri.AbsolutePath}: lazyjithub only reads GitHub"
            )
