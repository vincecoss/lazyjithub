/// GitHub Markdown, as in issue bodies and comments, to styled blocks.
module LazyJithub.Markdown

open System
open System.Net
open System.Text.RegularExpressions
open Markdig
open Markdig.Extensions.EmphasisExtras
open Markdig.Extensions.Tables
open Markdig.Extensions.TaskLists
open Markdig.Syntax
open Markdig.Syntax.Inlines
open LazyJithub.Text

let private pipeline =
    MarkdownPipelineBuilder()
        .UsePipeTables()
        .UseTaskLists()
        .UseEmphasisExtras(EmphasisExtraOptions.Strikethrough)
        .UseAutoLinks()
        .Build()

let private parse (text: string) =
    Markdig.Markdown.Parse(text.Replace("\r\n", "\n"), pipeline)

let private emojis =
    Map.ofList
        [ ":warning:", "⚠️"
          ":white_check_mark:", "✅"
          ":heavy_check_mark:", "✔️"
          ":x:", "❌"
          ":information_source:", "ℹ️"
          ":bulb:", "💡"
          ":rocket:", "🚀"
          ":fire:", "🔥"
          ":bug:", "🐛"
          ":question:", "❓"
          ":exclamation:", "❗"
          ":+1:", "👍"
          ":thumbsup:", "👍"
          ":-1:", "👎"
          ":thumbsdown:", "👎"
          ":smile:", "😄"
          ":tada:", "🎉"
          ":eyes:", "👀"
          ":pushpin:", "📌"
          ":memo:", "📝"
          ":sparkles:", "✨"
          ":star:", "⭐" ]

let private emojiRx = Regex(@":[a-z0-9_+-]+:", RegexOptions.Compiled)

let private withEmojis (text: string) =
    emojiRx.Replace(text, fun m -> Map.tryFind m.Value emojis |> Option.defaultValue m.Value)

/// `@login`, `#12` and `owner/repo#12`, which GitHub links in plain text.
let private refRx =
    Regex(@"(?<![\w@/])@[A-Za-z0-9][A-Za-z0-9-]*(?:/[\w.-]+)?|(?<![\w/#&])(?:[\w.-]+/[\w.-]+)?#\d+\b", RegexOptions.Compiled)

let private issueUrlRx =
    Regex(@"^https?://[^/]+/([\w.-]+/[\w.-]+)/(?:issues|pull)/(\d+)/?(?:[#?].*)?$", RegexOptions.Compiled)

let private highlightRefs (s: Segment) : Segment list =
    let matches = refRx.Matches s.Text

    if matches.Count = 0 then
        [ s ]
    else
        let mutable last = 0

        [ for m in matches do
              if m.Index > last then
                  { s with Text = s.Text.Substring(last, m.Index - last) }

              { s with
                  Text = m.Value
                  Color = Some(if m.Value.StartsWith "@" then Palette.lavender else Palette.blue)
                  Style = Bold }

              last <- m.Index + m.Length
          if last < s.Text.Length then
              { s with Text = s.Text.Substring last } ]

let private link (url: string) (text: string) : Segment list =
    let m = issueUrlRx.Match url

    if m.Success && (text = url || text = "") then
        [ boldColored Palette.blue $"{m.Groups.[1].Value}#{m.Groups.[2].Value}" ]
    else
        [ { seg (if text = "" then url else text) with
              Color = Some Palette.link
              Style = Underline } ]

let private tagRx = Regex(@"<[^>]*>", RegexOptions.Compiled)
let private commentRx = Regex(@"<!--[\s\S]*?(-->|$)", RegexOptions.Compiled)
let private imgRx = Regex(@"<img\b[^>]*>", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let private altRx = Regex(@"\balt\s*=\s*""([^""]*)""", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let private breakRx = Regex(@"<br\s*/?>", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let private summaryRx = Regex(@"<summary>([\s\S]*?)</summary>", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let private imageLabel (tag: string) =
    let alt = altRx.Match tag
    let name = if alt.Success && alt.Groups.[1].Value.Trim() <> "" then alt.Groups.[1].Value else "image"
    $"▣ {name}"

/// Raw HTML as GitHub shows it: comments hidden, images named, other tags dropped.
let private htmlText (html: string) =
    let text = commentRx.Replace(html, "")
    let text = imgRx.Replace(text, MatchEvaluator(fun m -> imageLabel m.Value))
    let text = breakRx.Replace(text, "\n")
    WebUtility.HtmlDecode(tagRx.Replace(text, ""))

let private literal (text: string) = highlightRefs (seg (withEmojis text))

let private styled (style: Style) (segments: Segment list) =
    segments
    |> List.map (fun s ->
        match s.Style, s.Color with
        | Plain, None
        | Dim, None -> { s with Style = style }
        | _ -> s)

let rec private inlineSegments (node: Inline) : Segment list =
    match node with
    | :? LiteralInline as l -> literal (l.Content.ToString())
    | :? CodeInline as c ->
        [ { seg c.Content with
              Color = Some Palette.code
              Background = Some Palette.codeBackground } ]
    | :? LinkInline as l when l.IsImage ->
        let alt = children l |> segmentsText
        [ colored Palette.teal $"""▣ {(if alt = "" then "image" else alt)}""" ]
    | :? LinkInline as l -> link (if isNull l.Url then "" else l.Url) (children l |> segmentsText)
    | :? AutolinkInline as a -> link a.Url ""
    | :? LineBreakInline -> [ seg "\n" ]
    | :? HtmlEntityInline as h -> [ seg (h.Transcoded.ToString()) ]
    | :? HtmlInline as h when breakRx.IsMatch h.Tag -> [ seg "\n" ]
    | :? HtmlInline as h when imgRx.IsMatch h.Tag -> [ colored Palette.teal (imageLabel h.Tag) ]
    | :? HtmlInline -> []
    | :? TaskList -> []
    | :? EmphasisInline as e ->
        let style =
            match e.DelimiterChar, e.DelimiterCount with
            | '~', _ -> Strike
            | _, 1 -> Italic
            | _ -> Bold

        children e |> styled style
    | :? ContainerInline as c -> children c
    | _ -> []

and private children (c: ContainerInline) : Segment list =
    [ for child in c -> child ] |> List.collect inlineSegments

let private inlines (leaf: LeafBlock) =
    if isNull leaf.Inline then [] else children leaf.Inline

let private paragraph (segments: Segment list) =
    if (segmentsText segments).Trim() = "" then [] else [ Rich.Para segments ]

let private alerts =
    [ "NOTE", Palette.blue
      "TIP", Palette.green
      "IMPORTANT", Palette.purple
      "WARNING", Palette.yellow
      "CAUTION", Palette.red ]

/// Removes the first `n` characters of `segments`, and the line break that follows them.
let private dropChars (n: int) (segments: Segment list) =
    let rec go n (rest: Segment list) =
        match rest with
        | s :: tail when n >= s.Text.Length -> go (n - s.Text.Length) tail
        | s :: tail -> { s with Text = s.Text.Substring n } :: tail
        | [] -> []

    match go n segments with
    | s :: tail when s.Text.StartsWith "\n" -> { s with Text = s.Text.Substring 1 } :: tail
    | rest -> rest

let private isTask (item: ListItemBlock) =
    match Seq.tryHead item with
    | Some(:? ParagraphBlock as p) when not (isNull p.Inline) ->
        match p.Inline.FirstChild with
        | :? TaskList as t -> Some t.Checked
        | _ -> None
    | _ -> None

let rec private blocksOf (depth: int) (block: Block) : Rich.Block list =
    match block with
    | :? HeadingBlock as h -> [ Rich.Sub [ boldColored Palette.teal (segmentsText (inlines h)) ] ]
    | :? ParagraphBlock as p -> paragraph (inlines p)
    | :? ListBlock as l ->
        let start =
            match Int32.TryParse(if isNull l.OrderedStart then "1" else l.OrderedStart) with
            | true, n -> n
            | _ -> 1

        [ for n, child in Seq.indexed l do
              match child with
              | :? ListItemBlock as item ->
                  let marker =
                      match isTask item with
                      | Some true -> colored Palette.green "☑ "
                      | Some false -> dim "☐ "
                      | None when l.IsOrdered -> colored Palette.blue $"{start + n}. "
                      | None -> colored Palette.blue (if depth % 2 = 0 then "• " else "◦ ")

                  let body = [ for b in item -> b ] |> List.collect (blocksOf (depth + 1))
                  Rich.Item(marker, body)
              | other -> yield! blocksOf depth other ]
    | :? CodeBlock as c ->
        let text = c.Lines.ToString().TrimEnd('\n')
        [ Rich.Code(text.Split('\n') |> Array.toList) ]
    | :? QuoteBlock as q ->
        let body = [ for b in q -> b ] |> List.collect (blocksOf depth)

        let alert =
            match body with
            | Rich.Para first :: rest ->
                let text = segmentsText first

                alerts
                |> List.tryPick (fun (name, color) ->
                    let marker = $"[!{name}]"

                    if text.StartsWith(marker, StringComparison.OrdinalIgnoreCase) then
                        let label = name.Substring(0, 1) + name.Substring(1).ToLowerInvariant()

                        Some(
                            Rich.Para [ boldColored color label ]
                            :: (paragraph (dropChars marker.Length first) @ rest)
                        )
                    else
                        None)
            | _ -> None

        [ Rich.Quote(defaultArg alert body) ]
    | :? ThematicBreakBlock -> [ Rich.Rule ]
    | :? Table as t ->
        let cell (c: TableCell) =
            [ for b in c -> b ]
            |> List.collect (function
                | :? LeafBlock as leaf -> inlines leaf
                | _ -> [])
            |> List.map (fun s -> { s with Text = s.Text.Replace("\n", " ") })

        let rows =
            [ for r in t do
                  match r with
                  | :? TableRow as row -> [ for c in row -> c ] |> List.choose (function :? TableCell as c -> Some(cell c) | _ -> None)
                  | _ -> () ]

        if rows.IsEmpty then [] else [ Rich.Table rows ]
    | :? HtmlBlock as h ->
        let raw = h.Lines.ToString()

        let summaries =
            [ for m in summaryRx.Matches raw -> Rich.Sub [ bold ("▸ " + (htmlText m.Groups.[1].Value).Trim()) ] ]

        let rest = htmlText (summaryRx.Replace(raw, ""))

        summaries
        @ (rest.Split('\n')
           |> Array.map _.Trim()
           |> Array.filter ((<>) "")
           |> Array.map (fun line ->
               if line.StartsWith "▣ " then Rich.Para [ colored Palette.teal line ] else Rich.Para(literal line))
           |> Array.toList)
    | :? LinkReferenceDefinitionGroup -> []
    | :? ContainerBlock as c -> [ for b in c -> b ] |> List.collect (blocksOf depth)
    | :? LeafBlock as leaf -> paragraph (inlines leaf)
    | _ -> []

let toBlocks (text: string) : Rich.Block list =
    if String.IsNullOrWhiteSpace text then
        []
    else
        [ for b in parse text -> b ] |> List.collect (blocksOf 0)

let rec private descendants (block: Block) : Inline seq =
    seq {
        match block with
        | :? ContainerBlock as c ->
            for b in c do
                yield! descendants b
        | :? LeafBlock as leaf when not (isNull leaf.Inline) -> yield! inlineTree leaf.Inline
        | _ -> ()
    }

and private inlineTree (node: Inline) : Inline seq =
    seq {
        yield node

        match node with
        | :? ContainerInline as c ->
            for child in c do
                yield! inlineTree child
        | _ -> ()
    }

/// Issues the text points at, as `owner/repo#12` keys; `repo` completes a bare `#12`.
/// Code is left out, as GitHub does.
let references (repo: string) (text: string) : string list =
    if String.IsNullOrWhiteSpace text then
        []
    else
        let doc = parse text

        [ for node in doc |> Seq.collect descendants do
              match node with
              | :? LiteralInline as l ->
                  for m in refRx.Matches(l.Content.ToString()) do
                      if not (m.Value.StartsWith "@") then
                          match m.Value.Split('#') with
                          | [| ""; n |] when repo <> "" -> $"{repo}#{n}"
                          | [| r; n |] when r <> "" -> $"{r}#{n}"
                          | _ -> ()
              | :? LinkInline as l when not l.IsImage && not (isNull l.Url) ->
                  let m = issueUrlRx.Match l.Url
                  if m.Success then $"{m.Groups.[1].Value}#{m.Groups.[2].Value}"
              | :? AutolinkInline as a ->
                  let m = issueUrlRx.Match a.Url
                  if m.Success then $"{m.Groups.[1].Value}#{m.Groups.[2].Value}"
              | _ -> () ]
        |> List.distinct

/// Web links of the text, images and issue links left out.
let urls (text: string) : string list =
    if String.IsNullOrWhiteSpace text then
        []
    else
        [ for node in parse text |> Seq.collect descendants do
              match node with
              | :? LinkInline as l when not l.IsImage && not (isNull l.Url) -> l.Url
              | :? AutolinkInline as a -> a.Url
              | _ -> () ]
        |> List.filter (fun u -> u.StartsWith "http" && not (issueUrlRx.IsMatch u))
        |> List.distinct
