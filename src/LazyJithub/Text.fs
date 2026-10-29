module LazyJithub.Text

open System

type Style =
    | Plain
    | Bold
    | Dim
    | Italic
    | Underline
    | BoldUnderline
    | Strike

type Segment =
    { Text: string
      Color: string option
      Style: Style
      Background: string option }

let seg text =
    { Text = text
      Color = None
      Style = Plain
      Background = None }

let colored color text = { seg text with Color = Some color }
let boldColored color text = { seg text with Color = Some color; Style = Bold }
let bold text = { seg text with Style = Bold }
let dim text = { seg text with Style = Dim }
let italic text = { seg text with Style = Italic }

module Palette =
    let blue = "#4c9aff"
    let green = "#57d9a3"
    let grey = "#b3bac5"
    let dimGrey = "#7f848e"
    let red = "#ff5630"
    let orange = "#ff7452"
    let yellow = "#ffab00"
    let teal = "#00b8d9"
    let purple = "#bf7fff"
    let lavender = "#998dd9"
    let darkGreen = "#36b37e"
    let code = "#e5c07b"
    let codeBackground = "#2c313a"
    let link = "#61afef"
    let text = "#c5c9d1"
    let matchForeground = "#1e1e1e"

let segmentsText (segments: Segment list) = String.Join("", segments |> List.map _.Text)

/// Word-wraps to `width` columns; a word longer than the width is cut.
let wrapLines (width: int) (text: string) : string list =
    let rec go (lines: string list) (current: string) (words: string list) =
        match words with
        | [] -> List.rev (current :: lines)
        | w :: rest when current = "" -> go lines w rest
        | w :: rest when current.Length + 1 + w.Length <= width -> go lines (current + " " + w) rest
        | w :: rest -> go (current :: lines) w rest

    if width <= 0 then
        []
    else
        text.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.collect (fun w -> if w.Length <= width then [ w ] else List.ofSeq (Seq.chunkBySize width w) |> List.map String)
        |> go [] ""
        |> List.filter ((<>) "")

/// Like `wrapLines`, kept to `rows` lines with an ellipsis on the last one.
let wrap (width: int) (rows: int) (text: string) =
    let lines = wrapLines width text

    if lines.Length <= rows then
        lines
    else
        let last = lines.[rows - 1]
        let cut = if last.Length >= width then last.Substring(0, width - 1) else last
        List.truncate (rows - 1) lines @ [ cut + "…" ]

type private Token =
    | Word of Segment list
    | Space
    | Break

/// Text glued across segments, like "**bold**,", stays one word so it never splits at a style change.
let private tokenize (segments: Segment list) =
    let tokens = ResizeArray<Token>()
    let word = ResizeArray<Segment>()

    let flush () =
        if word.Count > 0 then
            tokens.Add(Word(List.ofSeq word))
            word.Clear()

    for s in segments do
        let mutable start = 0

        for i in 0 .. s.Text.Length - 1 do
            match s.Text.[i] with
            | ' '
            | '\n' as c ->
                if i > start then word.Add { s with Text = s.Text.Substring(start, i - start) }
                flush ()
                tokens.Add(if c = '\n' then Break else Space)
                start <- i + 1
            | _ -> ()

        if start < s.Text.Length then
            word.Add { s with Text = s.Text.Substring start }

    flush ()
    List.ofSeq tokens

let private splitWord (width: int) (word: Segment list) =
    let rec take (n: int) (taken: Segment list) (rest: Segment list) =
        match rest with
        | [] -> List.rev taken, []
        | s :: tail when s.Text.Length <= n -> take (n - s.Text.Length) (s :: taken) tail
        | s :: tail when n > 0 ->
            List.rev ({ s with Text = s.Text.Substring(0, n) } :: taken), { s with Text = s.Text.Substring n } :: tail
        | _ -> List.rev taken, rest

    take width [] word

/// Greedy word wrap that keeps each word's style; a "\n" in a segment forces a break.
let wrapSegments (width: int) (segments: Segment list) : Segment list list =
    if width <= 0 then
        []
    else
        let lines = ResizeArray<Segment list>()
        let current = ResizeArray<Segment>()
        let mutable used = 0
        let mutable pendingSpace = false

        let newLine () =
            lines.Add(List.ofSeq current)
            current.Clear()
            used <- 0
            pendingSpace <- false

        let rec place (word: Segment list) =
            let length = segmentsText word |> String.length

            if used = 0 && length > width then
                let head, rest = splitWord width word
                current.AddRange head
                used <- width
                newLine ()
                place rest
            elif used > 0 && used + (if pendingSpace then 1 else 0) + length > width then
                newLine ()
                place word
            elif length > 0 then
                if pendingSpace && used > 0 then
                    current.Add(seg " ")
                    used <- used + 1

                current.AddRange word
                used <- used + length
                pendingSpace <- false

        for token in tokenize segments do
            match token with
            | Word w -> place w
            | Space -> pendingSpace <- true
            | Break -> newLine ()

        if current.Count > 0 || lines.Count = 0 then
            newLine ()

        List.ofSeq lines

/// Splits segments around case-insensitive matches of `query` and paints the matches.
let highlight (query: string) (segments: Segment list) : Segment list =
    if String.IsNullOrEmpty query then
        segments
    else
        let text = segmentsText segments

        let matches =
            let rec find (from: int) acc =
                match text.IndexOf(query, from, StringComparison.OrdinalIgnoreCase) with
                | -1 -> List.rev acc
                | i -> find (i + query.Length) ((i, i + query.Length) :: acc)

            find 0 []

        if matches.IsEmpty then
            segments
        else
            let inMatch (pos: int) =
                matches |> List.exists (fun (a, b) -> pos >= a && pos < b)

            let cuts =
                matches |> List.collect (fun (a, b) -> [ a; b ]) |> Set.ofList

            let mutable pos = 0

            segments
            |> List.collect (fun s ->
                let start = pos
                pos <- pos + s.Text.Length

                let bounds =
                    [ start
                      yield! cuts |> Seq.filter (fun c -> c > start && c < pos)
                      pos ]

                bounds
                |> List.pairwise
                |> List.map (fun (a, b) ->
                    let piece = { s with Text = s.Text.Substring(a - start, b - a) }

                    if inMatch a then
                        { piece with
                            Color = Some Palette.matchForeground
                            Background = Some Palette.yellow }
                    else
                        piece))
            |> List.filter (fun s -> s.Text <> "")

let relativeAge (now: DateTimeOffset) (value: DateTimeOffset) =
    let span = now - value

    if span.TotalMinutes < 1.0 then "just now"
    elif span.TotalHours < 1.0 then $"{int span.TotalMinutes}m ago"
    elif span.TotalDays < 1.0 then $"{int span.TotalHours}h ago"
    elif span.TotalDays < 60.0 then $"{int span.TotalDays}d ago"
    elif span.TotalDays < 730.0 then $"{int (span.TotalDays / 30.0)}mo ago"
    else $"{int (span.TotalDays / 365.0)}y ago"
