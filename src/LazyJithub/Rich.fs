module LazyJithub.Rich

open System
open LazyJithub.Text

type Block =
    | Heading of string
    | Sub of Segment list
    | Para of Segment list
    | Item of marker: Segment * body: Block list
    | Code of string list
    | Quote of Block list
    | Rule
    | Table of Segment list list list
    | Grid of (Segment list * Segment list) list

/// A heading line carries its title, so the view can jump between sections and name the current one.
type Line =
    { Segments: Segment list
      Section: string option }

let private line segments = { Segments = segments; Section = None }
let private blank = line []
let private width' (segments: Segment list) = (segmentsText segments).Length

let private indent (prefix: Segment list) (rest: Segment list) (lines: Line list) =
    lines
    |> List.mapi (fun i l ->
        { l with
            Segments = (if i = 0 then prefix else rest) @ l.Segments })

let private pad (n: int) = if n > 0 then [ seg (String(' ', n)) ] else []

let private cut (width: int) (segments: Segment list) =
    match wrapSegments width segments with
    | first :: _ :: _ when width > 1 ->
        let text = segmentsText first

        if text.Length < width then first @ [ dim "…" ]
        else (wrapSegments (width - 1) segments).Head @ [ dim "…" ]
    | first :: _ -> first
    | [] -> []

let private gridLabelWidth (pairs: (Segment list * Segment list) list) =
    pairs |> List.fold (fun acc (label, _) -> max acc (width' label)) 0 |> min 22

/// A grid whose labels are plain dim text, as for issue fields.
let grid (pairs: (string * Segment list) list) =
    Grid(pairs |> List.map (fun (label, value) -> [ dim label ], value))

let rec private block (width: int) (b: Block) : Line list =
    let width = max 1 width

    match b with
    | Heading title ->
        [ { Segments =
              [ { seg title with
                    Color = Some Palette.blue
                    Style = BoldUnderline } ]
            Section = Some title } ]
    | Sub segments
    | Para segments -> wrapSegments width segments |> List.map line
    | Item(marker, body) ->
        let m = marker.Text.Length
        let inner = layout true (width - m) body
        let inner = if inner.IsEmpty then [ blank ] else inner
        indent [ marker ] (pad m) inner
    | Code lines ->
        let gutter = [ dim "│ " ]

        lines
        |> List.collect (fun l ->
            let text = l.Replace("\t", "    ")

            if text = "" then
                [ gutter ]
            else
                text
                |> Seq.chunkBySize (max 1 (width - 2))
                |> Seq.map (fun chunk -> gutter @ [ colored Palette.code (String chunk) ])
                |> List.ofSeq)
        |> List.map line
    | Quote body -> layout false (width - 2) body |> indent [ colored Palette.dimGrey "▎ " ] [ colored Palette.dimGrey "▎ " ]
    | Rule -> [ line [ dim (String('─', width)) ] ]
    | Table rows -> table width rows
    | Grid pairs ->
        let labelWidth = gridLabelWidth pairs
        let valueWidth = max 8 (width - labelWidth - 2)

        pairs
        |> List.collect (fun (label, value) ->
            let label = if width' label > labelWidth then cut labelWidth label else label

            let values =
                match wrapSegments valueWidth value with
                | [] -> [ [] ]
                | lines -> lines

            values
            |> List.mapi (fun i v ->
                if i = 0 then line (label @ pad (labelWidth - width' label + 2) @ v)
                else line (pad (labelWidth + 2) @ v)))

and private table (width: int) (rows: Segment list list list) =
    let columns = rows |> List.fold (fun acc r -> max acc r.Length) 0
    let rows = rows |> List.map (fun r -> r @ List.replicate (columns - r.Length) [])

    let natural =
        [ for c in 0 .. columns - 1 -> rows |> List.fold (fun acc r -> max acc (width' r.[c])) 1 ]

    let border (left: string) (middle: string) (right: string) =
        line [ dim (left + String.Join(middle, natural |> List.map (fun w -> String('─', w + 2))) + right) ]

    if columns = 0 then
        []
    elif List.sum natural + 3 * columns + 1 <= width then
        let row (bold': bool) (cells: Segment list list) =
            line
                [ dim "│"
                  yield!
                      cells
                      |> List.mapi (fun c cell ->
                          let cell = if bold' then cell |> List.map (fun s -> { s with Style = Bold }) else cell
                          [ seg " "; yield! cell; yield! pad (natural.[c] - width' cell + 1); dim "│" ])
                      |> List.concat ]

        [ border "┌" "┬" "┐"
          row true rows.Head
          if rows.Length > 1 then
              border "├" "┼" "┤"
          yield! rows.Tail |> List.map (row false)
          border "└" "┴" "┘" ]
    else
        // Too wide to draw as a table: each row becomes a small grid of header and value.
        let headers = rows.Head |> List.map segmentsText

        rows.Tail
        |> List.mapi (fun n r ->
            [ if n > 0 then
                  line [ dim (String('┄', min width 20)) ]
              yield! block width (grid (List.zip headers r)) ])
        |> List.concat

/// Section headings get two blank lines before and one after; list items in a list stay tight.
and layout (tight: bool) (width: int) (blocks: Block list) : Line list =
    let gap (previous: Block option) (next: Block) =
        match previous, next with
        | None, _ -> 0
        | Some _, Heading _ -> 2
        | Some(Heading _), _ -> 1
        | Some(Item _), Item _
        | Some(Sub _), Item _ -> 0
        | Some _, _ when tight -> 0
        | Some _, _ -> 1

    blocks
    |> List.fold
        (fun (acc: Line list list, previous) b -> (List.replicate (gap previous b) blank @ block width b) :: acc, Some b)
        ([], None)
    |> fst
    |> List.rev
    |> List.concat

let render (width: int) (blocks: Block list) = layout false width blocks

let lineText (l: Line) = segmentsText l.Segments
