module LazyJithub.Ui.Widgets

open System
open System.Collections
open System.Collections.Specialized
open System.Drawing
open Terminal.Gui.Drawing
open Terminal.Gui.Input
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open LazyJithub
open LazyJithub.Text
open LazyJithub.Render
open LazyJithub.Ui.Theme

type Row =
    { Id: string
      Segments: Segment list
      Search: string
      Separator: bool
      Marked: bool }

module Row =
    let make id segments search =
        { Id = id
          Segments = segments
          Search = search
          Separator = false
          Marked = false }

    let text id (label: string) = make id [ seg label ] label

    let separator id =
        { Id = id
          Segments = [ dim "--" ]
          Search = ""
          Separator = true
          Marked = false }

type RowSource(rows: Row[], focused: unit -> bool, flashing: string -> bool) =
    let changed = Event<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>()

    interface IListDataSource with
        member _.Count = rows.Length

        member _.MaxItemLength =
            rows |> Array.fold (fun acc r -> max acc (segmentsText r.Segments).Length) 0

        member val SuspendCollectionChangedEvent = false with get, set

        [<CLIEvent>]
        member _.CollectionChanged = changed.Publish

        member _.IsMarked _ = false
        member _.SetMark(_, _) = ()
        member _.ToList() = ResizeArray(rows) :> IList
        member _.RenderMark(_, _, _, _, _) = false

        member _.Render(lv, selected, item, col, row, width, _) =
            let normal = normalOf lv

            let bg =
                match selected && not rows.[item].Separator, rows.[item].Marked with
                | _ when flashing rows.[item].Id -> rowFlash
                | true, true -> if focused () then rowMarkedFocused else rowMarked
                | true, false -> if focused () then rowFocused else rowBlurred
                | false, true -> rowMarked
                | false, false -> normal.Background

            let used = drawSegments lv col row width bg rows.[item].Segments
            fill lv (col + used) row (width - used) bg

    interface IDisposable with
        member _.Dispose() = ()

type ListPanel(number: int, initialTabs: string list) as this =
    inherit View()

    let list =
        new ListView(
            X = Pos.Absolute 1,
            Y = Pos.Absolute 1,
            Width = Dim.Fill(Dim.Absolute 1),
            Height = Dim.Fill(Dim.Absolute 1),
            CanFocus = true
        )

    let filterField =
        new TextField(X = Pos.Absolute 1, Y = Pos.AnchorEnd 2, Width = Dim.Fill(Dim.Absolute 1), Visible = false)

    let mutable tabs = initialTabs
    let mutable tab = 0
    let mutable note = ""
    let mutable loading = false
    let mutable items: Row[] = [||]
    let mutable visible: Row[] = [||]
    let mutable query = ""
    let mutable lastId: string option = None
    let mutable flash: string option = None
    let mutable quiet = false

    let highlightedItem () =
        match list.SelectedItem with
        | sel when sel.HasValue && sel.Value >= 0 && sel.Value < visible.Length -> Some visible.[sel.Value]
        | _ -> None

    let emit () =
        let id = highlightedItem () |> Option.map _.Id

        if id <> lastId then
            lastId <- id
            this.OnHighlighted id

    let matches (item: Row) =
        query = ""
        || query.Split(' ', StringSplitOptions.RemoveEmptyEntries)
           |> Array.forall (fun w -> item.Search.Contains(w, StringComparison.OrdinalIgnoreCase))

    let selectIndex (index: int option) =
        list.SelectedItem <- (match index with Some i -> Nullable i | None -> Nullable())

        if index.IsSome then
            list.EnsureSelectedItemVisible()

    let render (keep: string option) =
        visible <- items |> Array.filter (fun i -> not i.Separator || query = "") |> Array.filter matches
        quiet <- true
        list.Source <- new RowSource(visible, (fun () -> this.HasFocus), (fun id -> flash = Some id))

        let byId id =
            visible |> Array.tryFindIndex (fun i -> i.Id = id && not i.Separator)

        let index =
            keep
            |> Option.bind byId
            |> Option.orElse (lastId |> Option.bind byId)
            |> Option.orElse (visible |> Array.tryFindIndex (fun i -> not i.Separator))

        selectIndex index
        quiet <- false
        this.SetNeedsDraw()
        emit ()

    let move (step: int) =
        if visible.Length > 0 then
            let start =
                match list.SelectedItem with
                | sel when sel.HasValue -> sel.Value
                | _ -> -step

            let rec find i =
                if i < 0 || i >= visible.Length then None
                elif visible.[i].Separator then find (i + sign step)
                else Some i

            match find (max 0 (min (visible.Length - 1) (start + step))) with
            | Some i -> selectIndex (Some i)
            | None -> ()

    do
        this.CanFocus <- true
        this.Add(list, filterField) |> ignore
        this.DrawingContent.Add(fun _ -> drawFrame this this.HasFocus (this.Title()) (this.Footer()))
        this.HasFocusChanged.Add(fun _ -> this.SetNeedsDraw())

        // A panel that grows keeps its scroll, which would leave rows above the highlight hidden.
        list.FrameChanged.Add(fun _ ->
            let viewport = list.Viewport
            let top = max 0 (min viewport.Y (visible.Length - viewport.Height))

            if top <> viewport.Y then
                list.Viewport <- Rectangle(viewport.X, top, viewport.Width, viewport.Height)

            list.EnsureSelectedItemVisible())

        list.ValueChanged.Add(fun _ ->
            this.SetNeedsDraw()

            if not quiet then
                emit ())
        list.Accepting.Add(fun e ->
            e.Handled <- true
            this.Activate())

        filterField.ValueChanged.Add(fun e ->
            query <- e.NewValue
            render None)

    member val OnHighlighted: string option -> unit = ignore with get, set
    member val OnActivated: string -> unit = ignore with get, set
    member val OnToggled: string -> unit = ignore with get, set
    member val OnReset: unit -> unit = ignore with get, set
    member val OnTabChanged: int -> unit = ignore with get, set

    member _.List = list
    member _.Tab = tab
    member _.Tabs = tabs
    member _.Items = items
    member _.Visible = visible
    member _.Query = query
    member _.IsFiltering = filterField.Visible && filterField.HasFocus
    member _.HighlightedId = highlightedItem () |> Option.map _.Id

    member _.Title() : Segment list =
        let focused = this.HasFocus
        let active (t: string) = if focused then boldColored Palette.green t else bold t
        let allTabs = (tabs |> List.sumBy String.length) + 3 * (tabs.Length - 1)

        [ if number > 0 then
              (if focused then boldColored Palette.green $"[{number}] " else seg $"[{number}] ")
          if tabs.Length > 1 && allTabs > this.Viewport.Width - 10 then
              active tabs.[tab]
              dim $" {tab + 1}/{tabs.Length}"
          else
              yield!
                  tabs
                  |> List.indexed
                  |> List.collect (fun (n, t) ->
                      [ if n > 0 then dim " - "
                        if n = tab then active t else dim t ])
          if note <> "" then dim $" · {note}"
          if query <> "" then colored Palette.yellow $" /{query}"
          if loading then colored Palette.yellow " loading…" ]

    member _.Footer() : Segment list =
        match list.SelectedItem with
        | sel when sel.HasValue && visible.Length > 0 -> [ dim $" {sel.Value + 1} of {visible.Length} " ]
        | _ -> []

    member _.SetTabs(newTabs: string list, index: int) =
        tabs <- newTabs
        tab <- max 0 (min index (newTabs.Length - 1))
        this.SetNeedsDraw()

    member _.NextTab(step: int) =
        if tabs.Length > 1 then
            tab <- (tab + step + tabs.Length) % tabs.Length
            this.SetNeedsDraw()
            this.OnTabChanged tab

    member _.SetNote(text: string) =
        if note <> text then
            note <- text
            this.SetNeedsDraw()

    /// Draws the row of `id` in the flash color until set back to `None`.
    member _.SetFlash(id: string option) =
        flash <- id
        list.SetNeedsDraw()

    member _.SetLoading(value: bool) =
        loading <- value
        this.SetNeedsDraw()

    member _.SetItems(newItems: Row list, ?keep: string) =
        items <- List.toArray newItems
        render keep

    /// Replaces the rows without moving the highlight or raising events.
    member _.UpdateItems(newItems: Row list) =
        items <- List.toArray newItems
        let current = highlightedItem () |> Option.map _.Id
        render current

    member _.Select(id: string) : bool =
        match visible |> Array.tryFindIndex (fun i -> i.Id = id && not i.Separator) with
        | Some i ->
            selectIndex (Some i)
            true
        | None ->
            if query <> "" && items |> Array.exists (fun i -> i.Id = id) then
                this.ClearFilter()
                this.Select id
            else
                false

    member _.Move(step: int) = move step
    member _.Page(step: int) = move (step * max 1 (list.Viewport.Height - 1))

    member _.Home() =
        selectIndex (visible |> Array.tryFindIndex (fun i -> not i.Separator))

    member _.End() =
        selectIndex (visible |> Array.tryFindIndexBack (fun i -> not i.Separator))

    member _.Activate() =
        match highlightedItem () with
        | Some item when not item.Separator -> this.OnActivated item.Id
        | _ -> ()

    member _.Toggle() =
        match highlightedItem () with
        | Some item when not item.Separator -> this.OnToggled item.Id
        | _ -> ()

    member _.FocusList() = list.SetFocus() |> ignore

    member _.StartFilter() =
        filterField.Visible <- true
        list.Height <- Dim.Fill(Dim.Absolute 2)
        filterField.Text <- query
        filterField.SetFocus() |> ignore
        filterField.MoveEnd() |> ignore

    member _.EndFilter() =
        filterField.Visible <- false
        list.Height <- Dim.Fill(Dim.Absolute 1)
        list.SetFocus() |> ignore

    member _.ClearFilter() =
        filterField.Text <- ""
        query <- ""
        filterField.Visible <- false
        list.Height <- Dim.Fill(Dim.Absolute 1)
        render None

    member _.Escape() =
        if query <> "" then this.ClearFilter() else this.OnReset()

    member _.EnsureVisible() = list.EnsureSelectedItemVisible()

/// Plain text in light grey; bold and colored text keep their own color, so they stand out.
let private soften (segments: Segment list) =
    segments
    |> List.map (fun s ->
        match s.Color, s.Style with
        | None, (Plain | Italic | Underline | Strike) -> { s with Color = Some Palette.text }
        | _ -> s)

/// A scrolling Rich document, laid out again only when the width changes.
type RichView() as this =
    inherit View()

    let mutable blocks: Rich.Block list = []
    let mutable laidOut: (int * Rich.Line[]) option = None
    let mutable top = 0
    let mutable query = ""
    let mutable current = -1
    let mutable jumped = 0

    let lines () =
        let width = max 1 this.Viewport.Width

        match laidOut with
        | Some(w, ls) when w = width -> ls
        | _ ->
            let ls = Rich.render width blocks |> List.toArray
            laidOut <- Some(width, ls)
            ls

    let height () = max 1 this.Viewport.Height

    // A section jump may scroll past the end, so the last headings can still reach the top line.
    let clamp () =
        top <- max 0 (min top (max ((lines ()).Length - height ()) jumped))

    let matches () =
        if query = "" then
            [||]
        else
            lines ()
            |> Array.indexed
            |> Array.filter (fun (_, l) -> (Rich.lineText l).Contains(query, StringComparison.OrdinalIgnoreCase))
            |> Array.map fst

    let sections () =
        lines () |> Array.indexed |> Array.choose (fun (i, l) -> l.Section |> Option.map (fun s -> i, s))

    let moved () =
        clamp ()
        this.SetNeedsDraw()
        this.OnMoved()

    do
        this.CanFocus <- true

        this.DrawingContent.Add(fun _ ->
            clamp ()
            let all = lines ()
            let normal = normalOf this

            for y in 0 .. height () - 1 do
                if top + y < all.Length then
                    drawSegments this 0 y this.Viewport.Width normal.Background (highlight query (soften all.[top + y].Segments))
                    |> ignore)

        this.MouseEvent.Add(fun m ->
            if m.Flags.HasFlag MouseFlags.WheeledDown then
                this.Scroll 3
                m.Handled <- true
            elif m.Flags.HasFlag MouseFlags.WheeledUp then
                this.Scroll -3
                m.Handled <- true)

    member val OnMoved: unit -> unit = ignore with get, set
    member _.Query = query

    member _.SetBlocks(document: Rich.Block list) =
        blocks <- document
        laidOut <- None
        top <- 0
        jumped <- 0
        query <- ""
        current <- -1
        moved ()

    member _.Scroll(rows: int) =
        top <- max 0 (top + rows)
        moved ()

    member _.Page(step: int) = this.Scroll(step * max 1 (height () - 2))

    member _.Top() =
        top <- 0
        jumped <- 0
        moved ()

    member _.Bottom() =
        top <- (lines ()).Length
        jumped <- 0
        moved ()

    member _.JumpSection(step: int) =
        let all = sections ()

        let target =
            if step > 0 then all |> Array.tryFind (fun (i, _) -> i > top)
            else all |> Array.tryFindBack (fun (i, _) -> i < top)

        match target with
        | Some(i, _) ->
            top <- i
            jumped <- i
        | None ->
            top <- if step > 0 then (lines ()).Length else 0
            jumped <- 0

        moved ()

    /// The heading at or above the first visible line.
    member _.Section =
        sections () |> Array.tryFindBack (fun (i, _) -> i <= top) |> Option.map snd

    member _.Percent =
        let total = (lines ()).Length - height ()
        if total <= 0 then 100 else min 100 (top * 100 / total)

    member _.Search(text: string) =
        query <- text
        current <- -1
        this.NextMatch 1

    member _.ClearSearch() =
        query <- ""
        current <- -1
        moved ()

    /// Returns the 1-based position of the match now shown and the number of matches.
    member _.NextMatch(step: int) =
        let found = matches ()

        if found.Length = 0 then
            moved ()
            0, 0
        else
            current <-
                if current < 0 then
                    if step > 0 then
                        found |> Array.tryFindIndex (fun i -> i >= top) |> Option.defaultValue 0
                    else
                        found |> Array.tryFindIndexBack (fun i -> i < top) |> Option.defaultValue (found.Length - 1)
                else
                    (current + step + found.Length) % found.Length

            top <- max 0 (found.[current] - height () / 3)
            moved ()
            current + 1, found.Length

type DetailPanel() as this =
    inherit View()

    let header = new View(X = Pos.Absolute 2, Y = Pos.Absolute 1, Width = Dim.Fill(Dim.Absolute 2), Height = Dim.Absolute 3)

    let body =
        new RichView(X = Pos.Absolute 2, Y = Pos.Absolute 4, Width = Dim.Fill(Dim.Absolute 2), Height = Dim.Fill(Dim.Absolute 1))

    let mutable title: Segment list = []
    let mutable headerLines: Segment list list = []
    let mutable summary = ""
    let mutable footer = ""
    let mutable searchStatus = ""

    /// The summary wraps, so the header height follows the pane width.
    let placeHeader () =
        let width = max 1 (this.Viewport.Width - 4)
        let rows = headerLines.Length + (wrap width 2 summary).Length
        let y = if rows = 0 then 1 else rows + 2

        if header.Visible <> (rows > 0) then
            header.Visible <- rows > 0

        if header.Frame.Height <> max 1 rows || body.Frame.Y <> y then
            header.Height <- Dim.Absolute(max 1 rows)
            body.Y <- Pos.Absolute y

    let footerSegments () =
        let where =
            [ yield! body.Section |> Option.toList
              if body.Percent < 100 || body.Section.IsSome then $"{body.Percent}%%" ]

        [ if searchStatus <> "" then
              colored Palette.yellow $" {searchStatus} "
          if not where.IsEmpty then
              dim (" " + String.Join(" · ", where) + " ")
          if footer <> "" then
              dim $" {footer} " ]

    do
        this.CanFocus <- true
        this.Add(header, body) |> ignore
        this.DrawingContent.Add(fun _ -> drawFrame this this.HasFocus (this.FrameTitle()) (footerSegments ()))
        this.HasFocusChanged.Add(fun _ -> this.SetNeedsDraw())
        this.FrameChanged.Add(fun _ -> placeHeader ())
        body.OnMoved <- fun () -> this.SetNeedsDraw()

        header.DrawingContent.Add(fun _ ->
            let normal = normalOf header
            let width = header.Viewport.Width

            headerLines
            |> List.iteri (fun y line ->
                if y < header.Viewport.Height then
                    drawSegments header 0 y width normal.Background line |> ignore)

            wrap width 2 summary
            |> List.iteri (fun n line ->
                let y = headerLines.Length + n

                if y < header.Viewport.Height then
                    drawSegments header 0 y width normal.Background [ bold line ] |> ignore))

    member val Key: string option = None with get, set
    member val Prefix = "[0] " with get, set
    member val EmptyTitle = "Issue" with get, set

    member _.FrameTitle() =
        let focused = this.HasFocus
        let accent text = if focused then boldColored Palette.green text else bold text

        (if focused then boldColored Palette.green this.Prefix else seg this.Prefix)
        :: (if title.IsEmpty then [ accent this.EmptyTitle ] else title)

    member _.SetTitle(segments: Segment list) =
        title <- segments
        this.SetNeedsDraw()

    member _.SetFooter(text: string) =
        footer <- text
        this.SetNeedsDraw()

    /// `lines` are drawn as is; `wrapped` follows them in bold, word-wrapped on up to two rows.
    member _.Show(key: string option, lines: Segment list list, wrapped: string, document: Rich.Block list) =
        this.Key <- key
        headerLines <- lines
        summary <- wrapped
        searchStatus <- ""
        placeHeader ()
        body.SetBlocks document
        header.SetNeedsDraw()
        this.SetNeedsLayout()
        this.SetNeedsDraw()

    member this.ShowMessage(lines: Segment list list, document: Rich.Block list) =
        this.Show(this.Key, lines, "", document)

    member _.Scroll(rows: int) = body.Scroll rows
    member _.Page(step: int) = body.Page step
    member _.Top() = body.Top()
    member _.Bottom() = body.Bottom()
    member _.JumpSection(step: int) = body.JumpSection step
    member _.FocusBody() = body.SetFocus() |> ignore
    member _.Searching = body.Query <> ""

    member _.Search(query: string) =
        let n, total = body.Search query
        searchStatus <- if total = 0 then $"/{query}: no match" else $"/{query} {n}/{total}"
        this.SetNeedsDraw()

    member _.NextMatch(step: int) =
        if body.Query <> "" then
            let n, total = body.NextMatch step
            searchStatus <- if total = 0 then $"/{body.Query}: no match" else $"/{body.Query} {n}/{total}"
            this.SetNeedsDraw()

    member _.ClearSearch() =
        searchStatus <- ""
        body.ClearSearch()

/// Lines are laid out for the inner width on every draw, so they rewrap when the terminal resizes.
type LinesPanel(render: int -> Segment list list) as this =
    inherit View()

    let mutable top = 0
    let mutable title: Segment list = []
    let innerWidth () = max 1 (this.Viewport.Width - 4)
    let innerHeight () = max 1 (this.Viewport.Height - 2)

    let clamp (count: int) =
        top <- max 0 (min top (count - innerHeight ()))

    do
        this.CanFocus <- true

        this.DrawingContent.Add(fun _ ->
            let lines = render (innerWidth ())
            clamp lines.Length
            let height = innerHeight ()

            let footer =
                if lines.Length > height then
                    [ dim $" {top + 1}-{min lines.Length (top + height)} of {lines.Length} " ]
                else
                    []

            drawFrame this true title footer
            let normal = normalOf this

            lines
            |> List.skip (min top lines.Length)
            |> List.truncate height
            |> List.iteri (fun y line -> drawSegments this 2 (y + 1) (innerWidth ()) normal.Background line |> ignore))

    member _.SetTitle(segments: Segment list) =
        title <- segments
        this.SetNeedsDraw()

    member _.Scroll(rows: int) =
        top <- max 0 (top + rows)
        this.SetNeedsDraw()

    member _.Page(step: int) = this.Scroll(step * max 1 (innerHeight () - 1))

    member _.Top() =
        top <- 0
        this.SetNeedsDraw()

    member _.Bottom() = this.Scroll(Int32.MaxValue / 2)

type PromptPanel(caption: string) as this =
    inherit View()

    let field = new TextField(X = Pos.Absolute 1, Y = Pos.Absolute 1, Width = Dim.Fill(Dim.Absolute 1))

    do
        this.CanFocus <- true
        this.Height <- Dim.Absolute 3
        this.Add field |> ignore
        this.DrawingContent.Add(fun _ -> drawFrame this true [ boldColored Palette.green caption ] [])

    member _.Field = field

    member _.Value
        with get () = field.Text
        and set (v: string) =
            field.Text <- v
            field.MoveEnd() |> ignore

/// Sees keys before any view, so list views never run their type-to-search on j or k.
/// Terminals with the kitty keyboard protocol, such as Ghostty, name a shifted symbol `Shift+?`
/// where others send `?`; Terminal.Gui already names a shifted letter `A`, so only symbols need this.
let keyName (key: Key) =
    let text = string key

    if text.Length = 7 && text.StartsWith "Shift+" && not (Char.IsLetterOrDigit text.[6]) then
        text.Substring 6
    else
        text

type KeyRouter() =
    let handlers = Collections.Generic.Stack<Terminal.Gui.Input.Key -> bool>()

    member _.Push(handler: Terminal.Gui.Input.Key -> bool) = handlers.Push handler
    member _.Pop() = handlers.Pop() |> ignore

    member _.Handle(key: Terminal.Gui.Input.Key) =
        handlers.Count > 0 && handlers.Peek() key

type StatusLine() as this =
    inherit View()

    let mutable left: Segment list = []
    let mutable message: Segment option = None

    do
        this.Height <- Dim.Absolute 1
        this.DrawingContent.Add(fun _ ->
            let w = this.Viewport.Width
            let normal = normalOf this
            let right = message |> Option.map (fun m -> m.Text.Length) |> Option.defaultValue 0
            let used = drawSegments this 1 0 (max 0 (w - right - 3)) normal.Background left

            match message with
            | Some m when right > 0 ->
                let x = max (used + 2) (w - right - 1)
                drawSegments this x 0 (w - x) normal.Background [ m ] |> ignore
            | _ -> ())

    member _.SetHints(text: string) =
        left <- [ dim text ]
        this.SetNeedsDraw()

    member _.SetSegments(segments: Segment list) =
        left <- segments
        this.SetNeedsDraw()

    member _.SetMessage(segment: Segment option) =
        message <- segment
        this.SetNeedsDraw()
