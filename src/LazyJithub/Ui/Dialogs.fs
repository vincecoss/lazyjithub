/// Each popup runs a nested Terminal.Gui session and owns the keys while it is open.
module LazyJithub.Ui.Dialogs

open System
open Terminal.Gui.App
open Terminal.Gui.Drawing
open Terminal.Gui.Input
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open LazyJithub
open LazyJithub.Model
open LazyJithub.Text
open LazyJithub.Render
open LazyJithub.Ui.Widgets

let private popup (width: Dim) (height: Dim) =
    new Window(
        X = Pos.Center(),
        Y = Pos.Center(),
        Width = width,
        Height = height,
        BorderStyle = LineStyle.None,
        ShadowStyle = Nullable ShadowStyles.None
    )

let private run (app: IApplication) (router: KeyRouter) (window: Window) (handler: Key -> bool) =
    router.Push handler

    try
        app.Run(window, null) |> ignore
    finally
        router.Pop()
        window.Dispose()
        app.LayoutAndDraw true

let private name (key: Key) = keyName key

let private scrollKeys (panel: DetailPanel) (key: Key) =
    match name key with
    | "j"
    | "CursorDown" -> panel.Scroll 1; true
    | "k"
    | "CursorUp" -> panel.Scroll -1; true
    | "Ctrl+D"
    | "PageDown"
    | "Space" -> panel.Page 1; true
    | "Ctrl+U"
    | "PageUp" -> panel.Page -1; true
    | "Home" -> panel.Top(); true
    | "End" -> panel.Bottom(); true
    | _ -> false

let listKeys (panel: ListPanel) (key: Key) =
    match name key with
    | "j"
    | "CursorDown" -> panel.Move 1; true
    | "k"
    | "CursorUp" -> panel.Move -1; true
    | "Ctrl+D"
    | "PageDown" -> panel.Page 1; true
    | "Ctrl+U"
    | "PageUp" -> panel.Page -1; true
    | "Home" -> panel.Home(); true
    | "End" -> panel.End(); true
    | "/" -> panel.StartFilter(); true
    | _ -> false

/// While a list filter field has focus, only enter and esc are ours; the rest is typing.
let filterKeys (panel: ListPanel) (key: Key) =
    match name key with
    | "Enter"
    | "CursorDown"
    | "CursorUp" ->
        panel.EndFilter()
        if name key = "CursorDown" then panel.Move 1
        elif name key = "CursorUp" then panel.Move -1
        true
    | "Esc" ->
        panel.ClearFilter()
        panel.FocusList()
        true
    | _ -> false

let document (app: IApplication) (router: KeyRouter) (title: Segment list) (footer: string) (blocks: Rich.Block list) =
    let window = popup (Dim.Percent 80) (Dim.Percent 85)

    let panel =
        new DetailPanel(X = Pos.Absolute 0, Y = Pos.Absolute 0, Width = Dim.Fill(), Height = Dim.Fill(), Prefix = "")

    window.Add panel |> ignore
    panel.SetTitle title
    panel.SetFooter footer
    panel.ShowMessage([], blocks)
    panel.FocusBody()

    run app router window (fun key ->
        match name key with
        | "Esc"
        | "q"
        | "?" ->
            app.RequestStop()
            true
        | "}"
        | "]" -> panel.JumpSection 1; true
        | "{"
        | "[" -> panel.JumpSection -1; true
        | _ -> scrollKeys panel key)

let lines (app: IApplication) (router: KeyRouter) (title: Segment list) (render: int -> Segment list list) =
    let window = popup (Dim.Percent 80) (Dim.Percent 90)
    let panel = new LinesPanel(render, X = Pos.Absolute 0, Y = Pos.Absolute 0, Width = Dim.Fill(), Height = Dim.Fill())
    window.Add panel |> ignore
    panel.SetTitle title
    panel.SetFocus() |> ignore

    run app router window (fun key ->
        match name key with
        | "Esc"
        | "q"
        | "?" ->
            app.RequestStop()
            true
        | "j"
        | "CursorDown" -> panel.Scroll 1; true
        | "k"
        | "CursorUp" -> panel.Scroll -1; true
        | "Ctrl+D"
        | "PageDown"
        | "Space" -> panel.Page 1; true
        | "Ctrl+U"
        | "PageUp" -> panel.Page -1; true
        | "Home" -> panel.Top(); true
        | "End" -> panel.Bottom(); true
        | _ -> false)

let prompt (app: IApplication) (router: KeyRouter) (caption: string) (history: string list) (initial: string) =
    let window = popup (Dim.Percent 70) (Dim.Absolute 3)
    let panel = new PromptPanel($" {caption} ", X = Pos.Absolute 0, Y = Pos.Absolute 0, Width = Dim.Fill())
    window.Add panel |> ignore
    panel.Value <- initial
    panel.Field.SetFocus() |> ignore
    let mutable result = None
    let mutable index = -1

    let browse step =
        if not history.IsEmpty then
            index <- max -1 (min (history.Length - 1) (index + step))
            panel.Value <- if index < 0 then initial else history.[index]

    run app router window (fun key ->
        match name key with
        | "Enter" ->
            let value = panel.Value.Trim()
            result <- if value = "" then None else Some value
            app.RequestStop()
            true
        | "Esc" ->
            app.RequestStop()
            true
        | "CursorUp" -> browse 1; true
        | "CursorDown" -> browse -1; true
        | _ -> false)

    result

type private NoteFrame(title: Segment list, lines: Segment list list) as this =
    inherit View()

    do
        this.DrawingContent.Add(fun _ ->
            Theme.drawFrame this false title []
            let normal = Theme.normalOf this

            lines
            |> List.truncate (max 0 (this.Viewport.Height - 2))
            |> List.iteri (fun y line -> Theme.drawSegments this 2 (y + 1) (this.Viewport.Width - 4) normal.Background line |> ignore))

/// Like `pick`, with `note` framed under the list: a title and lines, such as the issues the choice applies to.
let pickWith
    (app: IApplication)
    (router: KeyRouter)
    (title: string)
    (hint: string)
    (items: Row list)
    (note: (string * Segment list list) option)
    =
    let noteLines =
        match note with
        | Some(_, lines) when lines.Length > 8 -> List.truncate 7 lines @ [ [ dim $"and {lines.Length - 7} more" ] ]
        | Some(_, lines) -> lines
        | None -> []

    let noteRows = if note.IsSome then noteLines.Length + 2 else 0
    let window = popup (Dim.Percent 70) (Dim.Absolute(min 30 (items.Length + 3) + noteRows))

    let panel =
        new ListPanel(0, [ title ], X = Pos.Absolute 0, Y = Pos.Absolute 0, Width = Dim.Fill(), Height = Dim.Fill(Dim.Absolute noteRows))

    window.Add panel |> ignore

    match note with
    | Some(noteTitle, _) ->
        new NoteFrame(
            [ bold noteTitle ],
            noteLines,
            X = Pos.Absolute 0,
            Y = Pos.AnchorEnd noteRows,
            Width = Dim.Fill(),
            Height = Dim.Absolute noteRows
        )
        |> window.Add
        |> ignore
    | None -> ()

    panel.SetItems items
    panel.SetNote hint
    panel.FocusList()
    let mutable result = None

    run app router window (fun key ->
        if panel.IsFiltering then
            filterKeys panel key
        else
            match name key with
            | "Enter" ->
                result <- panel.HighlightedId
                app.RequestStop()
                true
            | "Esc"
            | "q" ->
                if panel.Query <> "" then panel.ClearFilter() else app.RequestStop()
                true
            | _ -> listKeys panel key)

    result

let pick (app: IApplication) (router: KeyRouter) (title: string) (hint: string) (items: Row list) =
    pickWith app router title hint items None

type BoardCard =
    { Item: Model.Item
      Changed: bool
      Lane: string option }

/// Without a name, the lane has no frame and its columns fill the window.
type BoardLane =
    { Name: string option
      Columns: (string * BoardCard list) list }

type private LaneFrame(name: string, count: int) as this =
    inherit View()

    do
        this.CanFocus <- true

        this.DrawingContent.Add(fun _ ->
            let title = if this.HasFocus then boldColored Palette.green name else bold name
            Theme.drawFrame this this.HasFocus [ title; dim $" · {count}" ] [])

        this.HasFocusChanged.Add(fun _ -> this.SetNeedsDraw())

/// `load filtered` gives the lanes to show, with the status and repository filters applied or not;
/// `f` switches between the two. Returns the item picked with enter and the last filter choice.
let kanban
    (app: IApplication)
    (router: KeyRouter)
    (load: bool -> BoardLane list)
    (useFilters: bool)
    (current: string option)
    (statusColorOf: string -> StatusCategory -> string)
    =
    let window = popup (Dim.Fill()) (Dim.Fill())
    let preview = new StatusLine(X = Pos.Absolute 0, Y = Pos.AnchorEnd 1, Width = Dim.Fill())
    let finderRows = 10

    let finder =
        new ListPanel(
            0,
            [ "Find" ],
            X = Pos.Absolute 0,
            Y = Pos.AnchorEnd(finderRows + 1),
            Width = Dim.Fill(),
            Height = Dim.Absolute finderRows
        )

    let finderView = finder :> View
    finderView.Visible <- false
    finder.SetNote "type to match · ↑ ↓ pick · enter goes there · esc closes"
    window.Add(preview, finder) |> ignore
    let mutable filtered = useFilters
    let mutable blocks: (View * ListPanel list) list = []
    let mutable wants: int list = []
    let mutable cards: Map<string, BoardCard> = Map.empty
    let mutable lane = 0
    let mutable column = 0
    let mutable flashing: ListPanel option = None
    let mutable flashRun = 0

    let showPreview (id: string option) =
        preview.SetMessage(
            Some(
                if filtered then colored Palette.green "status and repository filters on · f shows all "
                else colored Palette.yellow "all statuses and repositories · f applies the filters "
            )
        )

        match id |> Option.bind cards.TryFind with
        | Some c ->
            let i = c.Item

            let assignees =
                match i.Assignees with
                | [] -> "Unassigned"
                | names -> String.Join(", ", names)

            preview.SetSegments(
                [ boldColored Palette.blue i.ShortKey
                  seg " "
                  itemGlyph i
                  seg " "
                  boldColored (statusColorOf i.Status i.Category) i.Status
                  dim " · "
                  colored Palette.lavender assignees ]
                @ (if i.Labels.IsEmpty then [] else dim " · " :: labelChips i.Labels)
                @ (c.Lane |> Option.map (fun lane -> [ dim " · "; bold lane ]) |> Option.defaultValue [])
                @ [ dim " · "; seg i.Title ]
            )
        | None ->
            let lanes = if blocks.Length > 1 then " · { } lanes" else ""
            preview.SetSegments [ dim $"h/l columns · j/k cards{lanes} · / find · enter opens · esc closes" ]

    let heights () =
        laneHeights (window.Viewport.Height - 1 - (if finderView.Visible then finderRows else 0)) wants lane

    let pane l c =
        blocks |> List.tryItem l |> Option.bind (fun (_, panes) -> List.tryItem c panes)

    let focusOn l c =
        if not blocks.IsEmpty then
            lane <- max 0 (min (blocks.Length - 1) l)
            let count = max 1 (snd blocks.[lane]).Length
            column <- (c % count + count) % count
            window.SetNeedsLayout()

            match pane lane column with
            | Some p ->
                p.FocusList()
                showPreview p.HighlightedId
            | None -> ()

    let atEdge (p: ListPanel) (step: int) =
        let last = p.Visible.Length - 1
        let selected = p.List.SelectedItem
        last < 0 || (selected.HasValue && selected.Value = (if step > 0 then last else 0))

    let stopFlash () =
        flashRun <- flashRun + 1
        flashing |> Option.iter (fun p -> p.SetFlash None)
        flashing <- None

    /// Blinks the card for 2 seconds: 8 ticks of 250 ms.
    let flash (p: ListPanel) (key: string) =
        stopFlash ()
        let current = flashRun
        let mutable ticks = 0
        flashing <- Some p
        p.SetFlash(Some key)

        app.AddTimeout(
            TimeSpan.FromMilliseconds 250.0,
            fun () ->
                ticks <- ticks + 1

                if current <> flashRun then
                    false
                elif ticks >= 8 then
                    stopFlash ()
                    false
                else
                    p.SetFlash(if ticks % 2 = 0 then Some key else None)
                    true
        )
        |> ignore

    let build (keep: string option) =
        stopFlash ()

        for block, _ in blocks do
            window.Remove block |> ignore
            block.Dispose()

        let lanes = load filtered |> List.filter (fun l -> not l.Columns.IsEmpty)
        cards <- lanes |> List.collect (fun l -> l.Columns |> List.collect snd) |> List.map (fun c -> c.Item.Key, c) |> Map.ofList
        let keyWidth = cards |> Map.fold (fun acc _ c -> max acc c.Item.ShortKey.Length) 0

        finder.SetItems(
            lanes
            |> List.collect (fun laneData ->
                laneData.Columns
                |> List.collect (fun (columnName, columnCards) ->
                    let place =
                        match laneData.Name with
                        | Some name -> $"{name} · {columnName}"
                        | None -> columnName

                    columnCards
                    |> List.map (fun card ->
                        let i = card.Item

                        Row.make
                            i.Key
                            [ boldColored Palette.blue (i.ShortKey.PadRight keyWidth)
                              seg " "
                              (match i.Assignees with
                               | first :: _ -> colored Palette.lavender (initials (Some first))
                               | [] -> dim "--")
                              seg " "
                              seg i.Title
                              dim $"  {place}" ]
                            $"{i.Key} {i.Title} {String.Join(' ', i.Assignees)}")))
        )

        wants <-
            lanes
            |> List.map (fun l ->
                match l.Name with
                | Some _ -> 4 + (l.Columns |> List.map (snd >> List.length) |> List.max |> max 1)
                | None -> Int32.MaxValue / 2)

        blocks <-
            lanes
            |> List.mapi (fun l laneData ->
                let inset = if laneData.Name.IsSome then 1 else 0
                let count = laneData.Columns.Length
                let edge n = inset + n * (window.Viewport.Width - 2 * inset) / count

                let block =
                    match laneData.Name with
                    | Some name -> new LaneFrame(name, laneData.Columns |> List.sumBy (snd >> List.length)) :> View
                    | None -> new View(CanFocus = true)

                block.X <- Pos.Absolute 0
                block.Y <- Pos.Func(Func<View, int>(fun _ -> heights () |> List.take l |> List.sum), null)
                block.Width <- Dim.Fill()
                block.Height <- Dim.Func(Func<View, int>(fun _ -> heights () |> List.item l), null)

                let panes =
                    laneData.Columns
                    |> List.mapi (fun c (columnName, columnCards) ->
                        let panel =
                            new ListPanel(
                                c + 1,
                                [ columnName ],
                                X = Pos.Func(Func<View, int>(fun _ -> edge c), null),
                                Y = Pos.Absolute inset,
                                Width = Dim.Func(Func<View, int>(fun _ -> edge (c + 1) - edge c), null),
                                Height = Dim.Fill(Dim.Absolute inset)
                            )

                        panel.SetNote(string columnCards.Length)

                        panel.SetItems(
                            columnCards
                            |> List.map (fun card ->
                                let i = card.Item

                                Row.make
                                    i.Key
                                    [ (if card.Changed then colored Palette.yellow "•" else seg " ")
                                      itemGlyph i
                                      seg " "
                                      boldColored Palette.blue i.ShortKey
                                      seg " "
                                      seg i.Title ]
                                    $"{i.Key} {i.Title} {String.Join(' ', i.Assignees)}")
                        )

                        panel.OnHighlighted <- fun id -> if panel.HasFocus then showPreview id

                        panel.HasFocusChanged.Add(fun _ ->
                            if panel.HasFocus && (lane, column) <> (l, c) then
                                lane <- l
                                column <- c
                                window.SetNeedsLayout())

                        block.Add panel |> ignore
                        panel)

                block, panes)

        for block, _ in blocks do
            window.Add block |> ignore

        let start =
            keep
            |> Option.bind (fun key ->
                blocks
                |> List.indexed
                |> List.tryPick (fun (l, (_, panes)) -> panes |> List.tryFindIndex (fun p -> p.Select key) |> Option.map (fun c -> l, c)))
            |> Option.orElse (
                blocks
                |> List.indexed
                |> List.tryPick (fun (l, (_, panes)) -> panes |> List.tryFindIndex (fun p -> p.Visible.Length > 0) |> Option.map (fun c -> l, c))
            )
            |> Option.defaultValue (0, 0)

        window.SetNeedsLayout()

        if blocks.IsEmpty then
            showPreview None
            preview.SetSegments [ dim "The filters hide every column · f shows all" ]
        else
            focusOn (fst start) (snd start)

    let closeFinder () =
        finder.ClearFilter()
        finderView.Visible <- false
        window.SetNeedsLayout()

    let goTo (key: string) =
        blocks
        |> List.indexed
        |> List.tryPick (fun (l, (_, panes)) -> panes |> List.tryFindIndex (fun p -> p.Select key) |> Option.map (fun c -> l, c))
        |> Option.iter (fun (l, c) ->
            focusOn l c
            pane l c |> Option.iter (fun p -> flash p key))

    finder.OnHighlighted <- fun id -> if finderView.Visible then showPreview id

    let finderKeys (key: Key) =
        match name key with
        | "Enter" ->
            let found = finder.HighlightedId
            closeFinder ()

            match found with
            | Some key -> goTo key
            | None -> focusOn lane column

            true
        | "Esc" ->
            closeFinder ()
            focusOn lane column
            true
        | "CursorDown"
        | "Ctrl+N" -> finder.Move 1; true
        | "CursorUp"
        | "Ctrl+P" -> finder.Move -1; true
        | "PageDown" -> finder.Page 1; true
        | "PageUp" -> finder.Page -1; true
        | _ -> false

    build current
    let mutable result = None

    run app router window (fun key ->
        let panel = pane lane column

        if finderView.Visible then
            finderKeys key
        else
            match name key, panel with
            | "/", _ when not cards.IsEmpty ->
                finderView.Visible <- true
                window.SetNeedsLayout()
                finder.StartFilter()
                showPreview finder.HighlightedId
                true
            | "f", _ ->
                filtered <- not filtered
                build (panel |> Option.bind _.HighlightedId |> Option.orElse current)
                true
            | ("h" | "CursorLeft" | "Shift+Tab"), _ -> focusOn lane (column - 1); true
            | ("l" | "CursorRight" | "Tab"), _ -> focusOn lane (column + 1); true
            | ("}" | "]"), _ -> focusOn (lane + 1) column; true
            | ("{" | "["), _ -> focusOn (lane - 1) column; true
            | ("j" | "CursorDown"), Some p when atEdge p 1 && lane < blocks.Length - 1 ->
                focusOn (lane + 1) column
                pane lane column |> Option.iter _.Home()
                true
            | ("k" | "CursorUp"), Some p when atEdge p -1 && lane > 0 ->
                focusOn (lane - 1) column
                pane lane column |> Option.iter _.End()
                true
            | "Enter", Some p ->
                result <- p.HighlightedId
                app.RequestStop()
                true
            | ("Esc" | "q" | "v"), _ ->
                app.RequestStop()
                true
            | k, Some _ when k.Length = 1 && Char.IsDigit k.[0] && int k > 0 && int k <= (snd blocks.[lane]).Length ->
                focusOn lane (int k - 1)
                true
            | _, Some p -> listKeys p key
            | _ -> false)

    flashRun <- flashRun + 1
    result, filtered
