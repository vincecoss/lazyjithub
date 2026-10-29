module LazyJithub.Ui.App

open System
open System.Collections.Generic
open System.Diagnostics
open System.Text.Json
open System.Threading.Tasks
open Terminal.Gui.App
open Terminal.Gui.Drawing
open Terminal.Gui.Input
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open LazyJithub
open LazyJithub.Config
open LazyJithub.GitHub
open LazyJithub.Model
open LazyJithub.Text
open LazyJithub.Render
open LazyJithub.Ui.Widgets

type Start =
    | StartProject of owner: string * number: int
    | StartSearch of string
    | StartLast

type private PanelId =
    | Sources
    | Statuses
    | Repos
    | Lanes
    | Items
    | Detail

let private panelOrder = [ Sources; Statuses; Repos; Lanes; Items; Detail ]

let private hints =
    function
    | Sources -> "enter open · tab projects/searches · / filter · : search · h/l panels · ? help · q quit"
    | Statuses -> "j/k pick status · space multi-select · esc all · enter items · ? help"
    | Repos -> "j/k pick repository · space multi-select · esc all · enter items · ? help"
    | Lanes -> "tab group by · j/k pick group · esc all · ? help"
    | Items -> "tab scope · s sort · m mine · v board · S stats · / filter · o browser · ? help"
    | Detail -> "j/k scroll · J/K next/prev item · } { sections · L links · / search · o browser · ? help"

let private searches =
    [ "Assigned to me", "is:open assignee:@me sort:updated-desc"
      "My pull requests", "is:open is:pr author:@me sort:updated-desc"
      "Review requested", "is:open is:pr review-requested:@me sort:updated-desc"
      "Created by me", "is:open is:issue author:@me sort:updated-desc"
      "Mentioning me", "is:open mentions:@me sort:updated-desc" ]

let private openExternal (target: string) =
    try
        let opener = if OperatingSystem.IsMacOS() then "open" else "xdg-open"
        let psi = ProcessStartInfo(opener, [| target |])
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        Process.Start psi |> ignore
        true
    with _ ->
        false

let rec private describe (e: exn) =
    match e with
    | :? AggregateException as a when a.InnerExceptions.Count = 1 -> describe a.InnerException
    | GitHubError msg -> msg
    | ReadOnly.ReadOnlyViolation msg -> msg
    | ConfigError msg -> msg
    | e -> e.Message

type Controller(app: IApplication, client: GitHubClient, config: Config, state: State, start: Start) as this =
    let window =
        new Window(BorderStyle = LineStyle.None, ShadowStyle = Nullable ShadowStyles.None)

    let sidebar =
        new View(
            X = Pos.Absolute 0,
            Y = Pos.Absolute 0,
            Width = Dim.Percent 42,
            Height = Dim.Fill(Dim.Absolute 1),
            CanFocus = true
        )

    let sources =
        new ListPanel(1, [ "Projects"; "Searches" ], X = Pos.Absolute 0, Y = Pos.Absolute 0, Width = Dim.Fill())

    let statuses =
        new ListPanel(2, [ "Status" ], X = Pos.Absolute 0, Y = Pos.Bottom sources, Width = Dim.Fill())

    let reposPanel =
        new ListPanel(3, [ "Repositories" ], X = Pos.Absolute 0, Y = Pos.Bottom statuses, Width = Dim.Fill())

    let lanes =
        new ListPanel(4, [ "None" ], X = Pos.Absolute 0, Y = Pos.Bottom reposPanel, Width = Dim.Fill())

    let itemsPanel =
        new ListPanel(5, [ "Items" ], X = Pos.Absolute 0, Y = Pos.Bottom lanes, Width = Dim.Fill(), Height = Dim.Fill())

    let detail =
        new DetailPanel(X = Pos.Right sidebar, Y = Pos.Absolute 0, Width = Dim.Fill(), Height = Dim.Fill(Dim.Absolute 1))

    let statusLine = new StatusLine(X = Pos.Absolute 0, Y = Pos.AnchorEnd 1, Width = Dim.Fill())
    let router = KeyRouter()

    let listPanels =
        [ Sources, sources; Statuses, statuses; Repos, reposPanel; Lanes, lanes; Items, itemsPanel ]

    let mutable projects: Project list = []
    let mutable projectsLoaded = false
    let mutable me: string option = None
    let mutable data: BoardData option = None
    let mutable laneModes: LaneMode list = [ NoLanes ]
    let mutable savedLaneMode = state.Get<string> "lane_mode" |> Option.defaultValue "none"
    let mutable filters = Filters.empty
    let mutable statusChecked: Set<int> = Set.empty
    let mutable statusCursor: int option = None
    let mutable repoChecked: Set<string> = Set.empty
    let mutable repoCursor: string option = None
    let mutable mineOnly = false
    let mutable savedFilterState: string = ""
    let mutable view: ItemView option = None
    let mutable back: string list = []
    let mutable layout = 0
    let mutable previousVisit: DateTimeOffset option = None
    let mutable sourceLoading = false
    let mutable applying = false
    let mutable messageToken = 0

    let mutable sortMode =
        state.Get<string> "sort" |> Option.bind SortMode.ofId |> Option.defaultValue ByStatus

    let mutable searchHistory = state.Get<string list> "search_history" |> Option.defaultValue []

    let rememberSearch (query: string) =
        searchHistory <- query :: (searchHistory |> List.filter ((<>) query)) |> List.truncate 20
        state.Set("search_history", searchHistory)
    let itemCache = Dictionary<string, JsonElement>()
    let seen = HashSet<string>()
    let generations = Dictionary<string, int>()

    let ui (action: unit -> unit) = app.Invoke(Action(action))

    /// Runs `work` off the UI thread; only the latest run of a group reports back.
    let launch (group: string) (work: unit -> Task<'T>) (onOk: 'T -> unit) (onError: exn -> unit) =
        let generation =
            match generations.TryGetValue group with
            | true, n -> n + 1
            | _ -> 1

        generations.[group] <- generation
        let current () = generations.[group] = generation

        Task.Run(fun () ->
            task {
                try
                    let! result = work ()
                    ui (fun () -> if current () then onOk result)
                with e ->
                    ui (fun () -> if current () then onError e)
            }
            :> Task)
        |> ignore

    /// Drops the pending run of a group, so it no longer reports back.
    let cancel (group: string) =
        match generations.TryGetValue group with
        | true, n -> generations.[group] <- n + 1
        | _ -> ()

    let notify (text: string) (color: string) =
        messageToken <- messageToken + 1
        let token = messageToken
        statusLine.SetMessage(Some(colored color text))

        app.AddTimeout(
            TimeSpan.FromSeconds 6.0,
            fun () ->
                if messageToken = token then
                    statusLine.SetMessage None

                false
        )
        |> ignore

    let info text = notify text Palette.green
    let warn text = notify text Palette.yellow
    let error (e: exn) = notify (describe e) Palette.red

    let statusColorOf (name: string) (category: StatusCategory) =
        statusColor config.StatusColors (data |> Option.map _.Columns |> Option.defaultValue []) name category

    let focusedPanel () =
        if detail.HasFocus then
            Some Detail
        else
            listPanels |> List.tryFind (fun (_, p) -> p.HasFocus) |> Option.map fst

    let panelOf id =
        listPanels |> List.tryFind (fun (p, _) -> p = id) |> Option.map snd

    let relayout () =
        let focused = focusedPanel ()
        let available = max 12 (app.Screen.Height - 1)
        let itemsFocused = focused = Some Items || focused = Some Detail
        let budget = available - (if itemsFocused then max 6 (available / 2) else 5)

        let wanted =
            [ Sources, sources, 3, max 8 (available * 2 / 5)
              Statuses, statuses, 8, 14
              Repos, reposPanel, 5, 12
              Lanes, lanes, (if filters.LaneMode = NoLanes then 3 else 5), 14 ]
            |> List.map (fun (id, p, collapsed, expanded) ->
                let rows = p.Items.Length + 2
                id, p, max 3 (min rows (if focused = Some id then expanded else collapsed)))

        let shrink (heights: (PanelId * ListPanel * int) list) victim =
            let excess = (heights |> List.sumBy (fun (_, _, h) -> h)) - budget

            if excess <= 0 then
                heights
            else
                heights |> List.map (fun (id, p, h) -> if id = victim then id, p, max 3 (h - excess) else id, p, h)

        let unfocused =
            [ Lanes; Repos; Statuses; Sources ] |> List.filter (fun id -> focused <> Some id)

        let heights = (wanted, unfocused @ (focused |> Option.toList)) ||> List.fold shrink

        for _, p, h in heights do
            p.Height <- Dim.Absolute h

        sidebar.Visible <- layout < 2
        sidebar.Width <- Dim.Percent(if layout = 0 then 42 else 30)
        detail.X <- (if layout = 2 then Pos.Absolute 0 else Pos.Right sidebar)

        for _, p in listPanels do
            p.EnsureVisible()

        window.SetNeedsLayout()

    let onFocusChanged () =
        match focusedPanel () with
        | Some id -> statusLine.SetHints(hints id)
        | None -> ()

        relayout ()

    let countLabel (label: string) (count: int) (isChecked: bool option) =
        [ match isChecked with
          | Some true -> boldColored Palette.green "✓ "
          | Some false -> seg "  "
          | None -> ()
          seg label
          (if count > 0 then dim $" {count}" else dim " 0") ]

    let currentSource () = data |> Option.map _.Source

    let fillSources () =
        let current = currentSource ()

        let marker active =
            if active then colored Palette.green "● " else seg "  "

        let items =
            match sources.Tab with
            | 0 ->
                projects
                |> List.sortBy (fun p -> p.Closed)
                |> List.map (fun p ->
                    let active = current |> Option.exists (fun s -> s.Kind = ProjectSource && s.Id = p.Id)

                    Row.make
                        $"p:{p.Id}"
                        [ marker active
                          (if active then bold p.Title
                           elif p.Closed then dim p.Title
                           else seg p.Title)
                          dim $"  {p.Owner} #{p.Number}"
                          if p.Closed then dim " · closed" ]
                        $"{p.Title} {p.Owner} {p.Number}")
            | _ ->
                let saved =
                    searches
                    |> List.mapi (fun n (name, query) ->
                        let active = current |> Option.exists (fun s -> s.Kind = SearchSource && s.Id = query)
                        Row.make $"q:{n}" [ marker active; colored Palette.yellow "★ "; (if active then bold name else seg name); dim $"  {query}" ] $"{name} {query}")

                let recent =
                    searchHistory
                    |> List.mapi (fun n q ->
                        let active = current |> Option.exists (fun s -> s.Kind = SearchSource && s.Id = q)
                        Row.make $"h:{n}" [ marker active; colored Palette.teal "› "; seg q ] q)

                [ Row.make "search:new" [ seg "  "; colored Palette.teal ": "; dim "new search" ] "new search" ]
                @ saved
                @ recent

        let keep =
            current
            |> Option.map (fun s ->
                match s.Kind with
                | ProjectSource -> $"p:{s.Id}"
                | SearchSource ->
                    match searches |> List.tryFindIndex (fun (_, q) -> q = s.Id), List.tryFindIndex ((=) s.Id) searchHistory with
                    | Some n, _ -> $"q:{n}"
                    | None, Some n -> $"h:{n}"
                    | None, None -> "")

        match keep with
        | Some id -> sources.SetItems(items, id)
        | None -> sources.SetItems items

        relayout ()

    let filtersKey () =
        data |> Option.map (fun d -> $"filters:{d.Source.StateKey}")

    let effectiveRepos () =
        if not repoChecked.IsEmpty then repoChecked
        else repoCursor |> Option.toList |> Set.ofList

    let saveFilters () =
        match data, filtersKey () with
        | Some d, Some key ->
            let columns = List.toArray d.Columns

            let saved =
                dict
                    [ "statuses", box (statusChecked |> Seq.filter (fun i -> i < columns.Length) |> Seq.map (fun i -> columns.[i].Name) |> Seq.sort |> Seq.toArray)
                      "status_cursor", box (statusCursor |> Option.filter (fun i -> i < columns.Length) |> Option.map (fun i -> columns.[i].Name) |> Option.toObj)
                      "repos", box (repoChecked |> Set.toArray)
                      "repo_cursor", box (Option.toObj repoCursor)
                      "lane", box (filters.Lane |> Option.map (fun l -> [| LaneMode.id filters.LaneMode; l |]) |> Option.toObj) ]

            let text = JsonSerializer.Serialize saved

            if text <> savedFilterState then
                savedFilterState <- text
                state.Set(key, saved)
        | _ -> ()

    let restoreFilters () =
        match data, filtersKey () with
        | Some d, Some key ->
            let saved = state.Get<JsonElement> key
            let columns = d.Columns |> List.mapi (fun i c -> c.Name, i) |> Map.ofList
            let known = BoardData.repos d |> Set.ofList
            let strs name = saved |> Option.map (Json.arr name >> List.choose Json.asString) |> Option.defaultValue []
            let one name = saved |> Option.bind (Json.str name)
            statusChecked <- strs "statuses" |> List.choose columns.TryFind |> Set.ofList
            statusCursor <- one "status_cursor" |> Option.bind columns.TryFind
            repoChecked <- strs "repos" |> List.filter known.Contains |> Set.ofList
            repoCursor <- one "repo_cursor" |> Option.filter known.Contains
            savedFilterState <- saved |> Option.map (fun s -> s.GetRawText()) |> Option.defaultValue ""
            filters <- { filters with Repos = effectiveRepos (); Lane = None }

            match strs "lane" with
            | [ mode; lane ] when mode = LaneMode.id filters.LaneMode -> $"l:{lane}"
            | _ -> "all"
        | _ -> "all"

    let fillColumns () =
        match data with
        | Some d ->
            let items =
                Row.text "all" "All"
                :: (d.Columns |> List.mapi (fun i c -> Row.text $"c{i}" c.Name))

            let keep = statusCursor |> Option.map (fun i -> $"c{i}") |> Option.defaultValue "all"
            statuses.SetItems(items, keep)
        | None -> ()

    let fillRepos () =
        match data with
        | Some d ->
            let items =
                Row.text "all" "All" :: (BoardData.repos d |> List.map (fun r -> Row.text $"r:{r}" r))

            let keep = repoCursor |> Option.map (fun r -> $"r:{r}") |> Option.defaultValue "all"
            reposPanel.SetItems(items, keep)
        | None -> ()

    let fillLanes (keep: string option) =
        match data with
        | Some d ->
            let items =
                Row.text "all" "All"
                :: (BoardData.lanes d filters |> List.map (fun l -> Row.text $"l:{l.Key}" l.Label))

            match keep with
            | Some k -> lanes.SetItems(items, k)
            | None -> lanes.SetItems items

            lanes.SetNote(if filters.LaneMode = NoLanes then "tab to group" else "")
        | None -> ()

    let isChanged (item: Item) =
        match previousVisit, item.Updated with
        | Some visit, Some updated -> updated > visit && not (seen.Contains item.Key)
        | _ -> false

    /// In the Status and Repositories panels, checked rows filter; with none checked, the highlight does.
    let applyFilters () =
        match data with
        | None -> ()
        | Some d when not applying ->
            applying <- true

            try
                let columnCount = d.Columns.Length
                statusCursor <- statusCursor |> Option.filter (fun i -> i < columnCount)
                statusChecked <- statusChecked |> Set.filter (fun i -> i < columnCount)

                let columnFilter =
                    if statusChecked.IsEmpty then statusCursor |> Option.toList |> Set.ofList else statusChecked

                let repoFilter = effectiveRepos ()

                if repoFilter <> filters.Repos then
                    filters <- { filters with Repos = repoFilter }
                    fillLanes None

                let lane =
                    lanes.HighlightedId
                    |> Option.filter (fun id -> id.StartsWith "l:")
                    |> Option.map (fun id -> id.Substring 2)

                filters <-
                    { filters with
                        Columns = columnFilter
                        Lane = lane
                        Assignee = if mineOnly then me else None }

                let counts = BoardData.columnCounts d filters |> List.toArray

                statuses.UpdateItems(
                    Row.make "all" (countLabel "All" (BoardData.totalForColumns d filters) (Some false)) "all"
                    :: (d.Columns
                        |> List.mapi (fun i c ->
                            let label = countLabel c.Name counts.[i] (Some(statusChecked.Contains i))

                            let label =
                                match c.Color with
                                | Some color -> label |> List.mapi (fun n s -> if n = 1 then colored color s.Text else s)
                                | None -> label

                            Row.make $"c{i}" label c.Name))
                )

                statuses.SetNote(if statusChecked.IsEmpty then "" else $"{statusChecked.Count} selected")

                let repoCounts = BoardData.repoCounts d filters

                reposPanel.UpdateItems(
                    Row.make "all" (countLabel "All" (repoCounts |> Map.toSeq |> Seq.sumBy snd) (Some false)) "all"
                    :: (BoardData.repos d
                        |> List.map (fun r ->
                            let count = Map.tryFind r repoCounts |> Option.defaultValue 0
                            Row.make $"r:{r}" (countLabel r count (Some(repoChecked.Contains r))) r))
                )

                reposPanel.SetNote(if repoChecked.IsEmpty then "" else $"{repoChecked.Count} selected")

                let laneItems = BoardData.lanes d filters
                let allLanes = BoardData.filtered d { filters with Lane = None } ByRank |> List.length

                lanes.UpdateItems(
                    Row.make "all" (countLabel "All" allLanes None) "all"
                    :: (laneItems |> List.map (fun l -> Row.make $"l:{l.Key}" (countLabel l.Label l.Count None) l.Label))
                )

                let shown = BoardData.filtered d filters sortMode
                let now = DateTimeOffset.UtcNow
                let keyWidth = shown |> List.fold (fun acc i -> max acc i.ShortKey.Length) 0

                let statusIds =
                    d.Columns |> List.indexed |> List.filter (fun (i, _) -> columnFilter.Contains i) |> List.map (snd >> _.Id)

                let showStatus = statusIds.Length <> 1

                let rows =
                    shown
                    |> List.mapi (fun n i ->
                        [ if sortMode = ByRepo && n > 0 && i.Repo <> shown.[n - 1].Repo then
                              Row.separator $"sep:{i.Repo}"
                          Row.make
                              i.Key
                              (itemRow i keyWidth showStatus statusColorOf now (isChanged i))
                              $"{i.Key} {i.Title} {String.Join(' ', i.Assignees)} {String.Join(' ', i.Labels |> List.map _.Name)} {i.Status}" ])
                    |> List.concat

                itemsPanel.SetItems rows

                let doneCount, _, _ = progress shown
                let total = d.Items |> List.filter (BoardData.inScope d filters.Scope) |> List.length

                let note =
                    [ if filters.Scope = Iteration then
                          match BoardData.currentIteration d with
                          | Some it -> $"""{it.Title} until {it.Start.AddDays(float (it.Days - 1)).ToString("MMM d")}"""
                          | None -> "no current iteration"
                      if not shown.IsEmpty && (filters.Scope = Iteration || d.Source.Kind = SearchSource) then
                          $"{progressBar 8 doneCount shown.Length} {doneCount}/{shown.Length}"
                      if shown.IsEmpty then
                          (if total = 0 then "empty" else $"filters hide all {total}, X clears")
                      if mineOnly then "mine"
                      if d.Truncated then $"first {d.Items.Length} of {d.Total}"
                      SortMode.label sortMode ]

                itemsPanel.SetNote(String.Join(" · ", note))
                sources.SetNote d.Source.Name
                saveFilters ()
            finally
                applying <- false
        | Some _ -> ()

    let itemOf (key: string) =
        data |> Option.bind (fun d -> d.Items |> List.tryFind (fun i -> i.Key = key))

    let headerFor (v: ItemView) (changed: bool) =
        let h = v.Header

        let kind =
            match itemOf h.Key |> Option.bind _.IssueType with
            | Some t -> $"{kindLabel h.Kind} · {t}"
            | None -> kindLabel h.Kind

        [ [ boldColored Palette.blue (if h.Kind = DraftKind then "draft" else h.Key)
            seg "  "
            stateGlyph h.Kind h.State h.StateReason h.IsDraft
            seg $" {kind}"
            match h.Status, itemOf h.Key with
            | Some status, Some i when i.ItemId.IsSome ->
                seg "  "
                boldColored (statusColorOf status i.Category) status
            | _ -> ()
            if changed then colored Palette.yellow "  • changed since your last visit" ] ]

    let showRaw (key: string) (raw: JsonElement) =
        let item = itemOf key
        let fields = data |> Option.map _.Fields |> Option.defaultValue []
        let v = itemView raw item fields statusColorOf DateTimeOffset.UtcNow
        let changed = item |> Option.exists isChanged
        view <- Some v
        let title = if key.StartsWith "draft:" then "Draft" else key
        detail.SetTitle [ (if detail.HasFocus then boldColored Palette.green title else bold title) ]
        detail.Show(Some key, headerFor v changed, v.Header.Title, v.Blocks)

        detail.SetFooter(
            match back with
            | prev :: _ -> $"backspace: back to {prev}"
            | [] -> ""
        )

        if seen.Add key && changed then
            applyFilters ()

    let fetchItem (key: string) : Task<JsonElement> =
        match itemOf key with
        | Some i when i.ContentId <> "" -> client.Content i.ContentId
        | _ ->
            match key.LastIndexOf '#' with
            | -1 -> Task.FromException<JsonElement>(GitHubError $"{key} is not owner/repo#number")
            | at ->
                match Int32.TryParse(key.Substring(at + 1)) with
                | true, n -> client.ContentByNumber(key.Substring(0, at), n)
                | _ -> Task.FromException<JsonElement>(GitHubError $"{key} is not owner/repo#number")

    let showItem (key: string) (delay: int) =
        match itemCache.TryGetValue key with
        | true, raw ->
            cancel "detail"
            showRaw key raw
        | _ ->
            detail.SetTitle [ bold key; colored Palette.yellow " loading…" ]

            launch
                "detail"
                (fun () ->
                    task {
                        do! Task.Delay delay
                        return! fetchItem key
                    })
                (fun raw ->
                    itemCache.[key] <- raw
                    showRaw key raw)
                (fun e ->
                    detail.SetTitle [ bold key ]
                    detail.Show(Some key, [ [ colored Palette.red (describe e) ] ], "", []))

    let welcome () =
        detail.Show(
            None,
            [ [ boldColored Palette.blue "lazyjithub"; dim $"  {config.Site}" ]
              [ dim "pick a project and press enter · ? shows all keys" ] ],
            "",
            [ yield! Help.sections
                     |> List.filter (fun (title, _) -> title = "Panels" || title = "Move")
                     |> List.collect (fun (title, rows) ->
                         [ Rich.Heading title
                           Rich.Grid(rows |> List.map (fun (keys, action) -> Help.keySegments keys, [ seg action ])) ]) ]
        )

    /// Drops cached items that changed on GitHub since the last load, and shows the open one again if it did.
    let forgetChanged (previous: BoardData) (loaded: BoardData) =
        let before = previous.Items |> List.map (fun i -> i.Key, i.Updated) |> Map.ofList

        let changed =
            loaded.Items
            |> List.filter (fun i -> Map.tryFind i.Key before |> Option.exists ((<>) i.Updated))
            |> List.map _.Key
            |> set

        for key in changed do
            itemCache.Remove key |> ignore

        match detail.Key with
        | Some key when changed.Contains key -> showItem key 0
        | _ -> ()

    let laneTabs (d: BoardData) =
        laneModes <- BoardData.laneModes d

        let mode =
            laneModes |> List.tryFind (fun m -> LaneMode.id m = savedLaneMode) |> Option.defaultValue NoLanes

        filters <- { filters with LaneMode = mode }
        lanes.SetTabs(laneModes |> List.map LaneMode.label, List.findIndex ((=) mode) laneModes)

    let rec openSource (source: Source) (reload: bool) =
        let same = data |> Option.exists (fun d -> d.Source = source)
        let refresh = same && not reload
        sourceLoading <- true

        if not refresh then
            itemsPanel.SetLoading true

        launch
            "board"
            (fun () ->
                task {
                    let now = DateTimeOffset.UtcNow

                    let! columns, fields, items, total =
                        task {
                            match source.Kind with
                            | ProjectSource ->
                                let! board = client.Board(source.Id, config.MaxItems)
                                return projectColumns board.Fields board.Items, board.Fields, board.Items, board.Total
                            | SearchSource ->
                                let! found, total = client.Search(source.Id, config.MaxItems)
                                return searchColumns found, [], found, total
                        }

                    return
                        { Source = source
                          Columns = columns
                          Fields = fields
                          Items = items
                          Total = total
                          Truncated = items.Length < total && items.Length >= config.MaxItems
                          Cutoff = now.AddDays(float -config.DoneDays)
                          Today = DateTime.Today }
                })
            (fun loaded ->
                let previous = data |> Option.filter (fun d -> d.Source = source)
                sourceLoading <- false
                itemsPanel.SetLoading false
                data <- Some loaded

                let scopes = BoardData.scopes loaded

                let scope =
                    (if same then Some filters.Scope else None)
                    |> Option.orElse (state.Get<string> $"scope:{source.StateKey}" |> Option.bind Scope.ofId)
                    |> Option.filter (fun s -> List.contains s scopes)
                    |> Option.defaultValue scopes.Head

                filters <- { filters with Scope = scope }
                laneTabs loaded
                let laneKeep = if same then None else Some(restoreFilters ())

                if not same then
                    previousVisit <-
                        state.Get<int64> $"visit:{source.StateKey}"
                        |> Option.map DateTimeOffset.FromUnixTimeSeconds

                    seen.Clear()
                    state.Set($"visit:{source.StateKey}", DateTimeOffset.UtcNow.ToUnixTimeSeconds())

                state.Set("source", Source.toState source)
                state.Set($"scope:{source.StateKey}", Scope.id scope)
                itemsPanel.SetTabs(scopes |> List.map Scope.label, List.findIndex ((=) scope) scopes)

                let tab =
                    match source.Kind with
                    | ProjectSource -> 0
                    | SearchSource -> 1

                if sources.Tab <> tab then
                    sources.SetTabs(sources.Tabs, tab)

                fillSources ()
                fillColumns ()
                fillRepos ()
                fillLanes laneKeep
                applyFilters ()
                relayout ()

                if loaded.Truncated && not refresh then
                    warn $"Showing the first {loaded.Items.Length} of {loaded.Total} items (max_items)"

                if not same then
                    itemsPanel.FocusList()
                    detail.Key |> Option.iter (fun key -> showItem key 0)

                previous |> Option.iter (fun p -> forgetChanged p loaded))
            (fun e ->
                sourceLoading <- false
                itemsPanel.SetLoading false
                error e)

    and openRow (id: string) =
        match id.Split(':', 2) with
        | [| "p"; ident |] ->
            projects |> List.tryFind (fun p -> p.Id = ident) |> Option.iter (fun p -> openSource (Source.project p) false)
        | [| "q"; n |] ->
            match Int32.TryParse n with
            | true, n when n < searches.Length -> openSource (Source.search (snd searches.[n])) false
            | _ -> ()
        | [| "h"; n |] ->
            match Int32.TryParse n with
            | true, n when n < searchHistory.Length -> openSource (Source.search searchHistory.[n]) false
            | _ -> ()
        | [| "search"; "new" |] -> this.PromptSearch()
        | _ -> ()

    let startSource () =
        match start with
        | StartProject(owner, number) ->
            match projects |> List.tryFind (fun p -> String.Equals(p.Owner, owner, StringComparison.OrdinalIgnoreCase) && p.Number = number) with
            | Some p -> openSource (Source.project p) false
            | None -> launch "project" (fun () -> client.Project(owner, number)) (fun p -> openSource (Source.project p) false) error
        | StartSearch query ->
            rememberSearch query
            openSource (Source.search query) false
        | StartLast ->
            match state.Get<string[]> "source" |> Option.bind Source.ofState with
            | Some s -> openSource s false
            | None -> ()

    let loadProjects () =
        sources.SetLoading true

        launch
            "projects"
            (fun () -> client.Projects())
            (fun (login, found, problems) ->
                sources.SetLoading false
                projectsLoaded <- true
                projects <- found
                me <- if login = "" then None else Some login
                fillSources ()

                match problems with
                | [] -> ()
                | first :: _ when found.IsEmpty ->
                    detail.Show(None, [ [ colored Palette.red first ] ], "", [ Rich.Para [ seg "Searches still work: tab in panel 1, or : to type one." ] ])
                | _ ->
                    let saml = problems |> List.filter (fun p -> p.Contains "SAML") |> List.length

                    if saml > 0 then
                        warn $"Projects of {saml} organization(s) with SAML SSO are missing: authorize the token for them"
                    else
                        warn $"Some projects are missing: {problems.Head}"

                startSource ())
            (fun e ->
                sources.SetLoading false
                error e

                detail.Show(
                    None,
                    [ [ colored Palette.red (describe e) ] ],
                    "",
                    [ Rich.Para
                          [ seg "Check "
                            colored Palette.code config.Source
                            seg ", "
                            colored Palette.code "GITHUB_TOKEN"
                            seg " or "
                            colored Palette.code "gh auth status"
                            seg ". The token needs the "
                            colored Palette.code "read:project"
                            seg " scope: "
                            colored Palette.code "gh auth refresh -s read:project" ] ]
                ))

    let focusPanel (id: PanelId) =
        if layout = 2 && id <> Detail then
            layout <- 0

        match id with
        | Detail -> detail.FocusBody()
        | other -> panelOf other |> Option.iter _.FocusList()

        onFocusChanged ()

    let cycle (step: int) =
        let order = if layout = 2 then [ Detail ] else panelOrder
        let current = focusedPanel () |> Option.filter (fun p -> List.contains p order) |> Option.defaultValue order.Head
        let index = List.findIndex ((=) current) order
        focusPanel order.[(index + step + order.Length) % order.Length]

    let goToItem (key: string) (remember: bool) =
        match detail.Key with
        | Some current when remember && current <> key -> back <- (current :: back) |> List.truncate 50
        | _ -> ()

        if itemsPanel.HighlightedId = Some key then
            showItem key 0
            itemsPanel.FocusList()
        elif itemsPanel.Select key then
            itemsPanel.FocusList()
        else
            showItem key 0
            detail.FocusBody()

        onFocusChanged ()

    let goToRef (text: string) =
        let repos = data |> Option.map BoardData.repos |> Option.defaultValue []
        let current = detail.Key |> Option.bind itemOf |> Option.map _.Repo |> Option.filter ((<>) "")

        let current =
            current
            |> Option.orElse (detail.Key |> Option.filter (fun k -> k.Contains '#') |> Option.map (fun k -> k.Substring(0, k.LastIndexOf '#')))

        match Item.parseRef repos current text with
        | Some key -> goToItem key true
        | None -> warn $"'{text}' is not an issue: use owner/repo#12, repo#12, #12 or a URL"

    let clearFilters () =
        statusChecked <- Set.empty
        repoChecked <- Set.empty
        statusCursor <- None
        repoCursor <- None
        mineOnly <- false
        filters <- { filters with Repos = Set.empty }
        itemsPanel.ClearFilter()
        fillColumns ()
        fillRepos ()
        fillLanes (Some "all")
        applyFilters ()

    let copy (text: string) (label: string) =
        if app.Clipboard <> null && app.Clipboard.TrySetClipboardData text then
            info $"Copied {label}"
        else
            warn "Clipboard unavailable"

    let showLinks () =
        match view with
        | Some v when v.Links.IsEmpty && v.Mentions.IsEmpty && v.Urls.IsEmpty ->
            info $"{v.Header.Key} has no linked or mentioned issues"
        | Some v ->
            let rows =
                [ for l in v.Links -> l.Relation, l.Key, Some l
                  for k in v.Mentions -> "mentioned", k, None
                  for u in v.Urls -> "link in text", u, None ]

            let width = rows |> List.fold (fun acc (relation, _, _) -> max acc relation.Length) 0

            let items =
                rows
                |> List.map (fun (relation, id, link) ->
                    let target =
                        match link with
                        | Some l ->
                            [ stateGlyph l.Kind l.State l.StateReason l.IsDraft
                              seg " "
                              boldColored Palette.blue l.Key
                              seg $" {l.Title}" ]
                        | None when id.StartsWith "http" -> [ { seg id with Color = Some Palette.link; Style = Underline } ]
                        | None ->
                            match itemOf id with
                            | Some i -> [ itemGlyph i; seg " "; boldColored Palette.blue id; seg $" {i.Title}" ]
                            | None -> [ boldColored Palette.blue id ]

                    Row.make id (dim (relation.PadRight width + " ") :: target) $"{relation} {id} {segmentsText target}")

            match Dialogs.pick app router $"Links · {v.Header.Key}" "enter goes to or opens · esc closes" items with
            | Some url when url.StartsWith "http" -> openExternal url |> ignore
            | Some key -> goToItem key true
            | None -> ()
        | None -> ()

    let showStats () =
        match data with
        | Some d ->
            let shown = BoardData.filtered d filters sortMode

            let section (title: string) (groups: (string * string * int) list) =
                let top = groups |> List.sortByDescending (fun (_, _, n) -> n) |> List.truncate 15
                let most = top |> List.fold (fun acc (_, _, n) -> max acc n) 1

                [ Rich.Heading title
                  Rich.grid (
                      top
                      |> List.map (fun (name, color, n) -> name, [ colored color (String('▓', max 1 (n * 24 / most))); seg $" {n}" ])
                  ) ]

            let by (key: Item -> string) (color: Item -> string) =
                shown |> List.groupBy key |> List.map (fun (k, group) -> k, color group.Head, group.Length)

            let doneCount, doing, todo = progress shown
            let now = DateTimeOffset.UtcNow

            let labels =
                shown
                |> List.collect _.Labels
                |> List.groupBy _.Name
                |> List.map (fun (name, all) -> name, all.Head.Color, all.Length)

            let stale =
                shown
                |> List.filter (fun i -> i.Category <> Done)
                |> List.choose (fun i -> daysSince now i.Updated |> Option.map (fun days -> i, days))
                |> List.sortByDescending snd
                |> List.truncate 10

            let document =
                [ Rich.Para
                      [ bold (string shown.Length)
                        seg " items · "
                        boldColored Palette.green (string doneCount)
                        seg " done · "
                        boldColored Palette.blue (string doing)
                        seg " in progress · "
                        boldColored Palette.grey (string todo)
                        seg " to do" ]
                  yield! section "Status" (by _.Status (fun i -> statusColorOf i.Status i.Category))
                  yield! section "Assignee" (by (fun i -> List.tryHead i.Assignees |> Option.defaultValue "Unassigned") (fun _ -> Palette.lavender))
                  yield! section "Repository" (by (fun i -> if i.Repo = "" then "Draft issues" else i.Repo) (fun _ -> Palette.blue))
                  yield! section "Type" (by (fun i -> match BoardData.laneKey d ByType i with "" -> "Issue" | t -> t) (fun i -> defaultArg (itemGlyph i).Color Palette.grey))
                  if not labels.IsEmpty then
                      yield! section "Labels" labels
                  if not stale.IsEmpty then
                      Rich.Heading "Longest without an update"

                      for i, days in stale do
                          Rich.Item(
                              ageSegment (Some days) i.Category,
                              [ Rich.Para
                                    [ seg " "
                                      boldColored Palette.blue i.ShortKey
                                      seg " "
                                      boldColored (statusColorOf i.Status i.Category) i.Status
                                      seg $" {i.Title}" ] ]
                          ) ]

            Dialogs.document app router [ bold $"Statistics · {d.Source.Name}" ] "} { sections · esc closes" document
        | None -> ()

    let showBoard () =
        match data with
        | Some d ->
            let load (useFilters: bool) =
                let scope =
                    if useFilters then { filters with Columns = Set.empty }
                    else { filters with Columns = Set.empty; Repos = Set.empty }

                let shown = BoardData.filtered d scope ByRank
                let grouped = scope.LaneMode <> NoLanes && scope.Lane.IsNone

                let columns =
                    d.Columns
                    |> List.indexed
                    |> List.filter (fun (i, _) -> not useFilters || filters.Columns.IsEmpty || filters.Columns.Contains i)
                    |> List.map snd

                let block (name: string option) (items: Item list) : Dialogs.BoardLane =
                    { Name = name
                      Columns =
                        columns
                        |> List.map (fun c ->
                            c.Name,
                            items
                            |> List.filter (fun i -> i.StatusId = c.Id)
                            |> List.map (fun i -> ({ Item = i; Changed = isChanged i; Lane = name }: Dialogs.BoardCard))) }

                let groups =
                    if grouped then
                        BoardData.lanes d scope
                        |> List.map (fun l -> block (Some l.Label) (shown |> List.filter (fun i -> BoardData.laneKey d scope.LaneMode i = l.Key)))
                        |> List.filter (fun b -> b.Columns |> List.exists (snd >> List.isEmpty >> not))
                    else
                        []

                if groups.IsEmpty then [ block None shown ] else groups

            let useFilters = state.Get<bool> "board_filters" |> Option.defaultValue true
            let picked, keptFilters = Dialogs.kanban app router load useFilters detail.Key statusColorOf

            if keptFilters <> useFilters then
                state.Set("board_filters", keptFilters)

            match picked with
            | Some key -> goToItem key true
            | None -> ()
        | None -> ()

    let toggleMine () =
        match me with
        | Some _ ->
            mineOnly <- not mineOnly
            applyFilters ()
            info (if mineOnly then "Showing only your items" else "Showing everyone's items")
        | None -> warn "Your account is not loaded yet"

    let reload (quiet: bool) =
        itemCache.Clear()

        match data with
        | Some d -> openSource d.Source true
        | None -> ()

        match detail.Key with
        | Some key when data.IsNone -> showItem key 0
        | _ -> ()

        if not projectsLoaded then loadProjects ()
        if not quiet then info "Reloading…"

    let cycleLayout (index: int option) =
        layout <- defaultArg index ((layout + 1) % 3)

        if layout = 2 then
            detail.FocusBody()

        relayout ()

    let browse () =
        let url =
            match detail.Key, view with
            | Some key, Some v when v.Header.Key = key && v.Header.Url <> "" -> Some v.Header.Url
            | _ ->
                match currentSource () with
                | Some s when s.Kind = ProjectSource && s.Url <> "" -> Some s.Url
                | Some s when s.Kind = SearchSource -> Some $"{config.WebUrl}/search?type=issues&q={Uri.EscapeDataString s.Id}"
                | _ -> None

        match url with
        | Some u -> openExternal u |> ignore
        | None -> ()

    let listKeys (panel: ListPanel) (key: string) =
        match key with
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
        | "Enter" -> panel.Activate(); true
        | "Space" -> panel.Toggle(); true
        | "Esc" -> panel.Escape(); true
        | "/" -> panel.StartFilter(); true
        | "Tab"
        | "]" ->
            (if panel.Tabs.Length > 1 then panel.NextTab 1 else cycle 1)
            true
        | "Shift+Tab"
        | "[" ->
            (if panel.Tabs.Length > 1 then panel.NextTab -1 else cycle -1)
            true
        | _ -> false

    let detailKeys (key: string) =
        match key with
        | "j"
        | "CursorDown" -> detail.Scroll 1; true
        | "k"
        | "CursorUp" -> detail.Scroll -1; true
        | "Ctrl+D"
        | "PageDown"
        | "Space" -> detail.Page 1; true
        | "Ctrl+U"
        | "PageUp" -> detail.Page -1; true
        | "Home" -> detail.Top(); true
        | "End" -> detail.Bottom(); true
        | "}"
        | "]" -> detail.JumpSection 1; true
        | "{"
        | "[" -> detail.JumpSection -1; true
        | "Tab" -> cycle 1; true
        | "Shift+Tab" -> cycle -1; true
        | "/" ->
            Dialogs.prompt app router "Search in the item" [] "" |> Option.iter detail.Search
            true
        | "n" -> detail.NextMatch 1; true
        | "N" -> detail.NextMatch -1; true
        | "Esc" ->
            if detail.Searching then detail.ClearSearch()
            true
        | "Enter" -> true
        | _ -> false

    let globalKeys (key: string) =
        match key with
        | "q" -> app.RequestStop(); true
        | "?" -> Dialogs.lines app router [ boldColored Palette.green "Help" ] Help.lines; true
        | "1" -> focusPanel Sources; true
        | "2" -> focusPanel Statuses; true
        | "3" -> focusPanel Repos; true
        | "4" -> focusPanel Lanes; true
        | "5" -> focusPanel Items; true
        | "0" -> focusPanel Detail; true
        | "h"
        | "CursorLeft" -> cycle -1; true
        | "l"
        | "CursorRight" -> cycle 1; true
        | "J" -> itemsPanel.Move 1; true
        | "K" -> itemsPanel.Move -1; true
        | "s" ->
            sortMode <- SortMode.next sortMode
            state.Set("sort", SortMode.id sortMode)
            applyFilters ()
            true
        | "X" -> clearFilters (); true
        | "m" -> toggleMine (); true
        | "o" -> browse (); true
        | "y" -> detail.Key |> Option.iter (fun k -> copy k k); true
        | "Y" ->
            view |> Option.filter (fun v -> v.Header.Url <> "") |> Option.iter (fun v -> copy v.Header.Url "URL")
            true
        | "c" ->
            view |> Option.iter (fun v -> copy $"[{v.Header.Key}]({v.Header.Url}) {v.Header.Title}" "Markdown link")
            true
        | "C" ->
            view |> Option.iter (fun v -> copy v.Markdown $"{v.Header.Key} as Markdown")
            true
        | "g" ->
            Dialogs.prompt app router "Go to (owner/repo#12, repo#12, #12 or a URL)" [] "" |> Option.iter goToRef
            true
        | "L" -> showLinks (); true
        | "v" -> showBoard (); true
        | "S" -> showStats (); true
        | ":" -> this.PromptSearch(); true
        | "Backspace" ->
            match back with
            | prev :: rest ->
                back <- rest
                goToItem prev false
            | [] -> info "No previous item"

            true
        | "r" -> reload false; true
        | "+" -> cycleLayout None; true
        | _ -> false

    let mainKeys (key: Key) =
        let name = keyName key
        let filtering = listPanels |> List.tryFind (fun (_, p) -> p.IsFiltering)

        match filtering with
        | Some(_, panel) -> Dialogs.filterKeys panel key
        | None ->
            let handled =
                match focusedPanel () with
                | Some Detail -> detailKeys name
                | Some id -> panelOf id |> Option.map (fun p -> listKeys p name) |> Option.defaultValue false
                | None -> false

            handled || globalKeys name || name = "Esc"

    do
        sidebar.Add(sources, statuses, reposPanel, lanes, itemsPanel) |> ignore
        window.Add(sidebar, detail, statusLine) |> ignore
        window.FrameChanged.Add(fun _ -> relayout ())

        app.Keyboard.KeyDown.Add(fun key ->
            if not key.Handled && router.Handle key then
                key.Handled <- true)

        router.Push mainKeys
        app.Navigation.FocusedChanged.Add(fun _ -> onFocusChanged ())

        sources.OnActivated <- openRow
        sources.OnTabChanged <- fun _ -> fillSources ()

        statuses.OnHighlighted <-
            fun id ->
                statusCursor <-
                    id
                    |> Option.filter (fun i -> i.StartsWith "c")
                    |> Option.map (fun i -> int (i.Substring 1))

                applyFilters ()

        statuses.OnToggled <-
            fun id ->
                if id = "all" then
                    statusChecked <- Set.empty
                else
                    let i = int (id.Substring 1)
                    statusChecked <- if statusChecked.Contains i then statusChecked.Remove i else statusChecked.Add i

                applyFilters ()

        statuses.OnReset <-
            fun () ->
                statusChecked <- Set.empty
                statuses.Select "all" |> ignore
                applyFilters ()

        statuses.OnActivated <- fun _ -> focusPanel Items

        reposPanel.OnHighlighted <-
            fun id ->
                repoCursor <-
                    id
                    |> Option.filter (fun i -> i.StartsWith "r:")
                    |> Option.map (fun i -> i.Substring 2)

                applyFilters ()

        reposPanel.OnToggled <-
            fun id ->
                if id = "all" then
                    repoChecked <- Set.empty
                else
                    let repo = id.Substring 2
                    repoChecked <- if repoChecked.Contains repo then repoChecked.Remove repo else repoChecked.Add repo

                applyFilters ()

        reposPanel.OnReset <-
            fun () ->
                repoChecked <- Set.empty
                reposPanel.Select "all" |> ignore
                applyFilters ()

        reposPanel.OnActivated <- fun _ -> focusPanel Items
        lanes.OnHighlighted <- fun _ -> applyFilters ()
        lanes.OnReset <- fun () -> lanes.Select "all" |> ignore
        lanes.OnActivated <- fun _ -> focusPanel Items

        lanes.OnTabChanged <-
            fun tab ->
                if tab < laneModes.Length then
                    filters <- { filters with LaneMode = laneModes.[tab]; Lane = None }
                    savedLaneMode <- LaneMode.id filters.LaneMode
                    state.Set("lane_mode", savedLaneMode)
                    fillLanes (Some "all")
                    applyFilters ()
                    relayout ()

        itemsPanel.OnHighlighted <-
            fun id ->
                match id with
                | Some key when not (key.StartsWith "sep:") -> showItem key 120
                | _ -> ()

        itemsPanel.OnActivated <- fun _ -> focusPanel Detail

        itemsPanel.OnTabChanged <-
            fun tab ->
                match data with
                | Some d ->
                    let scopes = BoardData.scopes d

                    if tab < scopes.Length then
                        filters <- { filters with Scope = scopes.[tab] }
                        state.Set($"scope:{d.Source.StateKey}", Scope.id filters.Scope)
                        fillLanes None
                        applyFilters ()
                | None -> ()

        if config.RefreshSeconds > 0 then
            app.AddTimeout(
                TimeSpan.FromSeconds(float config.RefreshSeconds),
                fun () ->
                    match data with
                    | Some d when not sourceLoading -> openSource d.Source false
                    | _ -> ()

                    true
            )
            |> ignore

    member _.Window = window

    member _.PromptSearch() =
        let initial =
            match currentSource () with
            | Some s when s.Kind = SearchSource -> s.Id
            | _ -> ""

        match Dialogs.prompt app router "Search GitHub (is:open assignee:@me repo:owner/name …)" searchHistory initial with
        | Some query ->
            rememberSearch query
            openSource (Source.search query) false
        | None -> ()

    member _.Start() =
        welcome ()
        statusLine.SetHints(hints Sources)

        // Terminal.Gui picks its own focus when the session starts, so ours must come after.
        app.AddTimeout(
            TimeSpan.FromMilliseconds 1.0,
            fun () ->
                if data.IsNone then
                    focusPanel Sources

                false
        )
        |> ignore

        loadProjects ()

    interface IDisposable with
        member _.Dispose() = window.Dispose()

let run (client: GitHubClient) (config: Config) (start: Start) =
    use app = Application.Create(null)
    app.Init(null) |> ignore
    let state = State(config.Site)
    use controller = new Controller(app, client, config, state, start)
    controller.Start()
    app.Run(controller.Window, null) |> ignore
