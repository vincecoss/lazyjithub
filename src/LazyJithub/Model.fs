module LazyJithub.Model

open System
open System.Globalization
open System.Text.Json
open LazyJithub.Json

type StatusCategory =
    | ToDo
    | InProgress
    | Done

module StatusCategory =
    let order =
        function
        | ToDo -> 0
        | InProgress -> 1
        | Done -> 2

    let private doneWords = [ "done"; "closed"; "complete"; "shipped"; "released"; "merged"; "terminé"; "fait" ]

    let private progressWords =
        [ "progress"; "doing"; "review"; "testing"; "qa"; "started"; "active"; "cours"; "révision" ]

    /// GitHub has no status categories: the name says what it can.
    let ofName (name: string) =
        let lowered = name.ToLowerInvariant()

        if doneWords |> List.exists lowered.Contains then Done
        elif progressWords |> List.exists lowered.Contains then InProgress
        else ToDo

type Project =
    { Id: string
      Number: int
      Title: string
      Owner: string
      Url: string
      Closed: bool
      Updated: DateTimeOffset option }

module Project =
    let ofJson (e: JsonElement) =
        { Id = strOr "" "id" e
          Number = num "number" e |> Option.defaultValue 0
          Title = strOr "?" "title" e
          Owner = pathStr [ "owner"; "login" ] e |> Option.defaultValue ""
          Url = strOr "" "url" e
          Closed = flag "closed" e = Some true
          Updated = str "updatedAt" e |> Option.bind parseDate }

type FieldOption =
    { Id: string
      Name: string
      Color: string option }

type Iteration =
    { Id: string
      Title: string
      Start: DateTime
      Days: int }

    member this.Contains(day: DateTime) =
        day.Date >= this.Start && day.Date < this.Start.AddDays(float this.Days)

type Field =
    { Id: string
      Name: string
      DataType: string
      Options: FieldOption list
      Iterations: Iteration list }

/// GitHub names project colors; these are its dark theme shades.
let optionColor (name: string) =
    match name.ToUpperInvariant() with
    | "GRAY" -> Some "#9198a1"
    | "BLUE" -> Some "#4493f8"
    | "GREEN" -> Some "#3fb950"
    | "YELLOW" -> Some "#d29922"
    | "ORANGE" -> Some "#db6d28"
    | "RED" -> Some "#f85149"
    | "PINK" -> Some "#db61a2"
    | "PURPLE" -> Some "#ab7df8"
    | _ -> None

let private parseDay (value: string option) =
    match value with
    | Some s ->
        match DateTime.TryParseExact(s, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, d -> Some d
        | _ -> None
    | None -> None

module Field =
    let private iteration (e: JsonElement) =
        parseDay (str "startDate" e)
        |> Option.map (fun start ->
            { Id = strOr "" "id" e
              Title = strOr "?" "title" e
              Start = start
              Days = num "duration" e |> Option.defaultValue 14 })

    let ofJson (e: JsonElement) =
        let config = prop "configuration" e

        let iterations name =
            config |> Option.map (arr name >> List.choose iteration) |> Option.defaultValue []

        { Id = strOr "" "id" e
          Name = strOr "?" "name" e
          DataType = strOr "" "dataType" e
          Options =
            arr "options" e
            |> List.map (fun o ->
                { Id = strOr "" "id" o
                  Name = strOr "?" "name" o
                  Color = str "color" o |> Option.bind optionColor })
          Iterations = iterations "completedIterations" @ iterations "iterations" |> List.sortBy _.Start }

    let isSingleSelect (f: Field) = f.DataType = "SINGLE_SELECT"
    let isIteration (f: Field) = f.DataType = "ITERATION"

    /// The field a GitHub board groups its columns by: "Status" when the project has it.
    let status (fields: Field list) =
        let choices = fields |> List.filter isSingleSelect

        choices
        |> List.tryFind (fun f -> String.Equals(f.Name, "Status", StringComparison.OrdinalIgnoreCase))
        |> Option.orElse (List.tryHead choices)

type FieldValue =
    | Choice of optionId: string * name: string
    | IterationValue of id: string * title: string
    | TextValue of string
    | NumberValue of float
    | DateValue of string

module FieldValue =
    let text =
        function
        | Choice(_, name) -> name
        | IterationValue(_, title) -> title
        | TextValue t -> t
        | NumberValue n -> n.ToString("0.##", CultureInfo.InvariantCulture)
        | DateValue d -> d

    /// The value and the name of its field; None for the values GitHub also gives as plain issue fields.
    let ofJson (e: JsonElement) : (string * FieldValue) option =
        let field = pathStr [ "field"; "name" ] e

        let value =
            match str "__typename" e with
            | Some "ProjectV2ItemFieldSingleSelectValue" ->
                str "name" e |> Option.map (fun name -> Choice(strOr "" "optionId" e, name))
            | Some "ProjectV2ItemFieldIterationValue" ->
                str "title" e |> Option.map (fun title -> IterationValue(strOr "" "iterationId" e, title))
            | Some "ProjectV2ItemFieldTextValue" -> str "text" e |> Option.map TextValue
            | Some "ProjectV2ItemFieldDateValue" -> str "date" e |> Option.map DateValue
            | Some "ProjectV2ItemFieldNumberValue" ->
                match prop "number" e with
                | Some n when n.ValueKind = JsonValueKind.Number -> Some(NumberValue(n.GetDouble()))
                | _ -> None
            | _ -> None

        match field, value with
        | Some f, Some v when f <> "Title" -> Some(f, v)
        | _ -> None

type Kind =
    | IssueKind
    | PullKind
    | DraftKind

type Label = { Name: string; Color: string }

module Label =
    let ofJson (e: JsonElement) =
        { Name = strOr "?" "name" e
          Color = "#" + strOr "7f848e" "color" e }

type Item =
    { Key: string
      ItemId: string option
      ContentId: string
      Kind: Kind
      Number: int option
      Repo: string
      Title: string
      State: string
      StateReason: string option
      IsDraft: bool
      Url: string
      Assignees: string list
      Author: string option
      Labels: Label list
      Milestone: string option
      IssueType: string option
      ParentKey: string option
      ParentTitle: string option
      SubIssues: (int * int) option
      StatusId: string
      Status: string
      Category: StatusCategory
      Fields: Map<string, FieldValue>
      Updated: DateTimeOffset option
      Created: DateTimeOffset option
      Closed: DateTimeOffset option }

    member this.IsOpen = this.State = "OPEN"

    /// `repo#12` without the owner, or "draft" for a draft issue.
    member this.ShortKey =
        match this.Number with
        | Some n ->
            let name = this.Repo.Substring(this.Repo.IndexOf '/' + 1)
            $"{name}#{n}"
        | None -> "draft"

let noStatus = "No Status"

module Item =
    let key (repo: string) (number: int) = $"{repo}#{number}"

    /// `owner/repo#12`, `repo#12` within `repos`, `#12` within `current`, or an issue or pull request URL.
    let parseRef (repos: string list) (current: string option) (text: string) : string option =
        let text = text.Trim()
        let m = Text.RegularExpressions.Regex.Match(text, @"^(?:https?://[^/]+/)?([\w.-]+)/([\w.-]+)(?:#|/issues/|/pull/)(\d+)\b")

        if m.Success then
            Some(key $"{m.Groups.[1].Value}/{m.Groups.[2].Value}" (int m.Groups.[3].Value))
        else
            let m = Text.RegularExpressions.Regex.Match(text, @"^([\w.-]+)?#(\d+)$")

            if not m.Success then
                None
            elif m.Groups.[1].Success then
                let name = m.Groups.[1].Value

                repos
                |> List.tryFind (fun r -> String.Equals(r.Substring(r.IndexOf '/' + 1), name, StringComparison.OrdinalIgnoreCase))
                |> Option.map (fun r -> key r (int m.Groups.[2].Value))
            else
                current |> Option.map (fun r -> key r (int m.Groups.[2].Value))

    let private logins (name: string) (e: JsonElement) =
        nodes name e |> List.choose (str "login")

    /// A search result has no project fields: its state is its column.
    let stateStatus (state: string) (isDraft: bool) =
        match state with
        | "MERGED" -> "Merged"
        | "CLOSED" -> "Closed"
        | _ when isDraft -> "Draft"
        | _ -> "Open"

    let category (state: string) (status: string) =
        match state with
        | "CLOSED"
        | "MERGED" -> Done
        | _ -> StatusCategory.ofName status

    /// An issue, pull request or draft issue; `item` is its project item, when it comes from a project.
    let ofContent (content: JsonElement) (item: JsonElement option) (statusField: string option) : Item option =
        let typename = strOr "" "__typename" content

        let kind =
            match typename with
            | "Issue" -> Some IssueKind
            | "PullRequest" -> Some PullKind
            | "DraftIssue" -> Some DraftKind
            | _ -> None

        kind
        |> Option.map (fun kind ->
            let fields =
                item
                |> Option.map (nodes "fieldValues" >> List.choose FieldValue.ofJson >> Map.ofList)
                |> Option.defaultValue Map.empty

            let repo = pathStr [ "repository"; "nameWithOwner" ] content |> Option.defaultValue ""
            let number = num "number" content
            let itemId = item |> Option.bind (str "id")
            let state = if kind = DraftKind then "OPEN" else strOr "OPEN" "state" content
            let isDraft = flag "isDraft" content = Some true

            let status =
                match item, statusField |> Option.bind (fun f -> Map.tryFind f fields) with
                | Some _, Some(Choice(id, name)) -> id, name
                | Some _, _ -> "", noStatus
                | None, _ ->
                    let name = stateStatus state isDraft
                    $"state:{name.ToLowerInvariant()}", name

            let dates name =
                [ yield! str name content |> Option.bind parseDate |> Option.toList
                  yield! item |> Option.bind (str name) |> Option.bind parseDate |> Option.toList ]

            let parentRepo = pathStr [ "parent"; "repository"; "nameWithOwner" ] content

            { Key =
                match number with
                | Some n when repo <> "" -> key repo n
                | _ -> "draft:" + (itemId |> Option.defaultValue (strOr "" "id" content))
              ItemId = itemId
              ContentId = strOr "" "id" content
              Kind = kind
              Number = if kind = DraftKind then None else number
              Repo = repo
              Title = strOr "" "title" content
              State = state
              StateReason = str "stateReason" content
              IsDraft = isDraft
              Url = strOr "" "url" content
              Assignees = logins "assignees" content
              Author = pathStr [ "author"; "login" ] content |> Option.orElse (pathStr [ "creator"; "login" ] content)
              Labels = nodes "labels" content |> List.map Label.ofJson
              Milestone = pathStr [ "milestone"; "title" ] content
              IssueType = pathStr [ "issueType"; "name" ] content
              ParentKey =
                match parentRepo, path [ "parent"; "number" ] content with
                | Some r, Some n when n.ValueKind = JsonValueKind.Number -> Some(key r (n.GetInt32()))
                | _ -> None
              ParentTitle = pathStr [ "parent"; "title" ] content
              SubIssues =
                match prop "subIssuesSummary" content with
                | Some s when num "total" s |> Option.exists (fun t -> t > 0) ->
                    Some(num "completed" s |> Option.defaultValue 0, num "total" s |> Option.defaultValue 0)
                | _ -> None
              StatusId = fst status
              Status = snd status
              Category = category state (snd status)
              Fields = fields
              Updated = dates "updatedAt" |> List.sortDescending |> List.tryHead
              Created = dates "createdAt" |> List.tryHead
              Closed = str "closedAt" content |> Option.orElse (str "mergedAt" content) |> Option.bind parseDate })

    /// A project item; None for archived items and for those the token cannot see.
    let ofProjectItem (statusField: string option) (e: JsonElement) : Item option =
        if flag "isArchived" e = Some true then
            None
        else
            prop "content" e |> Option.bind (fun content -> ofContent content (Some e) statusField)

type SourceKind =
    | ProjectSource
    | SearchSource

type Scope =
    | Board
    | Iteration
    | All
    | Results

module Scope =
    let label =
        function
        | Board -> "Board"
        | Iteration -> "Iteration"
        | All -> "All"
        | Results -> "Results"

    let id = label >> _.ToLowerInvariant()

    let ofId (s: string) =
        [ Board; Iteration; All; Results ] |> List.tryFind (fun x -> id x = s)

/// `Id` is the project node id, or the search query.
type Source =
    { Kind: SourceKind
      Id: string
      Name: string
      Owner: string
      Number: int
      Url: string }

    member this.StateKey =
        match this.Kind with
        | ProjectSource -> $"project:{this.Owner}/{this.Number}"
        | SearchSource -> $"search:{this.Id}"

module Source =
    let project (p: Project) =
        { Kind = ProjectSource
          Id = p.Id
          Name = p.Title
          Owner = p.Owner
          Number = p.Number
          Url = p.Url }

    let search (query: string) =
        { Kind = SearchSource
          Id = query
          Name = query
          Owner = ""
          Number = 0
          Url = "" }

    /// Saved as plain strings so the state file needs no F# option support.
    let toState (s: Source) =
        [| (match s.Kind with
            | ProjectSource -> "project"
            | SearchSource -> "search")
           s.Id
           s.Name
           s.Owner
           string s.Number
           s.Url |]

    let ofState (fields: string[]) =
        match fields with
        | [| kind; id; name; owner; number; url |] ->
            let kind =
                match kind with
                | "project" -> Some ProjectSource
                | "search" -> Some SearchSource
                | _ -> None

            kind
            |> Option.map (fun k ->
                { Kind = k
                  Id = id
                  Name = name
                  Owner = owner
                  Number =
                    match Int32.TryParse number with
                    | true, n -> n
                    | _ -> 0
                  Url = url })
        | _ -> None

type Column =
    { Id: string
      Name: string
      Color: string option }

type SortMode =
    | ByStatus
    | ByRepo
    | ByRank
    | ByUpdated

module SortMode =
    let all = [ ByStatus; ByRepo; ByRank; ByUpdated ]

    let label =
        function
        | ByStatus -> "by status, repository, rank"
        | ByRepo -> "by repository, status, rank"
        | ByRank -> "by rank"
        | ByUpdated -> "recently updated first"

    let id =
        function
        | ByStatus -> "status"
        | ByRepo -> "repo"
        | ByRank -> "rank"
        | ByUpdated -> "updated"

    let ofId s = all |> List.tryFind (fun m -> id m = s)

    let next m =
        all.[(List.findIndex ((=) m) all + 1) % all.Length]

type LaneMode =
    | NoLanes
    | ByAssignee
    | ByIteration
    | ByMilestone
    | ByParent
    | ByType
    | ByField of string

module LaneMode =
    let label =
        function
        | NoLanes -> "None"
        | ByAssignee -> "Assignee"
        | ByIteration -> "Iteration"
        | ByMilestone -> "Milestone"
        | ByParent -> "Parent"
        | ByType -> "Type"
        | ByField name -> name

    let id =
        function
        | ByField name -> $"field:{name}"
        | mode -> (label mode).ToLowerInvariant()

    let emptyLabel =
        function
        | ByAssignee -> "Unassigned"
        | ByIteration -> "No iteration"
        | ByMilestone -> "No milestone"
        | ByParent -> "No parent"
        | ByType -> "No type"
        | ByField name -> $"No {name}"
        | NoLanes -> "All"

type Lane = { Key: string; Label: string; Count: int }

/// Empty `Columns` or `Repos` means no filter on that dimension.
type Filters =
    { Scope: Scope
      Columns: Set<int>
      Repos: Set<string>
      Lane: string option
      LaneMode: LaneMode
      Assignee: string option }

module Filters =
    let empty =
        { Scope = Board
          Columns = Set.empty
          Repos = Set.empty
          Lane = None
          LaneMode = NoLanes
          Assignee = None }

type BoardData =
    { Source: Source
      Columns: Column list
      Fields: Field list
      Items: Item list
      Total: int
      Truncated: bool
      /// Closed items older than this stay off the Board tab.
      Cutoff: DateTimeOffset
      Today: DateTime }

module BoardData =
    let iterationField (data: BoardData) = data.Fields |> List.tryFind Field.isIteration

    let currentIteration (data: BoardData) =
        iterationField data |> Option.bind (fun f -> f.Iterations |> List.tryFind (fun i -> i.Contains data.Today))

    let scopes (data: BoardData) =
        match data.Source.Kind with
        | SearchSource -> [ Results ]
        | ProjectSource -> [ Board; if (iterationField data).IsSome then Iteration; All ]

    /// The group-by choices: fixed ones, then each single-select field but the one of the columns.
    let laneModes (data: BoardData) =
        let status = Field.status data.Fields |> Option.map _.Name

        [ NoLanes
          ByAssignee
          if (iterationField data).IsSome then ByIteration
          ByMilestone
          ByParent
          ByType
          yield!
              data.Fields
              |> List.filter (fun f -> Field.isSingleSelect f && Some f.Name <> status)
              |> List.map (fun f -> ByField f.Name) ]

    let laneKey (data: BoardData) (mode: LaneMode) (item: Item) =
        let field name =
            Map.tryFind name item.Fields |> Option.map FieldValue.text |> Option.defaultValue ""

        match mode with
        | ByAssignee -> List.tryHead item.Assignees |> Option.defaultValue ""
        | ByIteration -> iterationField data |> Option.map (fun f -> field f.Name) |> Option.defaultValue ""
        | ByMilestone -> item.Milestone |> Option.defaultValue ""
        | ByParent -> item.ParentKey |> Option.defaultValue ""
        | ByType ->
            match item.Kind, item.IssueType with
            | PullKind, _ -> "Pull request"
            | DraftKind, _ -> "Draft"
            | IssueKind, Some t -> t
            | IssueKind, None -> ""
        | ByField name -> field name
        | NoLanes -> ""

    let laneLabel (data: BoardData) (mode: LaneMode) (item: Item) =
        match laneKey data mode item with
        | "" -> LaneMode.emptyLabel mode
        | key when mode = ByParent ->
            match item.ParentTitle with
            | Some title -> $"{key} {title}"
            | None -> key
        | key -> key

    let inScope (data: BoardData) (scope: Scope) (item: Item) =
        match scope with
        | Board -> item.IsOpen || (item.Closed |> Option.orElse item.Updated |> Option.exists (fun d -> d >= data.Cutoff))
        | Iteration ->
            match iterationField data, currentIteration data with
            | Some f, Some current ->
                match Map.tryFind f.Name item.Fields with
                | Some(IterationValue(id, _)) -> id = current.Id
                | _ -> false
            | _ -> false
        | All
        | Results -> true

    let private matches (data: BoardData) (filters: Filters) column lane repo (item: Item) =
        let columns = List.toArray data.Columns

        inScope data filters.Scope item
        && (not repo || filters.Repos.IsEmpty || filters.Repos.Contains item.Repo)
        && (not column
            || filters.Columns.IsEmpty
            || filters.Columns |> Seq.exists (fun i -> i < columns.Length && columns.[i].Id = item.StatusId))
        && (not lane
            || filters.LaneMode = NoLanes
            || filters.Lane.IsNone
            || Some(laneKey data filters.LaneMode item) = filters.Lane)
        && (filters.Assignee.IsNone || item.Assignees |> List.exists (fun a -> Some a = filters.Assignee))

    let columnIndex (data: BoardData) =
        data.Columns |> List.mapi (fun n c -> c.Id, n) |> Map.ofList

    /// `data.Items` is in project order, as GitHub returns it, so rank is the list index.
    let filtered (data: BoardData) (filters: Filters) (sort: SortMode) : Item list =
        let columnOf = columnIndex data
        let rank = data.Items |> List.mapi (fun n i -> i.Key, n) |> Map.ofList
        let nColumns = data.Columns.Length

        let statusKey (i: Item) =
            Map.tryFind i.StatusId columnOf |> Option.defaultValue nColumns, i.Status

        let byStatus a b = compare (statusKey a) (statusKey b)
        let byRepo (a: Item) (b: Item) = compare a.Repo b.Repo
        let byRank (a: Item) (b: Item) = compare rank.[a.Key] rank.[b.Key]
        let byUpdated (a: Item) (b: Item) = compare b.Updated a.Updated

        let order =
            match sort with
            | ByStatus -> [ byStatus; byRepo; byRank ]
            | ByRepo -> [ byRepo; byStatus; byRank ]
            | ByRank -> [ byRank ]
            | ByUpdated -> [ byUpdated; byRank ]

        let compareAll a b =
            order
            |> List.tryPick (fun cmp ->
                match cmp a b with
                | 0 -> None
                | n -> Some n)
            |> Option.defaultValue 0

        data.Items |> List.filter (matches data filters true true true) |> List.sortWith compareAll

    let columnCounts (data: BoardData) (filters: Filters) =
        let pool = data.Items |> List.filter (matches data filters false true true)
        data.Columns |> List.map (fun c -> pool |> List.filter (fun i -> i.StatusId = c.Id) |> List.length)

    let totalForColumns (data: BoardData) (filters: Filters) =
        data.Items |> List.filter (matches data filters false true true) |> List.length

    let repos (data: BoardData) : string list =
        data.Items |> List.map _.Repo |> List.filter ((<>) "") |> List.distinct |> List.sort

    let repoCounts (data: BoardData) (filters: Filters) : Map<string, int> =
        data.Items
        |> List.filter (matches data filters true true false)
        |> List.countBy _.Repo
        |> Map.ofList

    /// A lane stays listed while the column filter hides all its items; it then counts 0.
    let lanes (data: BoardData) (filters: Filters) : Lane list =
        let mode = filters.LaneMode

        if mode = NoLanes then
            []
        else
            let present = data.Items |> List.filter (matches data filters false false true)
            let counted = present |> List.filter (matches data filters true false true)

            let position (key: string) =
                match mode with
                | ByField name ->
                    data.Fields
                    |> List.tryFind (fun f -> f.Name = name)
                    |> Option.bind (fun f -> f.Options |> List.tryFindIndex (fun o -> o.Name = key))
                    |> Option.defaultValue 1000
                | ByIteration ->
                    iterationField data
                    |> Option.bind (fun f -> f.Iterations |> List.tryFindIndex (fun i -> i.Title = key))
                    |> Option.defaultValue 1000
                | _ -> 0

            present
            |> List.distinctBy (laneKey data mode)
            |> List.map (fun i ->
                let key = laneKey data mode i

                { Key = key
                  Label = laneLabel data mode i
                  Count = counted |> List.filter (fun c -> laneKey data mode c = key) |> List.length })
            |> List.sortBy (fun l -> l.Key = "", position l.Key, l.Label.ToLowerInvariant())

/// "No Status" first, as on a GitHub board, while some item has no status;
/// then the options of the status field in their order.
let projectColumns (fields: Field list) (items: Item list) : Column list =
    [ if items |> List.exists (fun i -> i.StatusId = "") then
          { Id = ""; Name = noStatus; Color = None }
      yield!
          Field.status fields
          |> Option.map (fun f -> f.Options |> List.map (fun o -> { Id = o.Id; Name = o.Name; Color = o.Color }))
          |> Option.defaultValue [] ]

let private stateOrder = [ "Open"; "Draft"; "Merged"; "Closed" ]

let searchColumns (items: Item list) : Column list =
    stateOrder
    |> List.filter (fun name -> items |> List.exists (fun i -> i.Status = name))
    |> List.map (fun name ->
        { Id = $"state:{name.ToLowerInvariant()}"
          Name = name
          Color = None })

let progress (items: Item list) =
    let count category =
        items |> List.filter (fun i -> i.Category = category) |> List.length

    count Done, count InProgress, count ToDo
