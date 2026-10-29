module LazyJithub.Render

open System
open System.Text.Json
open LazyJithub.Json
open LazyJithub.Model
open LazyJithub.Text

let categoryColor =
    function
    | ToDo -> Palette.grey
    | InProgress -> Palette.blue
    | Done -> Palette.green

/// The color set in the config, then the one set in the project, then one by category.
let statusColor (colors: Map<string, string>) (columns: Column list) (name: string) (category: StatusCategory) =
    colors
    |> Map.tryFind (name.ToLowerInvariant())
    |> Option.orElse (columns |> List.tryFind (fun c -> c.Name = name) |> Option.bind _.Color)
    |> Option.defaultValue (categoryColor category)

let private openGreen = "#3fb950"
let private mergedPurple = "#ab7df8"

let stateGlyph (kind: Kind) (state: string) (stateReason: string option) (isDraft: bool) : Segment =
    match kind, state with
    | DraftKind, _ -> dim "◌"
    | PullKind, "MERGED" -> colored mergedPurple "⇄"
    | PullKind, "CLOSED" -> colored Palette.red "⇄"
    | PullKind, _ when isDraft -> dim "⇄"
    | PullKind, _ -> colored openGreen "⇄"
    | IssueKind, "CLOSED" when stateReason = Some "NOT_PLANNED" || stateReason = Some "DUPLICATE" -> dim "⊘"
    | IssueKind, "CLOSED" -> colored mergedPurple "✓"
    | IssueKind, _ -> colored openGreen "○"

let itemGlyph (i: Item) = stateGlyph i.Kind i.State i.StateReason i.IsDraft

let stateLabel (kind: Kind) (state: string) (stateReason: string option) (isDraft: bool) =
    match kind, state with
    | DraftKind, _ -> "Draft issue"
    | PullKind, "MERGED" -> "Merged"
    | PullKind, "CLOSED" -> "Closed"
    | PullKind, _ when isDraft -> "Draft"
    | PullKind, _ -> "Open"
    | IssueKind, "CLOSED" ->
        match stateReason with
        | Some "NOT_PLANNED" -> "Closed as not planned"
        | Some "DUPLICATE" -> "Closed as duplicate"
        | _ -> "Closed"
    | IssueKind, _ -> "Open"

let kindLabel =
    function
    | IssueKind -> "Issue"
    | PullKind -> "Pull request"
    | DraftKind -> "Draft"

let initials (login: string option) =
    match login with
    | None -> "--"
    | Some l ->
        let letters = l |> Seq.filter Char.IsLetterOrDigit |> Seq.truncate 2 |> Seq.toArray
        String(letters).ToUpperInvariant().PadRight(2)

/// Days since the last update: grey under 7, yellow up to 13, red from 14, grey once done.
let ageSegment (days: int option) (category: StatusCategory) =
    match days with
    | None -> seg "    "
    | Some d ->
        let text = (if d >= 1000 then $"{d / 365}y" else $"{d}d").PadLeft(4)

        if category = Done || d < 7 then dim text
        elif d < 14 then colored Palette.yellow text
        else colored Palette.red text

let daysSince (now: DateTimeOffset) (value: DateTimeOffset option) =
    value |> Option.map (fun d -> max 0 (int (now - d).TotalDays))

let formatDate (value: DateTimeOffset option) =
    value |> Option.map (fun d -> d.ToLocalTime().ToString("yyyy-MM-dd HH:mm")) |> Option.defaultValue ""

/// Joins segments with dim commas.
let commaList (parts: Segment list) : Segment list =
    parts |> List.mapi (fun n s -> if n = 0 then [ s ] else [ dim ", "; s ]) |> List.concat

let labelChips (labels: Label list) : Segment list =
    labels |> List.map (fun l -> colored l.Color l.Name) |> commaList

let itemRow
    (item: Item)
    (keyWidth: int)
    (showStatus: bool)
    (statusColorOf: string -> StatusCategory -> string)
    (now: DateTimeOffset)
    (changed: bool)
    : Segment list =
    [ (if changed then colored Palette.yellow "•" else seg " ")
      boldColored Palette.blue (item.ShortKey.PadRight keyWidth)
      seg " "
      itemGlyph item
      seg " "
      (match item.Assignees with
       | first :: _ -> colored Palette.lavender (initials (Some first))
       | [] -> dim "--")
      seg " "
      ageSegment (daysSince now item.Updated) item.Category
      seg " "
      if showStatus then
          boldColored (statusColorOf item.Status item.Category) item.Status
          seg " "
      seg item.Title ]

let progressBar (width: int) (doneCount: int) (total: int) =
    if total = 0 then
        String('░', width)
    else
        let filled = int (Math.Round(float width * float doneCount / float total))
        String('▓', filled) + String('░', width - filled)

type LinkedItem =
    { Relation: string
      Key: string
      Title: string
      Kind: Kind
      State: string
      StateReason: string option
      IsDraft: bool }

let private linked (relation: string) (kind: Kind) (e: JsonElement) =
    match pathStr [ "repository"; "nameWithOwner" ] e, num "number" e with
    | Some repo, Some n ->
        Some
            { Relation = relation
              Key = Item.key repo n
              Title = strOr "" "title" e
              Kind = kind
              State = strOr "OPEN" "state" e
              StateReason = str "stateReason" e
              IsDraft = flag "isDraft" e = Some true }
    | _ -> None

let linkedItems (raw: JsonElement) : LinkedItem list =
    [ yield! prop "parent" raw |> Option.bind (linked "parent" IssueKind) |> Option.toList
      yield! nodes "subIssues" raw |> List.choose (linked "sub-issue" IssueKind)
      yield! nodes "closedByPullRequestsReferences" raw |> List.choose (linked "closed by" PullKind)
      yield! nodes "closingIssuesReferences" raw |> List.choose (linked "closes" IssueKind) ]

type ItemHeader =
    { Key: string
      Title: string
      Kind: Kind
      State: string
      StateReason: string option
      IsDraft: bool
      Status: string option
      Url: string }

type ItemView =
    { Header: ItemHeader
      Blocks: Rich.Block list
      Markdown: string
      Links: LinkedItem list
      Mentions: string list
      Urls: string list }

let private cellText (s: string) = s.Replace("|", "\\|").Replace("\n", " ")

let private reviewLabel =
    function
    | "APPROVED" -> Some("approved", openGreen)
    | "CHANGES_REQUESTED" -> Some("changes requested", Palette.red)
    | "REVIEW_REQUIRED" -> Some("review required", Palette.yellow)
    | _ -> None

let private checksLabel =
    function
    | "SUCCESS" -> Some("passing", openGreen)
    | "FAILURE"
    | "ERROR" -> Some("failing", Palette.red)
    | "PENDING"
    | "EXPECTED" -> Some("pending", Palette.yellow)
    | _ -> None

/// The detail pane of an issue, pull request or draft issue. `item` is its row in the open
/// project or search, for the project fields; `fields` names those fields in project order.
let itemView
    (raw: JsonElement)
    (item: Item option)
    (fields: Field list)
    (statusColorOf: string -> StatusCategory -> string)
    (now: DateTimeOffset)
    : ItemView =
    let typename = strOr "" "__typename" raw

    let kind =
        match typename with
        | "PullRequest" -> PullKind
        | "DraftIssue" -> DraftKind
        | _ -> IssueKind

    let repo = pathStr [ "repository"; "nameWithOwner" ] raw |> Option.defaultValue ""
    let number = num "number" raw
    let state = if kind = DraftKind then "OPEN" else strOr "OPEN" "state" raw
    let stateReason = str "stateReason" raw
    let isDraft = flag "isDraft" raw = Some true

    let key =
        match number with
        | Some n when repo <> "" -> Item.key repo n
        | _ -> item |> Option.map _.Key |> Option.defaultValue "draft"

    let url =
        match str "url" raw with
        | Some u -> u
        | None -> item |> Option.map _.Url |> Option.defaultValue ""

    let status = item |> Option.map _.Status

    let header =
        { Key = key
          Title = strOr "" "title" raw
          Kind = kind
          State = state
          StateReason = stateReason
          IsDraft = isDraft
          Status = status
          Url = url }

    let dated (value: DateTimeOffset option) =
        match value with
        | Some d -> [ seg (formatDate (Some d)); dim $"  {relativeAge now d}" ]
        | None -> []

    let date name = str name raw |> Option.bind parseDate
    let logins name = nodes name raw |> List.choose (str "login")
    let grid = ResizeArray<string * Segment list>()

    match item, status with
    | Some i, Some s when i.ItemId.IsSome -> grid.Add("Status", [ boldColored (statusColorOf s i.Category) s ])
    | _ -> ()

    let stateText = stateLabel kind state stateReason isDraft
    grid.Add("State", [ stateGlyph kind state stateReason isDraft; seg $" {stateText}" ])

    if repo <> "" then
        grid.Add("Repository", [ seg repo ])

    let people (names: string list) =
        names |> List.map (colored Palette.lavender) |> commaList

    grid.Add(
        "Assignees",
        match logins "assignees" with
        | [] -> [ dim "Unassigned" ]
        | names -> people names
    )

    match pathStr [ "author"; "login" ] raw |> Option.orElse (pathStr [ "creator"; "login" ] raw) with
    | Some a -> grid.Add("Author", [ seg a ])
    | None -> ()

    match nodes "labels" raw |> List.map Label.ofJson with
    | [] -> ()
    | labels -> grid.Add("Labels", labelChips labels)

    match pathStr [ "issueType"; "name" ] raw with
    | Some t -> grid.Add("Type", [ seg t ])
    | None -> ()

    match pathStr [ "milestone"; "title" ] raw with
    | Some m ->
        let due =
            pathStr [ "milestone"; "dueOn" ] raw
            |> Option.bind parseDate
            |> Option.map (fun d -> d.ToString "yyyy-MM-dd")

        let overdue =
            pathStr [ "milestone"; "dueOn" ] raw
            |> Option.bind parseDate
            |> Option.exists (fun d -> d < now && state = "OPEN")

        grid.Add(
            "Milestone",
            [ seg m
              match due with
              | Some d when overdue -> colored Palette.red $"  due {d}"
              | Some d -> dim $"  due {d}"
              | None -> () ]
        )
    | None -> ()

    match prop "parent" raw with
    | Some p ->
        let parent = linked "parent" IssueKind p

        match parent with
        | Some l ->
            grid.Add(
                "Parent",
                [ stateGlyph IssueKind l.State l.StateReason false
                  seg " "
                  boldColored Palette.blue l.Key
                  seg $" {l.Title}" ]
            )
        | None -> ()
    | None -> ()

    match prop "subIssuesSummary" raw with
    | Some s when num "total" s |> Option.exists (fun t -> t > 0) ->
        let total = num "total" s |> Option.defaultValue 0
        let completed = num "completed" s |> Option.defaultValue 0
        grid.Add("Sub-issues", [ colored Palette.blue (progressBar 10 completed total); seg $" {completed}/{total}" ])
    | _ -> ()

    if kind = PullKind then
        match str "headRefName" raw, str "baseRefName" raw with
        | Some head, Some base' ->
            grid.Add("Branch", [ colored Palette.code head; dim " → "; colored Palette.code base' ])
        | _ -> ()

        match str "reviewDecision" raw |> Option.bind reviewLabel with
        | Some(text, color) -> grid.Add("Review", [ colored color text ])
        | None -> ()

        let reviews =
            nodes "latestReviews" raw
            |> List.choose (fun r ->
                pathStr [ "author"; "login" ] r
                |> Option.map (fun who ->
                    let color =
                        match str "state" r with
                        | Some "APPROVED" -> openGreen
                        | Some "CHANGES_REQUESTED" -> Palette.red
                        | _ -> Palette.grey

                    colored color who))

        if not reviews.IsEmpty then
            grid.Add("Reviewed by", commaList reviews)

        let requested =
            nodes "reviewRequests" raw
            |> List.choose (fun r ->
                pathStr [ "requestedReviewer"; "login" ] r
                |> Option.orElse (pathStr [ "requestedReviewer"; "name" ] r))

        if not requested.IsEmpty then
            grid.Add("Requested", people requested)

        let checks =
            nodes "commits" raw
            |> List.tryHead
            |> Option.bind (pathStr [ "commit"; "statusCheckRollup"; "state" ])
            |> Option.bind checksLabel

        match checks with
        | Some(text, color) -> grid.Add("Checks", [ colored color text ])
        | None -> ()

        match num "additions" raw, num "deletions" raw with
        | Some a, Some d ->
            let files = num "changedFiles" raw |> Option.defaultValue 0
            grid.Add("Changes", [ colored openGreen $"+{a}"; seg " "; colored Palette.red $"−{d}"; dim $"  in {files} files" ])
        | _ -> ()

    match item with
    | Some i ->
        let statusField = Field.status fields |> Option.map _.Name

        let ordered =
            fields
            |> List.choose (fun f -> Map.tryFind f.Name i.Fields |> Option.map (fun v -> f.Name, v))
            |> List.filter (fun (name, _) -> Some name <> statusField)

        for name, value in ordered do
            let color =
                fields
                |> List.tryFind (fun f -> f.Name = name)
                |> Option.bind (fun f ->
                    match value with
                    | Choice(id, _) -> f.Options |> List.tryFind (fun o -> o.Id = id) |> Option.bind _.Color
                    | _ -> None)

            let text = FieldValue.text value

            grid.Add(
                name,
                [ match color with
                  | Some c -> colored c text
                  | None -> seg text ]
            )
    | None -> ()

    grid.Add("Created", dated (date "createdAt"))
    grid.Add("Updated", dated (item |> Option.bind _.Updated |> Option.orElse (date "updatedAt")))

    match date "mergedAt", date "closedAt" with
    | Some merged, _ -> grid.Add("Merged", dated (Some merged))
    | None, Some closed -> grid.Add("Closed", dated (Some closed))
    | _ -> ()

    let body = strOr "" "body" raw
    let links = linkedItems raw
    let subIssues = links |> List.filter (fun l -> l.Relation = "sub-issue")
    let pulls = links |> List.filter (fun l -> l.Relation = "closed by" || l.Relation = "closes")
    let commentBlock = prop "comments" raw
    let comments = nodes "comments" raw
    let totalComments = commentBlock |> Option.bind (num "totalCount") |> Option.defaultValue comments.Length
    let author (c: JsonElement) = pathStr [ "author"; "login" ] c |> Option.defaultValue "ghost"

    let linkLine (l: LinkedItem) =
        [ stateGlyph l.Kind l.State l.StateReason l.IsDraft
          seg " "
          boldColored Palette.blue l.Key
          seg $" {l.Title}" ]

    let blocks =
        [ Rich.grid (List.ofSeq grid)
          Rich.Heading "Description"
          match Markdown.toBlocks body with
          | [] -> Rich.Para [ dim "No description." ]
          | parts -> yield! parts
          if not subIssues.IsEmpty then
              let count = prop "subIssues" raw |> Option.bind (num "totalCount") |> Option.defaultValue subIssues.Length
              Rich.Heading $"Sub-issues ({count})"

              for l in subIssues do
                  Rich.Item(seg "", [ Rich.Para(linkLine l) ])
          if not pulls.IsEmpty then
              Rich.Heading(if kind = PullKind then "Closes" else "Linked pull requests")

              for l in pulls do
                  Rich.Item(seg "", [ Rich.Para(linkLine l) ])
          if not comments.IsEmpty then
              Rich.Heading $"Comments ({totalComments})"

              if totalComments > comments.Length then
                  Rich.Para [ dim $"{totalComments - comments.Length} older comments not loaded; o opens the page." ]

              for c in comments do
                  Rich.Sub
                      [ boldColored Palette.lavender (author c)
                        seg "  "
                        yield! dated (str "createdAt" c |> Option.bind parseDate) |> List.map (fun s -> { s with Style = Dim }) ]

                  Rich.Item(seg "  ", Markdown.toBlocks (strOr "" "body" c)) ]

    let markdown =
        let md = ResizeArray<string>()
        let heading = if kind = DraftKind then header.Title else $"{key} {header.Title}"
        md.Add($"# {heading}" + (if url <> "" then $"\n\n{url}" else ""))

        let table =
            [ "Type", kindLabel kind ] @ (grid |> Seq.map (fun (k, v) -> k, segmentsText v) |> List.ofSeq)

        md.Add("| Field | Value |\n|---|---|\n" + String.Join("\n", table |> List.map (fun (k, v) -> $"| {cellText k} | {cellText v} |")))
        md.Add("## Description\n\n" + (if body.Trim() = "" then "*No description.*" else body.Trim()))

        if not links.IsEmpty then
            md.Add("## Links\n\n" + String.Join("\n", links |> List.map (fun l -> $"- {l.Relation} {l.Key} {l.Title}")))

        if not comments.IsEmpty then
            md.Add(
                "## Comments\n\n"
                + String.Join(
                    "\n\n",
                    comments
                    |> List.map (fun c ->
                        let created = formatDate (str "createdAt" c |> Option.bind parseDate)
                        let text = (strOr "" "body" c).Trim()
                        $"### {author c} · {created}\n\n{text}")
                )
            )

        String.Join("\n\n", md)

    let texts = body :: (comments |> List.map (strOr "" "body"))
    let known = links |> List.map _.Key |> Set.ofList |> Set.add key

    { Header = header
      Blocks = blocks
      Markdown = markdown
      Links = links
      Mentions = texts |> List.collect (Markdown.references repo) |> List.distinct |> List.filter (known.Contains >> not)
      Urls = texts |> List.collect Markdown.urls |> List.distinct }

/// Rows for each swimlane of the board view: every lane keeps a few rows, then the focused lane
/// grows to what it wants, then the others in order.
let laneHeights (available: int) (wants: int list) (focused: int) =
    let floor = if wants.Length * 5 <= available then 5 else 3
    let wants = List.toArray wants
    let heights = wants |> Array.map (min floor)
    let mutable left = available - Array.sum heights

    for i in focused :: ([ 0 .. wants.Length - 1 ] |> List.filter ((<>) focused)) do
        if i < wants.Length then
            let extra = max 0 (min left (wants.[i] - heights.[i]))
            heights.[i] <- heights.[i] + extra
            left <- left - extra

    List.ofArray heights
