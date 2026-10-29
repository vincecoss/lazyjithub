module LazyJithub.Help

open System
open System.Text.RegularExpressions
open LazyJithub.Text

let sections =
    [ "Panels",
      [ "1", "Projects, yours and your organizations'; and searches; tab switches between them"
        "2", "Statuses: the columns of the project, with counts"
        "3", "Repositories of the listed items, with counts"
        "4", "Group by assignee, iteration, milestone, parent, type, or a single-select field of the project"
        "5", "Items: board, current iteration or all for a project; results for a search"
        "0", "The issue, pull request or draft issue" ]
      "Move",
      [ "1-5, 0", "jump to a panel"
        "h / l, ← / →", "previous / next panel"
        "j / k, ↓ / ↑", "move in a list, scroll the item"
        "} / {", "in the item: next / previous section; ] and [ too"
        "ctrl+d / ctrl+u", "page down / up"
        "tab / shift+tab", "next / previous tab of a panel; ] and [ in lists too"
        "enter", "open a project or search, or move to the next panel"
        "J / K", "next / previous item from anywhere"
        "g", "go to an issue: owner/repo#12, repo#12, #12 or its URL"
        "L", "parent, sub-issues, linked pull requests, and issues and URLs mentioned in the text"
        "backspace", "back to the item before the last jump" ]
      "Filter",
      [ "space", "add or remove a status or repository from the selection"
        "esc", "clear the panel filter, or reset the panel to All"
        "/", "filter the focused list, or search the item"
        "n / N", "next / previous match in the item"
        "X", "clear all filters"
        "s", "sort by status, repository, rank, or last update"
        "m", "only items assigned to you"
        ":", "search GitHub issues and pull requests; ↑ / ↓ browse recent searches" ]
      "Show",
      [ "v", "board view: a block per group of panel 4, with the status columns inside; { } move between groups; / finds a card by key, title or assignee and enter goes to it; f turns the status and repository filters off or on"
        "S", "statistics of the listed items"
        "o", "open the item in the browser; the project when no item is shown"
        "y / Y / c", "copy the key / the URL / a Markdown link"
        "C", "copy the whole item as Markdown, comments included"
        "+", "layout: normal, wide item, full item"
        "r", "reload"
        "?", "this help"
        "q", "quit" ]
      "Markers",
      [ "○ ✓ ⊘", "open issue, closed issue, closed as not planned"
        "⇄", "pull request: green open, grey draft, purple merged, red closed"
        "◌", "draft issue of the project"
        "•", "the item changed since your last visit to this project"
        "3d", "days since the last update; yellow from 7, red from 14" ] ]

let private separators = Regex(@"( / |, )", RegexOptions.Compiled)

/// Keys in color, with the " / " and ", " between alternatives dimmed.
let keySegments (keys: string) : Segment list =
    separators.Split keys
    |> Array.filter ((<>) "")
    |> Array.map (fun part -> if separators.IsMatch part then dim part else boldColored Palette.blue part)
    |> Array.toList

let lines (width: int) : Segment list list =
    let keyWidth =
        sections |> List.collect snd |> List.map (fst >> String.length) |> List.max

    let actionWidth = max 10 (width - keyWidth - 2)
    let indent = String(' ', keyWidth + 2)

    let row (keys: string, action: string) =
        match wrapLines actionWidth action with
        | [] -> [ keySegments keys ]
        | first :: rest ->
            (keySegments keys @ [ seg (String(' ', keyWidth - keys.Length + 2)); seg first ])
            :: (rest |> List.map (fun line -> [ seg indent; seg line ]))

    [ for n, (title, rows) in List.indexed sections do
          if n > 0 then
              []
          [ boldColored Palette.green title ]
          yield! rows |> List.collect row
      []
      yield! wrapLines width "lazyjithub only reads GitHub: it refuses any request that could change it." |> List.map (dim >> List.singleton) ]
