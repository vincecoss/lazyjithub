module LazyJithub.Tests.MarkdownTests

open Xunit
open LazyJithub
open LazyJithub.Text

let private lines (width: int) (markdown: string) =
    Rich.render width (Markdown.toBlocks markdown) |> List.map Rich.lineText

[<Fact>]
let ``paragraphs keep their line breaks, as on GitHub`` () =
    Assert.Equal<string list>([ "first line"; "second line" ], lines 40 "first line\nsecond line")

[<Fact>]
let ``headings, lists and task lists`` () =
    let text = "## Steps\n\n- [x] done\n- [ ] todo\n  1. nested\n\n1. one\n2. two"
    let out = lines 40 text
    Assert.Equal<string list>([ "Steps"; "☑ done"; "☐ todo"; "  1. nested"; "1. one"; "2. two" ], out)

[<Fact>]
let ``code keeps its lines and inline code its color`` () =
    Assert.Equal<string list>([ "│ let x = 1"; "│ let y = 2" ], lines 40 "```fsharp\nlet x = 1\nlet y = 2\n```")

    match Markdown.toBlocks "run `make`" with
    | [ Rich.Para segments ] ->
        let code = segments |> List.find (fun s -> s.Text = "make")
        Assert.Equal(Some Palette.code, code.Color)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``HTML comments of issue templates are hidden`` () =
    let text = "<!-- Describe the bug -->\nThe app crashes.\n\n<!--\nmulti\n\nline\n-->\nEnd"
    Assert.Equal<string list>([ "The app crashes."; ""; "End" ], lines 40 text)

[<Fact>]
let ``details show their summary`` () =
    let out = lines 40 "<details><summary>Logs</summary>\n\nstack trace\n</details>"
    Assert.Equal<string list>([ "▸ Logs"; ""; "stack trace" ], out)

[<Fact>]
let ``alerts name their kind`` () =
    let out = lines 40 "> [!WARNING]\n> Breaks the API"
    Assert.Equal<string list>([ "▎ Warning"; "▎ "; "▎ Breaks the API" ], out)

[<Fact>]
let ``tables, images and rules`` () =
    let out = lines 40 "| a | b |\n|---|---|\n| 1 | 2 |\n\n![screenshot](https://x/y.png)\n\n---"
    Assert.Equal<string list>([ "┌───┬───┐"; "│ a │ b │"; "├───┼───┤"; "│ 1 │ 2 │"; "└───┴───┘"; ""; "▣ screenshot"; ""; String.replicate 40 "─" ], out)

[<Fact>]
let ``issue references and mentions stand out`` () =
    match Markdown.toBlocks "fixed by #12 in acme/api#3, thanks @bob" with
    | [ Rich.Para segments ] ->
        let bold = segments |> List.filter (fun s -> s.Style = Bold) |> List.map _.Text
        Assert.Equal<string list>([ "#12"; "acme/api#3"; "@bob" ], bold)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``an issue URL reads as its key`` () =
    Assert.Equal<string list>([ "see o/r#5" ], lines 40 "see https://github.com/o/r/issues/5")

[<Fact>]
let ``references are found outside code, with the repository for bare numbers`` () =
    let text = "Blocks #4 and o/r#5, see https://github.com/x/y/pull/6.\n\n`#7` is code\n\n```\n#8\n```"
    Assert.Equal<string list>([ "acme/api#4"; "o/r#5"; "x/y#6" ], Markdown.references "acme/api" text)
    Assert.Equal<string list>([], Markdown.references "acme/api" "color #ff0000 and C#")

[<Fact>]
let ``urls leave out images and issue links`` () =
    let text = "[docs](https://docs.example.com) ![img](https://x/i.png) https://github.com/o/r/issues/1 https://example.com/a"
    Assert.Equal<string list>([ "https://docs.example.com"; "https://example.com/a" ], Markdown.urls text)

[<Fact>]
let ``empty text gives nothing`` () =
    Assert.Empty(Markdown.toBlocks "")
    Assert.Empty(Markdown.toBlocks "  \n ")
    Assert.Empty(Markdown.toBlocks "<!-- only a comment -->")
