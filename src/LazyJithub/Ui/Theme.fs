module LazyJithub.Ui.Theme

open System
open Terminal.Gui.Drawing
open Terminal.Gui.ViewBase
open LazyJithub.Text

let color (hex: string) = Color(hex)

let attr (fg: Color) (bg: Color) (style: TextStyle) = Attribute(&fg, &bg, &style)

let borderFocused = color Palette.green
let borderBlurred = color "#5c6370"
let rowFocused = color "#2b4a6f"
let rowBlurred = color "#353a42"
let rowMarked = color "#24502f"
let rowMarkedFocused = color "#357a50"
let rowFlash = color "#b8860b"
let dimText = color Palette.dimGrey

let normalOf (v: View) = v.GetAttributeForRole VisualRole.Normal

let segmentAttr (normal: Attribute) (bg: Color) (s: Segment) =
    let fg =
        match s.Color, s.Style with
        | Some c, _ -> color c
        | None, Dim -> dimText
        | None, _ -> normal.Foreground

    let style =
        match s.Style with
        | Bold -> TextStyle.Bold
        | Italic -> TextStyle.Italic
        | Underline -> TextStyle.Underline
        | BoldUnderline -> TextStyle.Bold ||| TextStyle.Underline
        | Strike -> TextStyle.Strikethrough
        | Plain
        | Dim -> TextStyle.None

    attr fg (s.Background |> Option.map color |> Option.defaultValue bg) style

/// Draws segments from (x, y), cut to `width` columns with an ellipsis; returns the columns used.
let drawSegments (v: View) (x: int) (y: int) (width: int) (bg: Color) (segments: Segment list) =
    let normal = normalOf v
    let mutable col = 0

    for s in segments do
        let room = width - col

        if room > 0 && s.Text <> "" then
            let text =
                if s.Text.Length <= room then s.Text
                elif room = 1 then "…"
                else s.Text.Substring(0, room - 1) + "…"

            v.Move(x + col, y) |> ignore
            v.SetAttribute(segmentAttr normal bg s) |> ignore
            v.AddStr text
            col <- col + text.Length

    v.SetAttribute normal |> ignore
    col

let fill (v: View) (x: int) (y: int) (width: int) (bg: Color) =
    if width > 0 then
        let normal = normalOf v
        v.Move(x, y) |> ignore
        v.SetAttribute(attr normal.Foreground bg TextStyle.None) |> ignore
        v.AddStr(String(' ', width))
        v.SetAttribute normal |> ignore

let drawFrame (v: View) (focused: bool) (title: Segment list) (footer: Segment list) =
    let w = v.Viewport.Width
    let h = v.Viewport.Height

    if w >= 2 && h >= 2 then
        let normal = normalOf v
        let border = attr (if focused then borderFocused else borderBlurred) normal.Background TextStyle.None
        v.SetAttribute border |> ignore
        v.Move(0, 0) |> ignore
        v.AddStr("╭" + String('─', w - 2) + "╮")

        for y in 1 .. h - 2 do
            v.Move(0, y) |> ignore
            v.AddStr "│"
            v.Move(w - 1, y) |> ignore
            v.AddStr "│"

        v.Move(0, h - 1) |> ignore
        v.AddStr("╰" + String('─', w - 2) + "╯")
        drawSegments v 1 0 (w - 2) normal.Background title |> ignore

        let footerWidth = segmentsText footer |> _.Length

        if footerWidth > 0 && footerWidth < w - 4 then
            drawSegments v (w - 2 - footerWidth) (h - 1) footerWidth normal.Background footer |> ignore

        v.SetAttribute normal |> ignore
