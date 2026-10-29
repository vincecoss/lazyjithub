module LazyJithub.Json

open System
open System.Globalization
open System.Text.Json

let prop (name: string) (e: JsonElement) : JsonElement option =
    if e.ValueKind = JsonValueKind.Object then
        match e.TryGetProperty name with
        | true, v when v.ValueKind <> JsonValueKind.Null && v.ValueKind <> JsonValueKind.Undefined -> Some v
        | _ -> None
    else
        None

let path (names: string list) (e: JsonElement) : JsonElement option =
    names |> List.fold (fun acc name -> acc |> Option.bind (prop name)) (Some e)

let asString (e: JsonElement) : string option =
    match e.ValueKind with
    | JsonValueKind.String -> Some(e.GetString())
    | JsonValueKind.Number -> Some(e.GetRawText())
    | JsonValueKind.True -> Some "true"
    | JsonValueKind.False -> Some "false"
    | _ -> None

let str (name: string) (e: JsonElement) : string option = prop name e |> Option.bind asString

let strOr (fallback: string) (name: string) (e: JsonElement) = str name e |> Option.defaultValue fallback

let pathStr (names: string list) (e: JsonElement) = path names e |> Option.bind asString

let num (name: string) (e: JsonElement) : int option =
    match prop name e with
    | Some v when v.ValueKind = JsonValueKind.Number ->
        match v.TryGetInt32() with
        | true, n -> Some n
        | _ -> None
    | Some v when v.ValueKind = JsonValueKind.String ->
        match Int32.TryParse(v.GetString(), NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, n -> Some n
        | _ -> None
    | _ -> None

let flag (name: string) (e: JsonElement) : bool option =
    match prop name e with
    | Some v when v.ValueKind = JsonValueKind.True -> Some true
    | Some v when v.ValueKind = JsonValueKind.False -> Some false
    | _ -> None

let items (e: JsonElement) : JsonElement list =
    if e.ValueKind = JsonValueKind.Array then
        [ for item in e.EnumerateArray() -> item ]
    else
        []

let arr (name: string) (e: JsonElement) : JsonElement list =
    prop name e |> Option.map items |> Option.defaultValue []

let properties (e: JsonElement) : (string * JsonElement) list =
    if e.ValueKind = JsonValueKind.Object then
        [ for p in e.EnumerateObject() -> p.Name, p.Value ]
    else
        []

let parse (text: string) : JsonElement =
    use doc = JsonDocument.Parse(text)
    doc.RootElement.Clone()

let parseDate (value: string) : DateTimeOffset option =
    match DateTimeOffset.TryParse(value, CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) with
    | true, d -> Some d
    | _ -> None

/// The `nodes` of a GraphQL connection.
let nodes (name: string) (e: JsonElement) : JsonElement list =
    prop name e |> Option.map (arr "nodes") |> Option.defaultValue []
