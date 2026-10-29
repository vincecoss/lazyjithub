module LazyJithub.Config

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Tomlyn
open Tomlyn.Model

exception ConfigError of string

let private xdg (variable: string) (fallback: string list) =
    match Environment.GetEnvironmentVariable variable with
    | null
    | "" -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile :: fallback |> List.toArray)
    | dir -> dir

let configHome () = xdg "XDG_CONFIG_HOME" [ ".config" ]
let stateHome () = xdg "XDG_STATE_HOME" [ ".local"; "state" ]

let configPath () = Path.Combine(configHome (), "lazyjithub", "config.toml")

let statePath () = Path.Combine(stateHome (), "lazyjithub", "state.json")

let defaultUrl = "https://api.github.com"

let sampleConfig =
    """# ~/.config/lazyjithub/config.toml
# Without token or token_cmd, lazyjithub reads GITHUB_TOKEN or GH_TOKEN, then runs `gh auth token`.
# The token needs the read:project scope: gh auth refresh -s read:project
# token_cmd = "gh auth token"            # or: token = "ghp_..."

# Optional
# url = "https://api.github.com"         # GitHub Enterprise Server: https://ghe.example.com/api
# max_items = 1000                       # cap per project or search load
# done_days = 14                         # on the Board tab, hide items closed longer ago than this
# refresh_seconds = 120                  # reload the open project in the background; 0 turns it off

# Status colors by name, any case; without one, the color set in the project is used:
# [status_colors]
# "in review" = "#ffab00"

# Several accounts or servers:
# default_profile = "work"
# [profiles.work]
# url = "https://ghe.example.com/api"
# token_cmd = "gh auth token --hostname ghe.example.com"
"""

type Config =
    { Url: string
      Token: string
      MaxItems: int
      DoneDays: int
      RefreshSeconds: int
      Profile: string option
      StatusColors: Map<string, string>
      Source: string }

    /// The web host that matches the API: github.com for api.github.com, the server itself otherwise.
    member this.WebUrl =
        let uri = Uri this.Url

        if uri.Host = "api.github.com" then "https://github.com"
        else $"{uri.Scheme}://{uri.Authority}"

    member this.Site = Uri(this.WebUrl).Authority

let private isHexColor (value: string) =
    value.Length = 7
    && value.[0] = '#'
    && value |> Seq.skip 1 |> Seq.forall Uri.IsHexDigit

let private table (value: obj) =
    match value with
    | :? TomlTable as t -> t
    | _ -> TomlTable()

let private get (t: TomlTable) (key: string) : obj option =
    match t.TryGetValue key with
    | true, v when not (isNull v) -> Some v
    | _ -> None

let private getString t key =
    get t key |> Option.map string |> Option.filter (String.IsNullOrWhiteSpace >> not)

let private getInt (t: TomlTable) key fallback =
    match get t key with
    | Some(:? int64 as n) -> int n
    | Some(:? int as n) -> n
    | Some other ->
        match Int32.TryParse(string other) with
        | true, n -> n
        | _ -> raise (ConfigError $"{key} must be a number, got {other}")
    | None -> fallback

let tokenFromCommand (command: string) : string =
    let psi = ProcessStartInfo("/bin/sh", [| "-c"; command |])
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    use proc =
        try
            Process.Start psi
        with e ->
            raise (ConfigError $"cannot run '{command}': {e.Message}")

    let out = proc.StandardOutput.ReadToEnd()
    let err = proc.StandardError.ReadToEnd()
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        raise (ConfigError $"'{command}' failed ({proc.ExitCode}): {err.Trim()}")

    out.Trim()

let parseToml (text: string) : TomlTable =
    try
        TomlSerializer.Deserialize<TomlTable>(text, (null: TomlSerializerOptions))
    with :? TomlException as e ->
        raise (ConfigError $"invalid TOML: {e.Message}")

/// `gh` keeps one token per host; github.com is its default.
let ghCommand (url: string) =
    match Uri(url).Host with
    | "api.github.com" -> "gh auth token"
    | host -> $"gh auth token --hostname {host}"

/// `env` and `runCommand` are parameters so tests can load a config without the process environment.
let loadFrom
    (env: string -> string option)
    (runCommand: string -> string)
    (path: string)
    (text: string option)
    (profile: string option)
    : Config =
    let root = text |> Option.map parseToml |> Option.defaultValue (TomlTable())
    let profiles = get root "profiles" |> Option.map table |> Option.defaultValue (TomlTable())

    let name =
        profile
        |> Option.orElse (env "LAZYJITHUB_PROFILE")
        |> Option.orElse (getString root "default_profile")

    let data =
        match name with
        | None -> root
        | Some n ->
            match get profiles n with
            | Some p ->
                let merged = TomlTable()

                for KeyValue(k, v) in root do
                    if k <> "profiles" then merged.[k] <- v

                for KeyValue(k, v) in table p do
                    merged.[k] <- v

                merged
            | None -> raise (ConfigError $"profile '{n}' not found in {path}")

    let useEnv = name.IsNone

    let url =
        getString data "url" |> Option.defaultValue defaultUrl |> _.TrimEnd('/')

    match Uri.TryCreate(url, UriKind.Absolute) with
    | true, uri when uri.Scheme = "https" || uri.Scheme = "http" -> ()
    | _ -> raise (ConfigError $"url must be an http(s) address, got '{url}'")

    let missing (reason: string) =
        ConfigError
            $"no GitHub token ({reason}): set GITHUB_TOKEN, log in with `gh auth login`, or set token or token_cmd in {path}"

    let token =
        (if useEnv then env "GITHUB_TOKEN" |> Option.orElse (env "GH_TOKEN") else None)
        |> Option.orElse (getString data "token")
        |> Option.orElseWith (fun () -> getString data "token_cmd" |> Option.map runCommand)
        |> Option.orElseWith (fun () ->
            try
                Some(runCommand (ghCommand url))
            with ConfigError message ->
                raise (missing message))
        |> Option.filter (String.IsNullOrWhiteSpace >> not)

    match token with
    | None -> raise (missing "empty token")
    | Some token ->
        let colors =
            match get data "status_colors" with
            | Some t ->
                (Map.empty, table t)
                ||> Seq.fold (fun acc (KeyValue(k, v)) ->
                    let color = string v

                    if not (isHexColor color) then
                        raise (ConfigError $"status_colors: '{k}' has an invalid color '{color}', use #rrggbb")

                    acc |> Map.add (k.ToLowerInvariant()) color)
            | None -> Map.empty

        { Url = url
          Token = token
          MaxItems = getInt data "max_items" 1000
          DoneDays = getInt data "done_days" 14
          RefreshSeconds = getInt data "refresh_seconds" 120
          Profile = name
          StatusColors = colors
          Source = path }

let load (profile: string option) : Config =
    let env (name: string) =
        match Environment.GetEnvironmentVariable name with
        | null
        | "" -> None
        | v -> Some v

    let path = configPath ()
    let text = if File.Exists path then Some(File.ReadAllText path) else None
    loadFrom env tokenFromCommand path text profile

type State(site: string, path: string) =
    let all =
        try
            match JsonNode.Parse(File.ReadAllText path) with
            | :? JsonObject as o -> o
            | _ -> JsonObject()
        with _ ->
            JsonObject()

    let siteNode () =
        match all.[site] with
        | :? JsonObject as o -> o
        | _ ->
            let o = JsonObject()
            all.[site] <- o
            o

    let save () =
        try
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, all.ToJsonString(JsonSerializerOptions(WriteIndented = true)))
        with _ ->
            ()

    new(site) = State(site, statePath ())

    member _.Path = path

    member _.Get<'T>(key: string) : 'T option =
        match siteNode().[key] with
        | null -> None
        | node ->
            try
                Some(node.Deserialize<'T>())
            with _ ->
                None

    member _.Set<'T>(key: string, value: 'T) =
        siteNode().[key] <- JsonSerializer.SerializeToNode(value)
        save ()

    member _.Remove(key: string) =
        siteNode().Remove(key) |> ignore
        save ()
