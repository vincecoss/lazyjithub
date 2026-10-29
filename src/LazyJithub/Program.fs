module LazyJithub.Program

open System
open System.Text.RegularExpressions
open LazyJithub.Config
open LazyJithub.GitHub
open LazyJithub.Ui.App

let usage =
    """lazyjithub: a lazygit-style terminal UI for GitHub Projects.

Usage: lazyjithub [options]

Options:
  --project OWNER/NUMBER  open this project, as in github.com/orgs/OWNER/projects/NUMBER
  --search QUERY          open the result of a GitHub issue search
  --profile NAME          use a profile from the [profiles] table of the config file
  --check                 check the configuration and the connection, then exit
  -h, --help              show this help
  --version               show the version

Without --project or --search, lazyjithub reopens what you had open last."""

type Options =
    { Start: Start
      Profile: string option
      Check: bool }

let private projectRef =
    Regex(@"^(?:https?://[^/]+/(?:orgs|users)/)?([\w.-]+)(?:/projects)?/(\d+)/?$", RegexOptions.Compiled)

let rec parse (args: string list) (options: Options) : Result<Options, string> =
    match args with
    | [] -> Ok options
    | "--project" :: value :: rest ->
        let m = projectRef.Match value

        if m.Success then
            parse rest { options with Start = StartProject(m.Groups.[1].Value, int m.Groups.[2].Value) }
        else
            Error $"--project expects OWNER/NUMBER or a project URL, got '{value}'"
    | "--search" :: value :: rest -> parse rest { options with Start = StartSearch value }
    | "--profile" :: value :: rest -> parse rest { options with Profile = Some value }
    | "--check" :: rest -> parse rest { options with Check = true }
    | [ flag ] when List.contains flag [ "--project"; "--search"; "--profile" ] -> Error $"{flag} expects a value"
    | other :: _ -> Error $"unknown option '{other}'"

let private check (client: GitHubClient) (config: Config) =
    try
        let login, projects, problems = client.Projects().GetAwaiter().GetResult()
        printfn $"ok: {config.Url} as {login}, {projects.Length} projects (config: {config.Source})"

        for p in problems do
            eprintfn $"warning: {p}"

        0
    with e ->
        let message =
            match e with
            | GitHubError msg -> msg
            | e -> e.Message

        eprintfn $"lazyjithub: {message}"
        1

[<EntryPoint>]
let main argv =
    let args = Array.toList argv

    if List.contains "-h" args || List.contains "--help" args then
        printfn "%s" usage
        0
    elif List.contains "--version" args then
        printfn "lazyjithub 0.1.0"
        0
    else
        match parse args { Start = StartLast; Profile = None; Check = false } with
        | Error message ->
            eprintfn $"lazyjithub: {message}\n\n{usage}"
            2
        | Ok options ->
            match
                (try
                    Ok(load options.Profile)
                 with ConfigError message ->
                     Error message)
            with
            | Error message ->
                eprintfn $"lazyjithub: {message}\n\nExample {configPath ()}:\n\n{sampleConfig}"
                2
            | Ok config ->
                use client = new GitHubClient(config.Url, config.Token)

                if options.Check then
                    check client config
                else
                    run client config options.Start
                    0
