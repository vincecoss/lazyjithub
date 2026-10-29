module LazyJithub.Tests.ConfigTests

open Xunit
open LazyJithub.Config

let private noEnv (_: string) : string option = None

let private env (pairs: (string * string) list) (name: string) =
    pairs |> List.tryFind (fst >> (=) name) |> Option.map snd

let private failCommand (command: string) : string = raise (ConfigError $"{command}: not found")

/// Answers with the command it was given, so a test sees which one ran.
let private echoCommand (command: string) = "from " + command

[<Fact>]
let ``the environment token wins and no command runs`` () =
    let config = loadFrom (env [ "GITHUB_TOKEN", "env-token" ]) failCommand "c.toml" (Some "token_cmd = \"pass show github\"") None
    Assert.Equal("env-token", config.Token)

[<Fact>]
let ``GH_TOKEN is read when GITHUB_TOKEN is not set`` () =
    let config = loadFrom (env [ "GH_TOKEN", "cli-token" ]) failCommand "c.toml" None None
    Assert.Equal("cli-token", config.Token)

[<Fact>]
let ``token_cmd runs before the GitHub CLI`` () =
    let config = loadFrom noEnv echoCommand "c.toml" (Some "token_cmd = \"pass show github\"") None
    Assert.Equal("from pass show github", config.Token)

[<Fact>]
let ``without a token, the GitHub CLI gives the one of the url host`` () =
    let config = loadFrom noEnv echoCommand "c.toml" None None
    Assert.Equal("from gh auth token", config.Token)

    let enterprise = loadFrom noEnv echoCommand "c.toml" (Some "url = \"https://ghe.example.com/api\"") None
    Assert.Equal("from gh auth token --hostname ghe.example.com", enterprise.Token)
    Assert.Equal("https://ghe.example.com", enterprise.WebUrl)

[<Fact>]
let ``when the GitHub CLI fails, the error says how to give a token`` () =
    let e = Assert.Throws<ConfigError>(fun () -> loadFrom noEnv failCommand "c.toml" None None |> ignore)
    Assert.Contains("GITHUB_TOKEN", e.Data0)
    Assert.Contains("not found", e.Data0)

[<Fact>]
let ``defaults`` () =
    let config = loadFrom (env [ "GITHUB_TOKEN", "t" ]) failCommand "c.toml" None None
    Assert.Equal("https://api.github.com", config.Url)
    Assert.Equal("https://github.com", config.WebUrl)
    Assert.Equal("github.com", config.Site)
    Assert.Equal(1000, config.MaxItems)
    Assert.Equal(14, config.DoneDays)
    Assert.Equal(120, config.RefreshSeconds)

[<Fact>]
let ``a profile ignores the environment and overrides the root`` () =
    let text =
        """
max_items = 50
token = "root"
[profiles.work]
url = "https://ghe.example.com/api/"
token = "work"
"""

    let config = loadFrom (env [ "GITHUB_TOKEN", "env" ]) failCommand "c.toml" (Some text) (Some "work")
    Assert.Equal("work", config.Token)
    Assert.Equal("https://ghe.example.com/api", config.Url)
    Assert.Equal(50, config.MaxItems)
    Assert.Equal(Some "work", config.Profile)

[<Fact>]
let ``an unknown profile, a bad color or a bad url is an error`` () =
    Assert.Throws<ConfigError>(fun () -> loadFrom noEnv failCommand "c.toml" (Some "token = \"t\"") (Some "nope") |> ignore)
    |> ignore

    Assert.Throws<ConfigError>(fun () ->
        loadFrom noEnv failCommand "c.toml" (Some "token = \"t\"\n[status_colors]\ntodo = \"red\"") None |> ignore)
    |> ignore

    Assert.Throws<ConfigError>(fun () ->
        loadFrom noEnv failCommand "c.toml" (Some "token = \"t\"\nurl = \"github.com\"") None |> ignore)
    |> ignore

[<Fact>]
let ``status colors are keyed in lower case`` () =
    let config =
        loadFrom noEnv failCommand "c.toml" (Some "token = \"t\"\n[status_colors]\n\"In Review\" = \"#ffab00\"") None

    Assert.Equal(Some "#ffab00", Map.tryFind "in review" config.StatusColors)
