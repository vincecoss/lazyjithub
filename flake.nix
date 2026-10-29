{
  description = "lazyjithub: a read-only, lazygit-style terminal UI for GitHub Projects, in F# with Terminal.Gui";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        dotnet-sdk = pkgs.dotnetCorePackages.sdk_10_0;
        dotnet-runtime = pkgs.dotnetCorePackages.runtime_10_0;

        lazyjithub = pkgs.buildDotnetModule {
          pname = "lazyjithub";
          version = "0.1.0";
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let base = baseNameOf path;
              in !(builtins.elem base [ "bin" "obj" "result" ".direnv" ]);
          };
          projectFile = "src/LazyJithub/LazyJithub.fsproj";
          testProjectFile = "tests/LazyJithub.Tests/LazyJithub.Tests.fsproj";
          nugetDeps = ./deps.json;
          inherit dotnet-sdk dotnet-runtime;
          doCheck = true;
          executables = [ "lazyjithub" ];
          meta.mainProgram = "lazyjithub";
        };
      in
      {
        packages.default = lazyjithub;

        apps.default = {
          type = "app";
          program = "${lazyjithub}/bin/lazyjithub";
        };

        devShells.default = pkgs.mkShell {
          packages = [ dotnet-sdk pkgs.fsautocomplete pkgs.fantomas ];
          shellHook = ''
            export DOTNET_ROOT="${dotnet-sdk}/share/dotnet"
            export DOTNET_CLI_TELEMETRY_OPTOUT=1
            export DOTNET_NOLOGO=1
          '';
        };
      });
}
