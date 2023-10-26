{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=23.05";
    flakeUtils.url = "github:numtide/flake-utils";

    realfolkNix = {
      url = "github:realfolk/nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flakeUtils.follows = "flakeUtils";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flakeUtils
    , realfolkNix
    , ...
    }:
    flakeUtils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      realfolkNixPkgs = realfolkNix.packages.${system};

      neovim = realfolkNixPkgs.neovim;
      ranger = realfolkNixPkgs.ranger;
      rnixLsp = realfolkNixPkgs.rnixLsp;

      realfolkNixLib = realfolkNix.lib.${system};

      haskellPkgs = realfolkNixLib.haskellPackages;

      ghc = haskellPkgs.ghc;

      haskellProject = realfolkNixLib.haskellProject;
    in
    {
      packages.default = haskellPkgs.callCabal2nix "pouch" self { };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          neovim
          pkgs.fzf
          pkgs.inotifyTools
          pkgs.openssl
          pkgs.silver-searcher
          ranger
          rnixLsp

          # Haskell
          ghc
          haskellPkgs.cabal-install
          haskellPkgs.haskell-language-server
          haskellPkgs.hspec-discover
        ];

        shellHook = pkgs.lib.concatStrings [
          (
            ''
              # Load ~/.bashrc if it exists
              test -f ~/.bashrc && source ~/.bashrc

              # Initialize $PROJECT environment variable
              export PROJECT="$PWD"

              # Source .env file if present
              test -f "$PROJECT/.env" && source .env

              # Ignore files specified in .gitignore when using fzf
              # -t only searches text files and includes empty files
              export FZF_DEFAULT_COMMAND="ag -tl"
            ''
          )
          (haskellProject.shellHook ghc)
        ];
      };
    });
}
