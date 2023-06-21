{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    neovim.url = "github:realfolk/nix?dir=lib/packages/neovim";
    ranger.url = "github:realfolk/nix?dir=lib/packages/ranger";
    rnixLsp.url = "github:nix-community/rnix-lsp";
    haskellPackages.url = "github:realfolk/nix?dir=lib/projects/haskell/packages/ghc-9.2";
    haskellProject.url = "github:realfolk/nix?dir=lib/projects/haskell";
  };

  outputs =
    { self
    , nixpkgs
    , flakeUtils
    , neovim
    , ranger
    , rnixLsp
    , haskellPackages
    , haskellProject
    , ...
    }:
    flakeUtils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPkgs = haskellPackages.packages.${system};
      ghc = haskellPkgs.ghc;
    in
    {
      packages.default = haskellPkgs.callCabal2nix "pouch" self { };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.silver-searcher # ag
          pkgs.fzf
          pkgs.openssl
          pkgs.inotifyTools
          ghc
          neovim.packages.${system}.default
          ranger.packages.${system}.default
          rnixLsp.defaultPackage.${system}
          haskellPkgs.haskell-language-server
          haskellPkgs.hspec-discover
          haskellPkgs.cabal-install
        ];
        shellHook = pkgs.lib.concatStrings [
          (
            ''
              # Load ~/.bashrc if it exists
              test -f ~/.bashrc && source ~/.bashrc

              # Source .env file if present
              test -f "$PROJECT/.env" && source .env

              # Ignore files specified in .gitignore when using fzf
              # -t only searches text files and includes empty files
              export FZF_DEFAULT_COMMAND="ag -tl"

              # Initialize $PROJECT environment variable
              export PROJECT="$PWD"
            ''
          )
          (haskellProject.lib.shellHook ghc)
        ];
      };
    });
}
