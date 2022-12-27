{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      compiler = "ghc924";
      overrides = hfinal: hprev: {
        cofree-bot = hfinal.callCabal2nix "cofree-bot" ./. { };
      };
    in
    {
      overlays.default = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ghc8107 = prev.haskell.packages.ghc8107.override { inherit overrides; };
            ghc904 = prev.haskell.packages.ghc904.override { inherit overrides; };
            ghc924 = prev.haskell.packages.ghc924.override { inherit overrides; };
          };
        };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        scripts = import ./nix/scripts.nix {
          s = pkgs.writeShellScriptBin;
          ormolu = pkgs.ormolu;
        };
      in
      {
        devShells = flake-utils.lib.flattenTree {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              # haskell development tools.
              cabal-install
              ghcid
              ormolu
              nixpkgs-fmt
              (haskell.packages.${compiler}.ghcWithPackages (hpkgs: with hpkgs; [
                haskell-language-server
              ]))
              # system-level dependencies.
              zlib.dev
            ] ++ (builtins.attrValues scripts);
          };
        };

        packages = flake-utils.lib.flattenTree {
          docker = import ./nix/docker.nix {
            inherit pkgs;
            cofree-bot = pkgs.haskell.packages.${compiler}.cofree-bot;
          };
          cofree-bot = pkgs.haskell.packages.${compiler}.cofree-bot;
        };

        defaultPackage = self.packages.cofree-bot;
      });
}
