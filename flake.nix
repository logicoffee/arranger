{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: 
      let
        pkgs = import nixpkgs { inherit system; };
        ghc = (pkgs.haskell.packages.ghc96.ghcWithPackages (p: with p; [
          aeson
          containers
          cryptohash-sha256
          haskell-language-server
          http-client
          http-client-tls
          megaparsec
          servant
          servant-client
          servant-client-core
          servant-server
          text
          wai-logger
        ]));
      in
        {
          devShell = pkgs.mkShell {
            packages = [ ghc ];
          };
          packages.default = pkgs.stdenv.mkDerivation {
            name = "arranger";
            buildInputs = [ ghc ];
            src = ./.;
            buildPhase = ''
              ghc -isrc src/Main.hs -o arranger
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp arranger $out/bin
            '';
          };
        }
    );
}
