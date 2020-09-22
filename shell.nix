let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc883";
  hnix = import sources.iohk-hnix {};
  pkgs = (import hnix.sources.nixpkgs) hnix.nixpkgsArgs;
  hls = import sources.all-hls { inherit pkgs; version = "0.4.0"; ghc = "8.8.3"; }; # TODO: generate this string from the ghc template variable
in
(import ./.).shellFor {
  withHoogle = true;
  buildInputs = [
    hls
    (pkgs.haskell-nix.tool compilerVersion "hpack"         { index-state = "2020-07-01T00:00:00Z"; version = "0.34.2"; })
    (pkgs.haskell-nix.tool compilerVersion "cabal-install" { index-state = "2020-07-01T00:00:00Z"; version = "3.2.0.0"; })
    (pkgs.haskell-nix.tool compilerVersion "ghcid"         { index-state = "2020-07-01T00:00:00Z"; version = "0.8.7";  })
  ];
}
