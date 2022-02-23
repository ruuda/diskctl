let
  pkgs = import ./nixpkgs-pinned.nix {};

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.blaze
    p.blaze-html
    p.optparse-applicative
    p.text
    p.text-format
    p.time
    p.tomland
    p.unordered-containers
    p.uuid
    p.warp
  ]);
in
  pkgs.buildEnv {
    name = "diskctl-devenv";
    paths = [
      # Needed for the locale-archive, to avoid LC_* errors.
      pkgs.glibcLocales
      ghc
    ];
  }
