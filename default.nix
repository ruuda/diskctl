let
  pkgs = import ./nixpkgs-pinned.nix {};

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.text
    p.time
    p.tomland
    p.unordered-containers
    p.uuid
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
