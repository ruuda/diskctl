{
  description = "Disckctl";

  inputs.nixpkgs.url = "nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      ghc = pkgs.ghc.withPackages (ps: [
        ps.blaze
        ps.blaze-html
        ps.optparse-applicative
        ps.text
        ps.text-format
        ps.time
        ps.tomland
        ps.unordered-containers
        ps.uuid
        ps.warp
      ]);
    in
      {
        packages."${system}".default = pkgs.stdenv.mkDerivation rec {
          name = "diskctl";
          pname = name;
          src = ./src;
          buildPhase = ''
            ${ghc}/bin/ghc -o diskctl -outputdir . $src/*.hs
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp diskctl $out/bin
          '';
        };

        devShells."${system}".default = pkgs.mkShell {
          name = "diskctl";
          nativeBuildInputs = [ghc];
          # Needed for the locale-archive, to avoid LC_* errors.
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        };
      };
}
