{
  description = "takoyaki";
  nixConfig.bash-prompt = "[nix(takoyaki)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, hspkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = hspkgs.pkgs;

        packageName = "takoyaki";

        haskellExtend = hpFinal: hpPrev: {
          takoyaki = hpPrev.callCabal2nix packageName self { };
        };

        hsPkgs = pkgs.hspkgs.extend haskellExtend;

        takoyakiExe = pkgs.haskell.lib.justStaticExecutables hsPkgs.takoyaki;

      in {
        defaultExe = takoyakiExe;
        defaultPackage = hsPkgs.takoyaki;

        devShell = hsPkgs.shellFor {
          packages = p: [ p.takoyaki ];

          buildInputs = with pkgs; [
            ghcid
            ormolu
            cabal-install
            hlint
            haskell-language-server
          ];
        };
      });
}
