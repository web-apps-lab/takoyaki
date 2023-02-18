# {
#   description = "takoyaki";
#   nixConfig.bash-prompt = "[nix(takoyaki)] ";

#   inputs = {
#     hspkgs.url =
#       "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
#     flake-utils.url = "github:numtide/flake-utils";
#   };

#   outputs = { self, hspkgs, flake-utils }:
#     flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
#       let
#         pkgs = hspkgs.pkgs;

#         packageName = "takoyaki";

#         haskellExtend = hpFinal: hpPrev: {
#           takoyaki = hpPrev.callCabal2nix packageName self { };
#           ki = let
#             src = pkgs.fetchFromGitHub {
#               owner = "awkward-squad";
#               repo = "ki";
#               rev = "f439a7dda99c2f71fb1d288cccb7a19ac436ce6d";
#               sha256 = "sha256-HTS+0hAExe3wrFgqHx35mMyTZ823AhbXHLlxATx5ExM=";
#             };
#           in hpPrev.callCabal2nix "ki" "${src}/ki" { };
#           # butler = hpPrev.callCabal2nix "butler" (pkgs.fetchFromGitHub {
#           #   owner = "TristanCacqueray";
#           #   repo = "haskell-butler";
#           #   rev = "24205b72a19cb0e760f554785e42d4b70ec33919";
#           #   sha256 = "sha256-9Tow26utD+So6tbWdaq/HLdj6FaNz/BYXsaZlIyKmsU=";
#           # }) { };
#         };

#         hsPkgs = pkgs.hspkgs.extend haskellExtend;

#         takoyakiExe = pkgs.haskell.lib.justStaticExecutables hsPkgs.takoyaki;

#       in {
#         defaultExe = takoyakiExe;
#         defaultPackage = hsPkgs.takoyaki;

#         devShell = hsPkgs.shellFor {
#           packages = p: [ p.takoyaki p.ki ];

#           buildInputs = with pkgs; [
#             ghcid
#             ormolu
#             cabal-install
#             hlint
#             haskell-language-server
#           ];
#         };
#       });
# }

{
  nixConfig.bash-prompt = "[nix(HazardHunter)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    # "path:///srv/github.com/podenv/hspkgs";
    butler.url =
      "github:TristanCacqueray/haskell-butler/f955a3a59da27633f273f00c2dd26a519a749049";
  };
  outputs = { self, hspkgs, butler }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        takoyaki = hpPrev.callCabal2nix "takoyaki" self { };
        butlerPackage = hpPrev.callCabal2nix "butler" butler { };
        ebml = hpPrev.callCabal2nix "ebml" (pkgs.fetchFromGitHub {
          owner = "TristanCacqueray";
          repo = "haskell-ebml";
          rev = "aff25512b52e48e92d77cd59019a0291a8b43bf4";
          sha256 = "sha256-U2Mo83gr7dLm+rRKOLzS9LZUaZ90ECO6Zjbv6maflyc=";
        }) { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      pkg-exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.takoyaki;

      baseTools = with pkgs; [
        hpack
        cabal-install
        hlint
        tasty-discover
        fourmolu
        hsPkgs.doctest
      ];

    in {
      packages."x86_64-linux".default = pkg-exe;

      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.takoyaki p.butlerPackage ];
        buildInputs = with pkgs;
          [ ghcid haskell-language-server pkgs.gst_all_1.gstreamer ]
          ++ baseTools;
      };
    };
}
