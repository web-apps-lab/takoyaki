{
  description = "takoyaki";
  nixConfig.bash-prompt = "[nix(takoyaki)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/4a25962c7beede6cfcacc66000ef783e5c98e483";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, hspkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        packageName = "takoyaki";
        pkgs = hspkgs.pkgs;
        haskellPackages = pkgs.hspkgs;
        myPackage = haskellPackages.callCabal2nix packageName self { };
        takoyakiExe = pkgs.haskell.lib.justStaticExecutables myPackage;

        containerHome = "var/lib/${packageName}";
        mkContainerHome = "mkdir -p -m 744 ${containerHome}";

        takoyakiContainer = pkgs.dockerTools.buildLayeredImage {
          name = "localhost/takoyaki";
          contents = [ pkgs.coreutils pkgs.bash takoyakiExe ];
          extraCommands = "${mkContainerHome}";
          tag = "latest";
          created = "now";
          config = {
            Env = [
              "HOME=/${containerHome}"
              # Use fakeroot to avoid `No user exists for uid` error
              "LD_PRELOAD=${pkgs.fakeroot}/lib/libfakeroot.so"
            ];
          };
        };

      in {
        defaultExe = takoyakiExe;
        defaultPackage = myPackage;

        packages.container = takoyakiContainer;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = [
            pkgs.ghcid
            pkgs.ormolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
