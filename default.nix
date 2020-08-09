let
  haskellNixSrc = fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/tarball/af5998fe8d6b201d2a9be09993f1b9fae74e0082";
    sha256 = "0z5w99wkkpg2disvwjnsyp45w0bhdkrhvnrpz5nbwhhp21c71mbn";
  };

  haskellNix = import haskellNixSrc {};

  all-hies = fetchTarball "https://github.com/infinisil/all-hies/archive/master.tar.gz";

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    # crossSystem = (import <nixpkgs/lib>).systems.examples.musl64;
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (import all-hies {}).overlay
    ];
  });

  set = pkgs.haskell-nix.cabalProject' {
    name = "mqtt-webhook";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "mqtt-webhook";
      src = ./.;
    };
    ghc = pkgs.haskell-nix.compiler.ghc865;
    # configureFlags =
    #   pkgs.lib.optionals pkgs.hostPlatform.isMusl [
    #     "--disable-executable-dynamic"
    #     "--disable-shared"
    #     "--ghc-option=-optl=-pthread"
    #     "--ghc-option=-optl=-static"
    #     "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
    #     "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
    #   ];
    modules = [
      {
      # Make Cabal reinstallable
      nonReinstallablePkgs = [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base" "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell" "ghcjs-prim" "ghcjs-th" "ghc-boot" "ghc" "Win32" "array" "binary" "bytestring" "containers" "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim" "hpc" "mtl" "parsec" "process" "text" "time" "transformers" "unix" "xhtml" "terminfo" ];
    }];
  };
in set.hsPkgs.mqtt-webhook.components.exes.mqtt-webhook // {
  env = set.hsPkgs.shellFor {
    packages = p: [ p.mqtt-webhook ];
    exactDeps = true;
    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
    };
    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';
  };
}
