let

  haskellNixSrc = (import <nixpkgs> {}).fetchFromGitHub {
    repo = "haskell.nix";
    owner = "input-output-hk";
    rev = "c7c7d6c43af27a632f16e631202eb83ac3c047c3"; # master 11082020
    sha256 = "0xrfl0zwf98cyv6px0awblhff97vjv19a5mdvs6l98769qgh4558";
  };

  haskellNix = import haskellNixSrc {};

  all-hies = fetchTarball "https://github.com/infinisil/all-hies/archive/master.tar.gz";

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    crossSystem = haskellNix.pkgs.lib.systems.examples.musl64;
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (import all-hies {}).overlay
      (import ./nix/custom-overlay.nix)
    ];
  });

in pkgs.haskell-nix.cabalProject {
  name = "mqtt-webhook";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mqtt-webhook";
    src = ./.;
  };
  # ghc = pkgs.haskell-nix.compiler.ghc865;
  compiler-nix-name = "ghc865";
  configureFlags =
    pkgs.lib.optionals pkgs.hostPlatform.isMusl [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
    ];
  modules = [
    {
      # Make Cabal reinstallable
      nonReinstallablePkgs = [
        "haddock"
        "rts"
        "ghc-heap"
        "ghc-prim"
        "integer-gmp"
        "integer-simple"
        "base"
        "deepseq"
        "array"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        "ghcjs-prim"
        "ghcjs-th"
        "ghc-boot"
        "ghc"
        "Win32"
        "array"
        "binary"
        "bytestring"
        "containers"
        "directory"
        "filepath"
        "ghc-boot"
        "ghc-compact"
        "ghc-prim"
        "hpc"
        "mtl"
        "parsec"
        "process"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"
        "terminfo"
      ];
    }];
}
