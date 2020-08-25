let

  haskellNixSrc = (import <nixpkgs> {}).fetchFromGitHub {
    repo = "haskell.nix";
    owner = "input-output-hk";
    rev = "c7c7d6c43af27a632f16e631202eb83ac3c047c3"; # master 11082020
    sha256 = "0xrfl0zwf98cyv6px0awblhff97vjv19a5mdvs6l98769qgh4558";
  };

  haskellNix = import haskellNixSrc {};

  pkgsMusl64 = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    crossSystem = haskellNix.pkgs.lib.systems.examples.musl64;
  });

in {

  musl64 = pkgsMusl64.haskell-nix.project {
    src = pkgsMusl64.haskell-nix.haskellLib.cleanGit {
      name = "mqtt-webhook";
      src = ../.;
    };
    compiler-nix-name = "ghc865";
    configureFlags =
      pkgsMusl64.lib.optionals pkgsMusl64.hostPlatform.isMusl [
        "--disable-executable-dynamic"
        "--disable-shared"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-L${pkgsMusl64.gmp6.override { withStatic = true; }}/lib"
        "--ghc-option=-optl=-L${pkgsMusl64.zlib.static}/lib"
      ];
  };

}
