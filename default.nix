# https://hydra.nixos.org/build/86328601#tabs-buildinputs build docker ok
with import <nixpkgs> {};

let
  drv = haskellPackages.callCabal2nix "mqtt-webhook" ./. {};
in {

  nix = if lib.inNixShell then drv.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [ haskellPackages.ghcid cabal-install ];
  }) else drv;

  fhs = (buildFHSUserEnv { name = "webhook"; targetPkgs = p: (with p; [ghc gcc cabal-install binutils.bintools zlib.dev]); }).env;

}
