# https://hydra.nixos.org/build/86328601#tabs-buildinputs build docker ok
with import <nixpkgs> {};

let
  drv = haskellPackages.callCabal2nix "mqtt-webhook" ./. {};
in if lib.inNixShell then drv.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [ haskellPackages.ghcid cabal-install ];
  }) else drv
