with import <nixpkgs> {};

let
  rust = (rustChannels.nightly.rust.override {
    targets = [
      # "x86_64-unknown-linux-musl"
      "x86_64-unknown-linux-gnu"
      # "arm-linux-androideabi"
    ];
  });
in pkgs.mkShell {
    name = "dev-mqtt-webhook";
    buildInputs = [
      musl
      openssl
      openssl.dev
      pkgconfig
      pkgsMusl.gcc
      pkgsMusl.openssl.dev
      rust
      zlib
  ];
}
