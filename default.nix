with import <nixpkgs> {};

let
  rust = (rustChannels.nightly.rust.override {
    targets = [
      "x86_64-unknown-linux-musl"
    ];
  });
in pkgs.mkShell {
  name = "env-mqtt-webhook";
  buildInputs = [
    rust
  ];

  PKG_CONFIG_ALLOW_CROSS=true;
  PKG_CONFIG_ALL_STATIC=true;
  LIBZ_SYS_STATIC=1;

  OPENSSL_STATIC=1;
  OPENSSL_DIR = pkgsStatic.openssl.dev;
  OPENSSL_LIB_DIR = "${pkgsStatic.openssl.out}/lib";
}
