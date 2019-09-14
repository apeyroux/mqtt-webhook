with import <nixpkgs> {};

pkgs.mkShell {
    name = "dev-mqtt-webhook";
    buildInputs = [ pkgconfig zlib openssl ];
}
