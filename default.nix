with import <nixpkgs> {};

{
  static = buildGoPackage {
    name = "mqtt-webhook";
    goPackagePath = "github.com/apeyroux/mqtt-webhook";
    CGO_ENABLED=0;
    src = ./.;
  };
}
