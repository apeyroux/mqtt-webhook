with import <nixpkgs> {};

let
  version = "1.0";
  requirements = with python3Packages; [
      flask
      click
    ];
in rec {
  mqtt-webhook = python3Packages.buildPythonPackage {
    propagatedBuildInputs = requirements;
    name = "mqtt-webhook-${version}";
    src = ./.;
  };
  docker = dockerTools.buildImage {
    name = "mqtt-webhook-${version}";
    tag = "latest";
    created = "now";
    contents = [ mqtt-webhook ];
    config = {
      EntryPoint = ["mqtt-webhook"];
      Cmd = ["mqtt-webhook"];
      Ports = ["5000"];
    };
  };
}
