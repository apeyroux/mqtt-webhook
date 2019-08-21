with import <nixpkgs> {};

let
  version = (builtins.fetchGit { url = ./.; ref = "HEAD"; }).shortRev;
  srvProd = "10.227.193.18";
  pypi2nix = (import ./nix/requirements.nix {}).packages;
  vmDebian = pkgs.vmTools.diskImageFuns.debian8x86_64 {};
in rec {
  deploy = writeScriptBin "deploy-mqttwebhook-${version}" ''
echo "=== DEPLOY IMG ==="
cat ${docker} | ssh ${srvProd} docker load
  '';
  mqtt-webhook = python3Packages.buildPythonPackage {
    propagatedBuildInputs = builtins.attrValues pypi2nix;
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
