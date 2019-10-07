with import <nixpkgs> {};

mkShell {
  name = "dev-mqttwebhook";
  builtInputs = [ nodejs-11_x ];
}
