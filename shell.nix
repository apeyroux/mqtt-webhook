((import ./.).mqtt-webhook.components.exes.mqtt-webhook // {
  env = (import ./.).shellFor {
    packages = p: [ p.mqtt-webhook ];
    exactDeps = true;
    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
    };
    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';
  };
}).env
