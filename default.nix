{ mkDerivation, aeson, base, bytestring, ekg, ekg-core, mtl
, optparse-applicative, servant, servant-client, servant-ekg
, servant-server, stdenv, text, wai, wai-logger, warp
}:
mkDerivation {
  pname = "mqtt-webhook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring ekg ekg-core mtl optparse-applicative servant
    servant-client servant-ekg servant-server text wai wai-logger warp
  ];
  homepage = "https://github.com/githubuser/mqtt-webhook#readme";
  license = stdenv.lib.licenses.bsd3;
}
