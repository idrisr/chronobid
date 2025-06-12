{ mkDerivation, aeson, base, bytestring, cassava, directory
, filepath, fmt, lib, servant, servant-docs, servant-server, stm
, tasty, tasty-hunit, text, time, uuid, uuid-types, vector, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "chronobid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cassava fmt servant servant-docs
    servant-server stm time uuid uuid-types vector
  ];
  executableHaskellDepends = [
    aeson base bytestring cassava directory fmt servant servant-docs
    servant-server stm text time uuid uuid-types vector wai wai-extra
    warp
  ];
  testHaskellDepends = [
    base directory filepath fmt tasty tasty-hunit time uuid vector
  ];
  license = lib.licenses.mit;
}
