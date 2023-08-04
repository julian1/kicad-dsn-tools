{ mkDerivation, attoparsec, base, containers, mtl, raw-strings-qq
, stdenv, text
}:
mkDerivation {
  pname = "main";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers mtl raw-strings-qq text
  ];
  executableHaskellDepends = [
    attoparsec base containers raw-strings-qq text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
