{ mkDerivation, base, containers, exceptions, lens, template-haskell, text, bytestring
, lib, libGL, libX11, libXcursor, libXext, libXi, libXinerama, libXrandr, raylib, raygui, ...
}:
mkDerivation {
  pname = "h-raylib";
  version = "5.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  configureFlags = [
    "-f-detect-platform -fplatform-nixos"
  ];
  libraryHaskellDepends = [ base containers exceptions lens template-haskell text bytestring ];
  librarySystemDepends = [
    libGL
    libX11
    libXcursor
    libXext
    libXi
    libXinerama
    libXrandr
    raylib
    raygui
  ];
  description = "Raylib bindings for Haskell";
  license = lib.licenses.asl20;
}
