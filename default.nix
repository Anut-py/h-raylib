{ mkDerivation, base, containers, exceptions, lens, lib, libGL
, libX11, libXcursor, libXext, libXi, libXinerama, libXrandr, raylib, ...
}:
mkDerivation {
  pname = "h-raylib";
  version = "5.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  configureFlags = [
    "-fplatform-nixos"
  ];
  libraryHaskellDepends = [ base containers exceptions lens ];
  librarySystemDepends = [
    libGL libX11 libXcursor libXext libXi libXinerama libXrandr raylib
  ];
  description = "Raylib bindings for Haskell";
  license = lib.licenses.asl20;
}
