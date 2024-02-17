{ mkDerivation, base, containers, exceptions, lens, template-haskell, text, bytestring
, lib, systemDeps, buildExamples, ...
}:
mkDerivation {
  pname = "h-raylib";
  version = "5.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = buildExamples;
  configureFlags = ["-fplatform-nixos"] ++ lib.optional buildExamples ["-fexamples"];
  libraryHaskellDepends = [ base containers exceptions lens template-haskell text bytestring ];
  librarySystemDepends = systemDeps;
  description = "Raylib bindings for Haskell";
  license = lib.licenses.asl20;
}
