{ mkDerivation, base, containers, exceptions, lens, template-haskell, text, bytestring
, linear, lib, systemDeps, buildExamples, ...
}:
mkDerivation {
  pname = "h-raylib";
  version = "5.5.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = buildExamples;
  configureFlags = ["-fplatform-nixos"] ++ lib.optional buildExamples ["-fexamples"];
  libraryHaskellDepends = [ base containers exceptions lens linear template-haskell text bytestring ];
  librarySystemDepends = systemDeps;
  description = "Raylib bindings for Haskell";
  license = lib.licenses.asl20;
}
