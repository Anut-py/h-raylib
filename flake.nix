{
  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;

      pkgsForSystem =
        system:
        import nixpkgs { inherit system; overlays = [  ]; };
    in
      {
        devShells = forAllSystems (system:
          let
            pkgs = pkgsForSystem system;
          in
            {
              default =
                pkgs.mkShell rec {
                  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
                  buildInputs = with pkgs; [
                    stdenv.cc
                    ghc

                    ((raylib.override { includeEverything = true; }).overrideAttrs (old: {
                      patches = [];
                      src = fetchFromGitHub {
                        owner = "raysan5";
                        repo = "raylib";
                        rev = "b8cd10264b6d34ff4b09ccdd0b0f7b254cf3b122";
                        sha256 = "sha256-VRNQZ0Pp1uczhLSF4Hz2QWiEini2jFtEGJDZMcLE+0w=";
                      };
                      postFixup = ''
                        cp ../src/*.h $out/include/
                      '';
                    }))
                    glfw
                    cabal-install
                  ];
                };
            }
        );
      };
}
