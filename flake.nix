{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;

      pkgsForSystem =
        system:
        import nixpkgs { 
          inherit system; 
          overlays = [  
            (self: super: {
              raylib = super.raylib.overrideAttrs (old: {
                patches = [];
                version = "5.0.0";
                src = self.fetchFromGitHub {
                  owner = "raysan5";
                  repo = "raylib";
                  rev = "e9291fa4c77c85e1fe6808289632e5ce4a93eed6";
                  sha256 = "sha256-BtDD3L9O7mKcG4O/Nl449q/4sG7pdH/5G2XjvaqzxpU=";
                };
                postFixup = "cp ../src/*.h $out/include/";
              });
              raygui = super.stdenv.mkDerivation { # A bit of a hack to get raygui working
                name = "raygui";
                src = self.fetchFromGitHub {
                  owner = "raysan5";
                  repo = "raygui";
                  rev = "7fe39be75af7d166c50afb6e6b9013398d66a7e1";
                  sha256 = "sha256-jAVWNNieuYsSVQ/vQbSKOQLf4aedkUUgo8XCQt1oaGY=";
                };
                nativeBuildInputs = [];
                postFixup = "mkdir -p $out/include/ && cp ./src/raygui.h $out/include/ && cp ./examples/styles/*.h $out/include/";
              };
            })
          ]; 
        };
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
                    glfw
                    cabal-install
                    xorg.libXinerama
                    xorg.libXcursor
                    xorg.libXrandr
                    xorg.libXi
                    xorg.libXext
                    raylib
                    raygui
                  ];
                };
            }
        );
        packages = forAllSystems (system: let
          pkgs = pkgsForSystem system;
        in {
          default = import ./default.nix (pkgs // pkgs.xorg // pkgs.haskellPackages);
        });
      };
}
