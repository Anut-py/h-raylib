{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;

      raylibRev = "e9291fa4c77c85e1fe6808289632e5ce4a93eed6";
      raylibHash = "sha256-BtDD3L9O7mKcG4O/Nl449q/4sG7pdH/5G2XjvaqzxpU=";
      rayguiRev = "7fe39be75af7d166c50afb6e6b9013398d66a7e1";
      rayguiHash = "sha256-jAVWNNieuYsSVQ/vQbSKOQLf4aedkUUgo8XCQt1oaGY=";

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
                  rev = raylibRev;
                  sha256 = raylibHash;
                };
                postFixup = "cp ../src/*.h $out/include/";
              });
              raygui = super.stdenv.mkDerivation { # A bit of a hack to get raygui working
                name = "raygui";
                version = "4.0.0";
                src = self.fetchFromGitHub {
                  owner = "raysan5";
                  repo = "raygui";
                  rev = rayguiRev;
                  sha256 = rayguiHash;
                };
                nativeBuildInputs = [];
                postFixup = "mkdir -p $out/include/ && cp ./src/raygui.h $out/include/ && cp ./examples/styles/*.h $out/include/";
              };
            })
          ]; 
        };
      depsForSystem =
        system:
        pkgs:
        if pkgs.lib.hasSuffix "linux" system then
          with pkgs; [
            libGL
            xorg.libX11
            xorg.libXcursor
            xorg.libXext
            xorg.libXi
            xorg.libXinerama
            xorg.libXrandr
            raylib
            raygui
          ]
        else if pkgs.lib.hasSuffix "darwin" system then
          with pkgs.darwin.apple_sdk.frameworks; [
            OpenGL
            Cocoa
            IOKit
            CoreVideo
            CoreAudio
            CoreFoundation
          ]
        else [];
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
                  buildInputs = (with pkgs; [stdenv.cc ghc glfw cabal-install]) ++ depsForSystem system pkgs;
                };
            }
        );
        packages = forAllSystems (system: let
          pkgs = pkgsForSystem system;
        in {
          default = import ./default.nix (pkgs // pkgs.xorg // pkgs.haskellPackages // rec { systemDeps = depsForSystem system pkgs; });
        });
      };
}
