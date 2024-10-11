{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;

      raylibRev = "f5328a9bb63a0e0eca7dead15cfa01a3ec1417c2";
      raylibHash = "sha256-gAv1jfMdbKWnlIqqBddVn+WE0BOIARMmagpK61bxVnU=";
      rayguiRev = "1e03efca48c50c5ea4b4a053d5bf04bad58d3e43";
      rayguiHash = "sha256-PzQZxCz63EPd7sVFBYY0T1s9jA5kOAxF9K4ojRoIMz4=";

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
                version = "4.1.0";
                src = self.fetchFromGitHub {
                  owner = "raysan5";
                  repo = "raygui";
                  rev = rayguiRev;
                  sha256 = rayguiHash;
                };
                nativeBuildInputs = [];
                postFixup = "mkdir -p $out/include/ && cp ./src/raygui.h $out/include/ && cp ./styles/**/*.h $out/include/";
              };
            })
          ];
        };
      depsForSystem =
        system:
        pkgs:
        with pkgs; (
          [raylib raygui]
          ++ lib.optionals stdenv.isLinux (with xorg; [libGL libX11 libXcursor libXext libXi libXinerama libXrandr])
          ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [OpenGL Cocoa IOKit CoreVideo CoreAudio CoreFoundation])
        );
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
          baseInputs = pkgs // pkgs.xorg // pkgs.haskellPackages // rec { systemDeps = depsForSystem system pkgs; buildExamples = false; };
        in {
          default = import ./default.nix baseInputs;
          examples = import ./default.nix (baseInputs // rec { buildExamples = true; });
        });
      };
}
