{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;

      raylibRev = "289e7d3a6c5b9ee05ff9e4f77db6de7f897e49ec";
      raylibHash = "sha256-uWpk+N3hgTptenHbNbhFyXksRuBJ6jABaXMJMNcspFM=";
      rayguiRev = "f21318f5008e2d4b1a4980b7fd22b3885206981a";
      rayguiHash = "sha256-uh8DWqG7KSsxsxe38ovvJtkMsMmno7DhnTXcUzDU7XY=";

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
                postFixup = "mkdir -p $out/include/ && cp ./src/raygui.h $out/include/ && cp ./examples/styles/*.h $out/include/";
              };
              # temporary fix for CI/CD
              # TODO: remove when github.com/NixOS/nixpkgs/pull/293296 gets merged
              glfw = super.glfw.overrideAttrs (old: {
                version = "3.4.0";
                src = self.fetchFromGitHub {
                  owner = "glfw";
                  repo = "GLFW";
                  rev = "3.4";
                  sha256 = "sha256-FcnQPDeNHgov1Z07gjFze0VMz2diOrpbKZCsI96ngz0=";
                };
                cmakeFlags = old.cmakeFlags ++ [ "-DGLFW_BUILD_WAYLAND=OFF" ];
              });
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
