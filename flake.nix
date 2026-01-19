{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems' = nixpkgs.lib.genAttrs;
      forAllSystems = forAllSystems' supportedSystems;

      raylibRev = "a8c75f2bc537b65c6b6621819a1445c473ec6f83";
      raylibHash = "sha256-HidbA+qY7nAiCgIsjihu9QdlF1LiLKlhSeIu4gqGtXU=";
      rayguiRev = "9a1c183d8539e2470635b22f92bdaefaf5218aaa";
      rayguiHash = "sha256-eRcF41quNwlEn5lHVaoyn1wmppZ5Ou+M8c908qFvLek=";

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
          # ++ lib.optionals stdenv.isDarwin (with darwin.apple-sdk; [OpenGL Cocoa IOKit CoreVideo CoreAudio CoreFoundation])
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
          baseInputs = pkgs // pkgs.xorg // pks.apple-sdk // pkgs.haskellPackages // rec { systemDeps = depsForSystem system pkgs; buildExamples = false; };
        in {
          default = import ./default.nix baseInputs;
          examples = import ./default.nix (baseInputs // rec { buildExamples = true; });
        });
      };
}
