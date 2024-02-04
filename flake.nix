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
                  rev = "40f3df5b865eee0cd87a9e4e1347cb04c87841f8";
                  sha256 = "sha256-yJndpOz1DM9jmroZf5A+82uZ8f6TM+Qraidc9qetvbc=";
                };
                postFixup = "cp ../src/*.h $out/include/";
              });
              raygui = super.stdenv.mkDerivation { # A bit of a hack to get raygui working
                name = "raygui";
                src = self.fetchFromGitHub {
                  owner = "raysan5";
                  repo = "raygui";
                  rev = "45e7f967e62088b9fec02ac38c07d4b67d6466b0";
                  sha256 = "sha256-rt3W8hVAnq4WCnnMFj4sVOq0UjlkglHTKvtMye4w+TA=";
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
