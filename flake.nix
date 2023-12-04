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
                src = self.fetchFromGitHub {
                  owner = "raysan5";
                  repo = "raylib";
                  rev = "e33e9da277865207123158430ebf42cc5626e5b7";
                  sha256 = "sha256-tLvaO8zWHx8+NTV17X49JveWdVPG5AKuTzFsyDDaTss=";
                };
                postFixup = ''
                  cp ../src/*.h $out/include/
                '';
              });
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
