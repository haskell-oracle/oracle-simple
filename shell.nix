let
  oraclePkgs = (import ./default.nix {});
  inherit (oraclePkgs) oracle-simple;
in
  with oraclePkgs.pkgs;
  oracle-simple.env.overrideAttrs (drv: {
    shellHook = ''
      export PATH=$PATH:${pkgs.cabal-install}/bin
      function build () {
         cabal configure \
      	   --extra-lib-dirs=${pkgs.odpic}/lib \
      	   --extra-include-dirs=${pkgs.odpic}/include
         cabal build
      }
      function clean () {
         cabal clean
      }
      function ghcid () {
         ${pkgs.ghcid}/bin/ghcid --poll -c 'cabal configure \
      	   --extra-lib-dirs=${pkgs.odpic}/lib \
      	   --extra-include-dirs=${pkgs.odpic}/include && cabal repl'
      }
    '';
  })
