{ pkgs ? import ./nix
}:
with pkgs.haskell.lib;
let
  # derive-storable-plugin-src = pkgs.fetchFromGitHub {
  #   repo = "derive-storable-plugin";
  #   owner = "mkloczko";
  #   rev = "0a01dfb483db5bd15ef6b2400b4192b23ab82b2e";
  #   sha256 = "ppLkCL9O6jbjawj4JVwy0z3CIEFzis6jrTMb713pvPE=";
  # };
  overrides = self: super: {
  #  derive-storable-plugin = self.callCabal2nix "derive-storable-plugin" derive-storable-plugin-src {};
    derive-storable = enableCabalFlag super.derive-storable "sumtypes";
  };
  hPkgs = pkgs.haskell.packages.ghc961.override { inherit overrides; };
  src = ./.;
  oracle-simple = hPkgs.callCabal2nix "oracle-simple" src { inherit (pkgs) odpic; };
in
{
  oracle-simple = appendConfigureFlags (disableCabalFlag oracle-simple "default_paths")
    [ "--extra-include-dirs=${pkgs.odpic}/include"
      "--extra-lib-dirs=${pkgs.odpic}/lib"
    ];
  inherit pkgs;
}
