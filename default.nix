{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
}:
with pkgs.haskell.lib;
let
  generic-storable-src = pkgs.fetchFromGitHub {
    owner = "tanakh";
    repo = "generic-storable";
    rev = "e0293a5734068b80038259bab664aef62bded7d9";
    sha256 = "0ijkqf9dkbzyx1wdnjm3p4kbkp9bcdz0xsgpwbq2nkr6b70856mc";
  };
  odpic = pkgs.callPackage ./nix/odpic {};
  odpic-samples = pkgs.callPackage ./nix/odpic/samples.nix { inherit odpic; };
  overrides = self: super: {
    generic-storable = dontCheck (doJailbreak (super.callCabal2nix "generic-storable" generic-storable-src {}));
  };
  hPkgs = pkgs.haskellPackages.override { inherit overrides; };
  src = ./.;
  oracle-simple = hPkgs.callCabal2nix "oracle-simple" src { inherit odpic; };
  gen = pkgs.writeScriptBin "gen" ''
    ${pkgs.ghc}/bin/hsc2hs -I${odpic}/include \
       ./src/Database/Oracle/Simple/Internal.hsc
  '';
in
{
  inherit (hPkgs) generic-storable;
  inherit gen odpic odpic-samples;
  pkg = appendConfigureFlags (disableCabalFlag oracle-simple "default_paths")
    [ "--extra-include-dirs=${odpic}/include"
      "--extra-lib-dirs=${odpic}/lib"
    ];
}
