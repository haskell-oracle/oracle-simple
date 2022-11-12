{ lib, stdenv, fetchFromGitHub, fixDarwinDylibNames, odpic, gcc, clang }:

stdenv.mkDerivation {
  name = "odpic-samples";

  src = "${odpic.src}/samples";

  nativeBuildInputs = lib.optional stdenv.isDarwin fixDarwinDylibNames;

  buildInputs = [ odpic gcc clang ];

  makeFlags = [];

  installPhase = ''
    mkdir -p $out/bin
    cp -v build/* $out/bin
  '';

  meta = with lib; {
    description = "Oracle ODPI-C library samples";
    homepage = "https://oracle.github.io/odpi/samples";
    maintainers = with maintainers; [ dmjio ];
    license = licenses.asl20;
    platforms = [ "x86_64-linux" "x86_64-darwin" ];
    hydraPlatforms = [];
  };
}
