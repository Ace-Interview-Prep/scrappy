{ mkDerivation, base, bytestring, containers, extra, HTTP
, http-client, http-client-tls, lib, megaparsec, modern-uri, parsec
, replace-megaparsec, text, time, pkgs,  mtl, transformers, stdenv, parallel, webdriver, bibtex
}:
with pkgs;
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers extra HTTP http-client http-client-tls
    megaparsec modern-uri parsec replace-megaparsec text parallel webdriver bibtex
  ];
  librarySystemDepends = [
    pkgs.zlib
  ];
  testHaskellDepends = [ base ];
  homepage = "tbd";
  description = "html pattern matching library and high-level interface requests lib for webscraping";
  license = lib.licenses.bsd3;
}
