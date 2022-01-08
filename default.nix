<<<<<<< HEAD
{ mkDerivation, base, bytestring, containers, directory, exceptions
, extra, HTTP, http-client, http-client-tls, http-types, lib
, megaparsec, modern-uri, mtl, parallel, parsec, replace-megaparsec
, stm, text, time, transformers, webdriver
=======
{ mkDerivation, base, bytestring, containers, extra, HTTP
, http-client, http-client-tls, lib, megaparsec, modern-uri, parsec
, replace-megaparsec, text, time, pkgs,  mtl, transformers, stdenv, parallel, webdriver, bibtex
>>>>>>> origin/dyingLaptop
}:
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
<<<<<<< HEAD
    base bytestring containers directory exceptions extra HTTP
    http-client http-client-tls http-types megaparsec modern-uri mtl
    parallel parsec replace-megaparsec stm text time transformers
    webdriver
=======
    base bytestring containers extra HTTP http-client http-client-tls
    megaparsec modern-uri parsec replace-megaparsec text parallel webdriver bibtex
  ];
  librarySystemDepends = [
    pkgs.zlib
>>>>>>> origin/dyingLaptop
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
