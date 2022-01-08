{ mkDerivation, base, bytestring, containers, directory, exceptions
, extra, HTTP, http-client, http-client-tls, http-types, lib
, megaparsec, modern-uri, mtl, parallel, parsec, replace-megaparsec
, stm, text, time, transformers, webdriver
}:
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions extra HTTP
    http-client http-client-tls http-types megaparsec modern-uri mtl
    parallel parsec replace-megaparsec stm text time transformers
    webdriver
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
