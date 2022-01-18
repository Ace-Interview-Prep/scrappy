{ mkDerivation, base, bibtex, bytestring, containers, directory
, exceptions, extra, HTTP, http-client, http-client-tls, http-types
, lens, lib, megaparsec, modern-uri, mtl, parallel, parsec
, replace-megaparsec, stm, text, time, transformers, webdriver
, witherable
}:
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base bibtex bytestring containers directory exceptions extra HTTP
    http-client http-client-tls http-types lens megaparsec modern-uri
    mtl parallel parsec replace-megaparsec stm text time transformers
    webdriver witherable
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
