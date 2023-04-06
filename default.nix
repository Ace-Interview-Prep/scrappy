{ mkDerivation, aeson, base, bibtex, bytestring, containers
, deepseq, directory, exceptions, extra, HTTP, http-client
, http-client-tls, http-types, jsaddle, lens, lib, megaparsec
, MissingH, modern-uri, mtl, network-uri, parallel, parsec, process
, random, raw-strings-qq, stm, template-haskell
, temporary, text, time, transformers, which, nodejs
}:
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bibtex bytestring containers deepseq directory
    exceptions extra HTTP http-client http-client-tls http-types
    jsaddle lens megaparsec MissingH modern-uri mtl network-uri
    parallel parsec process random raw-strings-qq 
    stm template-haskell temporary text time transformers which
  ];
  librarySystemDepends = [ nodejs ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
