{ mkDerivation, aeson, base, bytestring, containers
, deepseq, directory, exceptions, extra, http-client
, http-client-tls, http-types, jsaddle, lens, lib, megaparsec
, modern-uri, mtl, network-uri, parallel, parsec, process
, random, stm, template-haskell
, temporary, text, time, transformers, which, nodejs
}:
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq directory
    exceptions extra http-client http-client-tls http-types
    jsaddle lens megaparsec modern-uri mtl network-uri
    parallel parsec process random
    stm template-haskell temporary text time transformers which
  ];
  librarySystemDepends = [ nodejs ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
