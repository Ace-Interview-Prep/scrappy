{ mkDerivation, base, bibtex, bytestring, containers, directory
, exceptions, extra, HTTP, http-client, http-client-tls, http-types
, lens, lib, megaparsec, modern-uri, mtl, parallel, parsec
, replace-megaparsec, stm, text, time, transformers, webdriver
, witherable, jsaddle, which, pkgs, raw-strings-qq, MissingH, nodejs
}:
let
  # TODO(galen): figure out if these even matter or just nodejs or something 
  nodePieces = (import ./node.nix {});
  nodeShell = nodePieces.shell;
  nodeDeps = nodePieces.nodeDependencies;
  nodeWithJSDom = nodePieces.package;
    
in 
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.; 
  libraryHaskellDepends = [
    base bibtex bytestring containers directory exceptions extra HTTP
    http-client http-client-tls http-types lens megaparsec modern-uri
    mtl parallel parsec replace-megaparsec stm text time transformers
    webdriver witherable jsaddle which nodejs nodeDeps
    raw-strings-qq
    MissingH
  ];
  librarySystemDepends = [ nodejs nodeDeps ]; # nodeDeps
  testHaskellDepends = [ base ];
  # buildInputs = [ pkgs.nodejs ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
} // { willThisWork = ""; }
