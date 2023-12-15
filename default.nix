{ pkgs, mkDerivation }:

let
  
  myPkgs =
    let
      hostPkgs = import <nixpkgs> {};
      pinnedPkgs = hostPkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        # nixos-unstable as of 2017-11-13T08:53:10-00:00
        rev = "b4372c4924d9182034066c823df76d6eaf1f4ec4";
        sha256 = "TJv2srXt6fYPUjxgLAL0cy4nuf1OZD4KuA1TrCiQqg0=";
      };
    in
      import pinnedPkgs {};

  lib = myPkgs.haskell.lib;
  nix-thunkSrc = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  nix-thunk = import nix-thunkSrc {};
  webdriverSrc = nix-thunk.thunkSource ./deps/haskell-webdriver;
  webdriver = myPkgs.haskellPackages.callCabal2nix "webdriver" webdriverSrc {};
  #webdriver = pkgs.haskellPackages.callHackage "webdriver" "0.10.0.0" {};
in
mkDerivation {
  pname = "scrappy";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends =
    with myPkgs.haskellPackages;
    with myPkgs.haskell.lib;
    [
      aeson
      bytestring
      containers
      directory 
      exceptions
      http-client
      http-client-tls
      http-types 
      lens
      modern-uri # less maintained?
      mtl
      network-uri
      parsec
      streaming-commons # required by http-client # TODO: try without, may just need zlib
      time
      text
      transformers
      webdriver
      witherable
    ];
  librarySystemDepends = [ myPkgs.nodejs pkgs.zlib pkgs.gmp ];
  testHaskellDepends = with myPkgs.haskellPackages;
    [
      process
      webdriver
      process
      which
      transformers
      parsec
      text
      exceptions
      lifted-base
      monad-control
    ];
  homepage = "https://github.com/Ace-Interview-Prep/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
