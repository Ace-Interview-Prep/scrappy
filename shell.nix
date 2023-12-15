{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  scrappy = import ./default.nix;
  drv = variant (haskellPackages.callPackage scrappy {});
  nix-thunk = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  n = import nix-thunk {};
  seleniumStandalone = n.thunkSource ./deps/seleniumExe;
  
  # google-chrome =
  #   let
  #     pkgs = import (builtins.fetchGit {
  #        # Descriptive name to make the store path easier to identify
  #       name = "my-old-revision";
  #       url = "https://github.com/NixOS/nixpkgs/";
  #       ref = "refs/heads/nixpkgs-unstable";
  #       rev = "3c3b3ab88a34ff8026fc69cb78febb9ec9aedb16";
  #     }) {};
  #   in 
  #     pkgs.google-chrome;
  chromedriver =
    let
      pkgs = import (builtins.fetchGit {
         # Descriptive name to make the store path easier to identify
         name = "my-old-revision";
         url = "https://github.com/NixOS/nixpkgs/";
         ref = "refs/heads/nixpkgs-unstable";
         rev = "4ab8a3de296914f3b631121e9ce3884f1d34e1e5";
     }) {};
    in
      pkgs.chromedriver;
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install (pkgs.selenium-server-standalone) ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
  shellHook = '' 
    export PATH=$PATH:${chromedriver}/bin
    # export PATH=$PATH:${seleniumStandalone}/bin
    export PATH=$PATH:${pkgs.selenium-server-standalone}/bin
    ${pkgs.jre8}/bin/java -jar ${seleniumStandalone}/selenium-server-*.jar &
  '' ;              

} 
