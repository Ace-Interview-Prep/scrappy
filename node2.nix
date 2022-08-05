{ pkgs ? import <nixpkgs> {} }:

let
  lib = import <nixpkgs/lib>;
  buildNodeJs = pkgs.callPackage <nixpkgs/pkgs/development/web/nodejs/nodejs.nix> {};

  #nodejsVersion = lib.fileContents ./.nvmrc;

  nodejs = buildNodeJs {
    enableNpm = false;
    version = "14.19.3";
    sha256 = "15691j5zhiikyamiwwd7f282g6d9acfhq91nrwx54xya38gmpx2w";
  };

  NPM_CONFIG_PREFIX = toString ./npm_config_prefix;

in pkgs.mkShell {
  packages = with pkgs; [
    nodejs
    nodePackages.npm
  ];

  inherit NPM_CONFIG_PREFIX;

  shellHook = ''
    export PATH="${NPM_CONFIG_PREFIX}/bin:$PATH"
  '';
}
