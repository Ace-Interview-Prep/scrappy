let
  nixpkgs-src = builtins.fetchTarball {
    # nixpkgs 20.03 as of 2020-09-10
    url = "https://github.com/NixOS/nixpkgs/archive/4bd1938e03e1caa49a6da1ec8cff802348458f05.tar.gz";
    sha256 = "0529npmibafjr80i2bhqg22pjr3d5qz1swjcq2jkdla1njagkq2k";
  };

  pkgs = import nixpkgs-src {};

in
  with pkgs;

  mkShell rec {
    buildInputs = [
      cabal-install
      haskell.compiler.ghc865
      haskell.compiler.ghcjs
      pkgconfig
      zlib
      nodejs
    ];

    # Ensure that libz.so and other libraries are available to TH
    # splices, cabal repl, etc.
    LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
  }
