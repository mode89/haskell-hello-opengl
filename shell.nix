{ pkgs ? import <nixpkgs> {}, ... }:

pkgs.mkShell {
  packages = with pkgs; [
    libGL
    libGLU
    xorg.libX11
    xorg.libXi
    xorg.libXrandr
    xorg.libXcursor
    xorg.libXinerama
    xorg.libXxf86vm
    cabal-install
    ghc
    haskellPackages.haskell-language-server
  ];
}
