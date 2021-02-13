{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.nodejs
    pkgs.purescript
    pkgs.spago
  ];
}
