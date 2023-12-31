
{ pkgs ? import <nixpkgs> {} }:
with pkgs;

# we don't need all the let bindings,
let
  x = 1 ;
  y = 2;
in


# hoogle db written to $HOME/.hoogle. persists through nix-shell restarts

pkgs.stdenv.mkDerivation {
  name = "my-example";

  # start with ghci!!
  # shellHook = ''figlet "Welcome!" | lolcat --freq 0.5 && exec ghci'';
 # shellHook = ''figlet "Welcome!" | lolcat --freq 0.5'';

  buildInputs = [
    figlet
    lolcat

    cabal-install
    ghc
    ghcid
    haskellPackages.hoogle
    haskellPackages.cabal2nix
      
    # OK. so the top level script passes down the dependencies. so we need these
    # if libraries depend on them

    # No. should set these up independently if needed
    #zlib
    #secp256k1

    # haskellPackages.hindent
    # haskellPackages.project-m36
    # haskellPackages.idris
  ];

}
