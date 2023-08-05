
cd test01
nix-shell haskell.nix

---
cabal init
cabal update
cabal run

---

nix-shell --packages cabal2nix --run "cabal2nix ." > default.nix
touch LICENSE
nix-build release.nix 
./result/bin/test01 

--
