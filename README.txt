

nix-shell

# cabal init
cabal update
cabal build
cabal run Main01 data/main.dsn

cabal install
~/.cabal/bin/main01
etc

---

nix-shell --packages cabal2nix --run "cabal2nix ." > default.nix
# touch LICENSE
nix-build release.nix
./result/bin/test01

--

