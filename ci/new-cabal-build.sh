#!/bin/bash

source ci/common.sh

message "Versions"
cabal --version
ghc   --version

message "Dependencies"
cabal update

message "Build"
cabal build --enable-tests --allow-newer
cabal test --allow-newer

message "Prepare artifacts"
mkdir -p bin sdist
cabal install --bindir=bin/ --allow-newer
cabal sdist   --builddir=sdist/ --allow-newer
cabal haddock --builddir hackage-docs --haddock-for-hackage --allow-newer

message "Test on own source"
cabal exec -- homplexity-cli lib/ # path is dist/build/homplexity-cli
