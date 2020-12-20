#!/usr/bin/env bash

set -e

cabal new-build

rm -rf dist/docs > /dev/null 2>&1

stack exec -- haddock --html src/Chapter15/BTEx.hs --hyperlinked-source --odir=dist/docs

open dist/docs

