#!/usr/bin/env bash

set -o errexit

stack setup
stack install --only-dependencies --no-terminal --haddock
stack test --ghc-options "-Werror" --no-terminal --haddock
