#!/usr/bin/env bash

set -o errexit

stack setup
stack test --ghc-options "-Werror" --no-terminal --haddock --coverage
