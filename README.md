# Multi-Core & Scratchpad Support for Resource-Aware Feldspar [![Build Status](https://travis-ci.org/kmate/raw-feldspar-mcs.svg?branch=master)](https://travis-ci.org/kmate/raw-feldspar-mcs)

## Installation

Here is a suggested incantation:

    git clone git@github.com:emilaxelsson/imperative-edsl
    git clone git@github.com:Feldspar/raw-feldspar
    git clone git@github.com:kmate/zeldspar
    git clone git@github.com:kmate/raw-feldspar-mcs
    cd raw-feldspar-mcs
    cabal sandbox init
    cabal sandbox add-source ../imperative-edsl
    cabal sandbox add-source ../raw-feldspar
    cabal sandbox add-source ../zeldspar
    cabal install --constraint="language-c-quote -full-haskell-antiquotes"

