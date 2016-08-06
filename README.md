# Multi-Core & Scratchpad Support for Resource-Aware Feldspar

## Installation

Here is a suggested incantation:

    git clone git@github.com:emilaxelsson/imperative-edsl
    git clone git@github.com:emilaxelsson/raw-feldspar
    git clone git@github.com:kmate/zeldspar
    git clone git@github.com:kmate/raw-feldspar-mcs
    cd raw-feldspar-mcs
    cabal sandbox init
    cabal sandbox add-source ../imperative-edsl
    cabal sandbox add-source ../raw-feldspar
    cabal sandbox add-source ../zeldspar
    cabal install --constraint="language-c-quote -full-haskell-antiquotes"

