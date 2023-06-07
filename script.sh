#!/bin/bash

cabal build
cabal exec stringdiagrams -- -o basicDiagram.svg -w 400 -l -s BasicDiagram.hs
