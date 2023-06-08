#!/bin/bash

cabal build
cabal exec stringdiagrams -- -o diagram.svg -w 400 -l -s PointDiagram.hs
