module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import BrickDiagrams (BrickDiagram(..),drawBrickDiagram) 

example :: BrickDiagram
example = 
    -- Morphism (1, 1) "2"
    -- Tensor (Morphism (1, 1) "3") (Morphism (2, 1) "4")
    Compose (Tensor (Morphism (1, 2) "1") (Morphism (1, 1) "2")) (Tensor (Morphism (1, 1) "3") (Morphism (2, 1) "4"))

main :: IO ()
main = mainWith $ d # frame 1
    where d = drawBrickDiagram example