module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import BrickDiagrams

example :: BrickDiagram
example =
    Compose 
        (Tensor (Tensor (Morphism (1, 2) "0") (Morphism (1, 1) "1")) (Morphism (1, 1) "2")) 
        (Tensor (Morphism (1, 1) "3") (Morphism (3, 5) "4"))

main :: IO ()
main = mainWith $ d # frame 1
    where d = brickToCustom example 
            # deformCD (Deformation $ scaleY 0.5) 
            # (customToDiagram "sd")