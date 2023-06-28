module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import BrickDiagrams

example :: BrickDiagram
example =
    Compose 
        (Tensor (Tensor (Morphism (7, 2) "0") (Morphism (1, 2) "1")) (Morphism (2, 2) "2")) 
        (Tensor (Morphism (1, 1) "3") (Tensor (Morphism (2, 1) "4") (Crossing 3 (Just f))))
        where 
            f 0 = 1
            f 1 = 2
            f 2 = 0

main :: IO ()
main = mainWith $ d # frame 1
    where d = brickToCustom example 
            # scaleYCD 0.4 # scaleXCD 1.5
            -- # squarify 
            # customToDiagram "sd"