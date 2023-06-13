module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import BrickDiagrams (BrickDiagram(..),drawPath) 

example :: BrickDiagram
example = Compose (Tensor (Morphism (1, 2)) (Morphism (1, 1))) (Tensor (Morphism (1, 1)) (Morphism (2, 1)))

main :: IO ()
main = mainWith $ (path # strokePath :: Diagram B) # frame 1
    where (path, _) = drawPath example