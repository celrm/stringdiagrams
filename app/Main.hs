module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( mainWith )

import qualified StringDiagrams.SimpleDraw as SimpleDraw
import qualified StringDiagrams.Draw as Draw
import StringDiagrams.Read

main :: IO ()
main = do
    inputDiagram <- readInputDiagram "example.json"
    case inputDiagram of
      Left e -> putStrLn e
      Right exmp ->  mainWith (d # frame 1)
        where d = hsep 0.5 [e,f]
              e = exmp # Draw.inputToOutput # Draw.isoscelify # scaleY 0.5 # Draw.outputToStrings
              f = exmp # SimpleDraw.inputToOutput # SimpleDraw.isoscelify # scaleY 0.5 # stroke