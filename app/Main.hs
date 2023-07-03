module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import StringDiagrams.Draw
import StringDiagrams.Read

main :: IO ()
main = do
    inputDiagram <- readInputDiagram "example.json"
    case inputDiagram of
      Nothing -> putStrLn "Failed to parse JSON file."
      Just exmp -> mainWith $ d # frame 1
          where d = exmp 
                  # inputToOutput
                  # scaleY 0.4 # scaleX 1.5
                  # outputToDiagram "sd"