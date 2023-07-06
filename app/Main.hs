module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( mainWith )

import StringDiagrams.Draw
import StringDiagrams.Read

main :: IO ()
main = do
    inputDiagram <- readInputDiagramWN "example.json"
    case inputDiagram of
      Nothing -> putStrLn "Failed to parse JSON file."
      Just exmp -> mainWith $ d # frame 1
          where d = exmp 
                  # inputToOutput 
                  # isoscelify # scaleY 0.5
                  # outputToStringDiagram