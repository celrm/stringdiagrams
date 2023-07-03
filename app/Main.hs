module Main where

import Diagrams.Prelude ( frame, scaleX, scaleY, (#) )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
import StringDiagrams.Draw ( inputToOutput, outputToStringDiagram, isoscelify )
import StringDiagrams.Read ( readInputDiagram )

main :: IO ()
main = do
    inputDiagram <- readInputDiagram "example.json"
    case inputDiagram of
      Nothing -> putStrLn "Failed to parse JSON file."
      Just exmp -> mainWith $ d # frame 1
          where d = exmp 
                  # inputToOutput
                  # scaleY 0.4 # scaleX 1.5 # isoscelify
                  # outputToStringDiagram