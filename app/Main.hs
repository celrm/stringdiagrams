module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( mainWith )

import StringDiagrams.Draw
import StringDiagrams.Read

main :: IO ()
main = do
    inputDiagram <- readInputDiagram "example.json"
    case inputDiagram of
      Left e -> putStrLn e
      Right exmp ->  mainWith (d # frame 1)
        where d = hsep 0.5 [e,f,g]
              f = (exmp # inputToOutput :: Path V2 Double) 
                # isoscelify # scaleY 0.5 # strokeOutput
              e = (exmp # inputToOutput :: OutputDiagram) 
                # isoscelify # scaleY 0.5 # strokeOutput
              g = exmp # inputToOutput
                # isoscelify # scaleY 0.5 # outputToStrings