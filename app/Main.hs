module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( mainWith )

import StringDiagrams.Read ( readInputDiagram )
import StringDiagrams.Draw ( OutputClass(..) )
import StringDiagrams.Draw.OutputDiagram ( OutputDiagram )

main :: IO ()
main = do
  inputDiagram <- readInputDiagram "example.json"
  case inputDiagram of
    Left e -> putStrLn e
    Right inp ->  mainWith $ 
      (inp # inputToOutput :: OutputDiagram) # strokeOutput