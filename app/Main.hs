module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( mainWith, B )

import StringDiagrams.Read (readInputDiagram)
import StringDiagrams.Draw (OutputClass(..))
import StringDiagrams.Draw.BrickWrapper (strokeBrick)
import StringDiagrams.Draw.OutputDiagram (OutputDiagram)

main :: IO ()
main = do
  inputDiagram <- readInputDiagram "example.json"
  case inputDiagram of
    Left e -> putStrLn e
    Right inp ->  mainWith $ mconcat [ppp # strokeBrick :: Diagram B] -- , ppp # strokeBrick
      where ppp = (inp # inputToOutput :: OutputDiagram)