{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Tree ( Tree )

import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG, B)

import StringDiagrams.Read (readInputDiagram, NodeType)
import StringDiagrams.Draw (FoldableDiagram(strokeOutput), inputToOutput)

import StringDiagrams.Draw.NaiveDiagram ()
import StringDiagrams.Draw.LabelsDiagram (LabelsDiagram , strokeWires)
import StringDiagrams.Draw.WiresDiagram (WiresDiagram)
import StringDiagrams.BrickWrapper (strokeBrick)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    processFile "test/hh19.json" (\inp -> (inp # inputToOutput :: Path V2 Double) # strokeOutput) "pathAsBD"
    -- processFile "test/hh19.json" (\inp -> (inp # inputToOutput :: Path V2 Double) # strokeOutput) "pathAsBDWrong"
    processFile "test/pointy.json" (\inp -> (inp # inputToOutput :: WiresDiagram) # strokeOutput) "pathAsWD"
    processFile "test/pointy.json" (\inp -> (inp # inputToOutput :: LabelsDiagram) # strokeWires) "outdAsWD"
    processFile "test/hh19.json" (\inp -> ((inp # inputToOutput :: LabelsDiagram) # strokeBrick
      <> (square 2 # alignBL # lc white)) # scale 8 # frame 1) "odBrick"
    processFile "test/hh19.json" (\inp -> ((inp # inputToOutput :: LabelsDiagram) # strokeWires
      <> (square 2 # alignBL # lc white)) # scale 8 # frame 1) "odWire"
    processFile "test/hh19 copy.json" (\inp -> ((inp # inputToOutput :: LabelsDiagram) # strokeOutput
      <> (square 2 # alignBL # lc white)) # scale 8 # frame 1) "odAll"

processFile :: FilePath -> (Tree NodeType -> QDiagram B V2 Double Any) -> FilePath -> IO ()
processFile f p s = do
    inputDiagram <- readInputDiagram f
    case inputDiagram of
      Left e -> putStrLn e
      Right exmp ->
          renderSVG
            (s++".svg")
            (mkSizeSpec $ V2 (Just 400) (Just 400))
            (p exmp)