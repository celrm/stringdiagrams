{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG, B)

import StringDiagrams

import StringDiagrams.Draw.NaiveDiagram ()
import StringDiagrams.Draw.WiresDiagram (WiresDiagram)
import StringDiagrams.Draw.LabelsDiagram (LabelsDiagram, strokeWires)
import StringDiagrams.Draw.MatrixDiagram (MatrixDiagram)
import Data.Matrix (prettyMatrix)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    processFile "hh19.json" (\inp -> (inp # inputToOutput :: Path V2 Double) # strokeOutput) "1-pathAsBD"
    -- processFile "hh19.json" (\inp -> (inp # inputToOutput :: Path V2 Double) # strokeOutput) "2-pathAsBDWrong"
    processFile "pointy.json" (\inp -> (inp # inputToOutput :: WiresDiagram) # strokeOutput) "3-pathAsWD"
    processFile "pointy.json" (\inp -> (inp # inputToOutput :: LabelsDiagram) # strokeWires) "4-outdAsWD"
    processFile "hh19.json" (\inp -> ((inp # inputToOutput :: LabelsDiagram) # strokeBrick
      <> (square 2 # alignBL # lc white)) # frame 1) "5-odBrick"
    processFile "hh19.json" (\inp -> ((inp # inputToOutput :: LabelsDiagram) # strokeWires
      <> (square 2 # alignBL # lc white)) # frame 1) "6-odWire"
    processFile "hh19.json" (\inp -> ((inp # inputToOutput :: LabelsDiagram) # strokeOutput
      <> (square 2 # alignBL # lc white)) # frame 1) "7-odAll"
    processFile "test58.json" (\inp -> (inp # inputToOutput :: LabelsDiagram) # strokeOutput) "9-blob"
    processMat "mat.json" "10-mat"

processMat :: FilePath -> FilePath -> IO ()
processMat f s = do
    inputDiagram <- readInputDiagram ("test/old/"++f)
    case inputDiagram of
      Left e -> putStrLn e
      Right exmp -> do
          let mat = exmp # inputToOutput :: MatrixDiagram
          putStr $ prettyMatrix $ getSemantics mat
          renderSVG
            ("test/old/"++s++".svg")
            (mkSizeSpec $ V2 (Just 400) (Just 400))
            (mat # getWrapper # stroke <> mat # strokeOutput)

processFile :: FilePath -> (Tree NodeType -> QDiagram B V2 Double Any) -> FilePath -> IO ()
processFile f p s = do
    inputDiagram <- readInputDiagram ("test/old/"++f)
    case inputDiagram of
      Left e -> putStrLn e
      Right exmp ->
          renderSVG
            ("test/old/"++s++".svg")
            (mkSizeSpec $ V2 (Just 400) (Just 400))
            (p exmp)