{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
    let label = inputToOutput :: Tree NodeType -> LabelsDiagram
    let framed f inp = (inp # label # f <> (square 2 # alignBL # lc white)) # frame 1
    processFile "hh19.json" (\inp -> (inp # inputToOutput :: Path V2 Double) # strokeOutput) "1-pathAsBD"
    -- processFile "hh19.json" (\inp -> (inp # inputToOutput :: Path V2 Double) # strokeOutput) "2-pathAsBDWrong"
    processFile "simple.json" (\inp -> (inp # inputToOutput :: WiresDiagram) # strokeOutput) "3-pathAsWD"
    processFile "simple.json" (strokeWires . label) "4-outdAsWD"
    processFile "hh19.json" (framed strokeBrick) "5-odBrick"
    processFile "hh19.json" (framed strokeWires) "6-odWire"
    processFile "hh19.json" (framed strokeOutput) "7-odAll"
    processFile "test58.json" (strokeOutput . label) "9-blob"
    processMat "mat.json" "10-mat"

processMat :: FilePath -> FilePath -> IO ()
processMat f s = do
    Right exmp <- readInputDiagram ("test/"++f)
    let mat = exmp # inputToOutput :: MatrixDiagram
    putStr $ prettyMatrix $ getSemantics mat
    renderToFile s (mat # getWrapper # stroke <> mat # strokeOutput)

processFile :: FilePath -> (Tree NodeType -> QDiagram B V2 Double Any) -> FilePath -> IO ()
processFile f p s = do
    Right exmp <- readInputDiagram ("test/"++f)
    renderToFile s (p exmp)

renderToFile :: FilePath -> QDiagram B V2 Double Any -> IO ()
renderToFile s = renderSVG
            ("test/"++s++".svg")
            (mkSizeSpec $ V2 (Just 400) (Just 400))
