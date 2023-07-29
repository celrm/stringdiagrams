{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), replaceExtension, takeExtension)
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

process :: Tree NodeType -> QDiagram B V2 Double Any
process input = vsep 0.5 [hsep 0.5 [bd1, bd2, bd3], hsep 0.5 [sd1, sd2, od]]
  where path = input # inputToOutput :: Path V2 Double
        wire = input # inputToOutput :: WiresDiagram
        outd = input # inputToOutput :: LabelsDiagram
        bd1 = path # strokeOutput -- "Path as OClass"
        bd2 = wire # strokeBrick -- "Brick of WD"
        bd3 = outd # strokeBrick -- "Brick of OD"
        sd1 = wire # strokeOutput -- "WD: Brick Path"
        sd2 = outd # strokeWires -- "Wires of OD"
        od  = outd # strokeOutput -- "OD: Brick LS"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let folderPath = "test"
    fileNames <- listDirectory folderPath
    let filePaths = map (folderPath </>) fileNames
    processFiles filePaths

processFiles :: [FilePath] -> IO ()
processFiles [] = pure ()
processFiles (filePath:rest) = 
  case takeExtension filePath of
    ".json" -> processFile filePath >> processFiles rest
    _ -> processFiles rest

processFile :: FilePath -> IO ()
processFile f = do
    inputDiagram <- readInputDiagram f
    case inputDiagram of
      Left e -> putStrLn e
      Right exmp ->
          renderSVG
            (replaceExtension f "svg")
            (mkSizeSpec $ V2 (Just 400) (Just 400))
            (process exmp)