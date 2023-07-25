{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), replaceExtension, takeExtension)
import Data.Tree ( Tree )

import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG, B)

import StringDiagrams.Read (readInputDiagram, NodeType)
import StringDiagrams.Draw (OutputClass(strokeOutput, inputToOutput))

import StringDiagrams.Draw.BrickDiagram ()
import StringDiagrams.Draw.BrickWrapper (strokeBrick)
import StringDiagrams.Draw.OutputDiagram (OutputDiagram , strokeWires)
import StringDiagrams.Draw.WireDiagram (WireDiagram)

--------------------------------------------------------------------------------

process :: Tree NodeType -> QDiagram B V2 Double Any
process input = vsep 0.5 [hsep 0.5 [bd1, bd2, bd3], hsep 0.5 [sd1, sd2, od]]
  where path = input # inputToOutput :: Path V2 Double
        wire = input # inputToOutput :: WireDiagram
        outd = input # inputToOutput :: OutputDiagram
        bd1 = path # strokeOutput
        bd2 = wire # strokeBrick
        bd3 = outd # strokeBrick
        sd1 = wire # strokeOutput
        sd2 = outd # strokeWires
        od  = outd # strokeOutput

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