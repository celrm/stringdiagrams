{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), replaceExtension, takeExtension)

import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG, SVG)

import StringDiagrams.Read (readInputDiagram, NodeType)
import Data.Tree (Tree)
import StringDiagrams.Draw (OutputClass(strokeOutput, inputToOutput))

--------------------------------------------------------------------------------

import StringDiagrams.Draw.BrickDiagram ()

process :: Tree NodeType -> QDiagram SVG V2 Double Any
process input = (input # inputToOutput :: Path V2 Double) # strokeOutput

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