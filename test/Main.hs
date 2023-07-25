{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), replaceExtension, takeExtension)

import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG)

import StringDiagrams.Read (readInputDiagram)
import StringDiagrams.Draw.OutputDiagram (OutputDiagram, outputToStrings)
import StringDiagrams.Draw (OutputClass(..), isoscelify, pinch, arity)
import StringDiagrams.Draw.StringDiagram ()

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
          (d <> cornersPath # pinch (-al) # pinch ar # stroke)
        where d = hsep 0.5 [z]
              x = (exmp # inputToOutput :: OutputDiagram)
                # strokeOutput
              y = (exmp # inputToOutput :: Path V2 Double)
                # strokeOutput
              z = exmp # inputToOutput
                # outputToStrings
              (al,ar) = (exmp # inputToOutput :: OutputDiagram) # arity

cornersPath :: Path V2 Double
cornersPath = toPath [ FLinear (p2 p + 0.00001) (p2 p) | p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]
