{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Directory (listDirectory)
import System.FilePath ((</>), replaceExtension, takeExtension)

import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG, B)

import StringDiagrams
import StringDiagrams.Draw.NaiveDiagram ()
import StringDiagrams.Draw.WiresDiagram (WiresDiagram)

--------------------------------------------------------------------------------

process :: Tree NodeType -> QDiagram B V2 Double Any
process input = lwG 0.05 $ vsep 0.5 
  [ hsep 0.5 [bd1, bd2, bd3]
  , hsep 0.5 [sd1, sd2, sd3] ]
  where path = input # inputToOutput :: Path V2 Double
        wire = input # inputToOutput :: WiresDiagram
        outd = input # inputToOutput :: LabelsDiagram
        bd1 = path # strokeOutput   -- "BD - Path"
        bd2 = wire # strokeBrick    -- "BD - strokeBrick"
        bd3 = outd # strokeBrick <> sd2    -- "BD + SD"
        sd1 = wire # strokeOutput   -- "SD - Path"
        sd2 = outd # strokeWires    -- "SD - strokeWires"
        sd3 = outd # strokeOutput  -- "SD"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let folderPath = "test/gen"
    fileNames <- listDirectory folderPath
    let filePaths = map (folderPath </>) fileNames
    processFiles filePaths

processFiles :: [FilePath] -> IO ()
processFiles [] = pure ()
processFiles (filePath:rest) =
  case takeExtension filePath of
    ".json" -> processFile filePath >> processFiles rest --  "test/gen/test0.json" -- 
    _ -> processFiles rest

processFile :: FilePath -> IO ()
processFile f = do
    inputDiagram <- readInputDiagram f
    case inputDiagram of
      Left e -> error e
      Right exmp ->
          renderSVG
            (replaceExtension f "svg")
            (mkSizeSpec $ V2 (Just 400) (Just 400))
            (process exmp # frame 1)