{-# LANGUAGE OverloadedStrings #-}

module StringDiagrams.Read (
    readInputDiagram
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import StringDiagrams.DiagramTypes ( InputDiagram(..), Arity )

data TupleDiagram = TID Arity InputDiagram

isPerm :: Maybe [Int] -> Int -> Bool
isPerm Nothing _ = True
isPerm (Just []) 0 = True
isPerm (Just []) _ = False
isPerm (Just (x:xs)) n = isPerm (Just xs) (n-1) && 0<=x

instance FromJSON TupleDiagram where
  parseJSON (Object v) = do
    typeName <- v .: "type"
    case typeName :: String of
      "Crossing" -> do
        a <- v .: "arity"
        jps <- v .:? "permutation"
        if isPerm jps (floor a) then return $ TID (a,a) (Crossing a jps) 
        else fail "Invalid InputDiagram Crossing"
      "Morphism" -> do
        arity <- v .: "arity"
        label <- v .: "label"
        return $ TID arity (Morphism arity label)
      "Compose" -> do
        TID (al1,ar1) diagram1 <- v .: "diagram1"
        TID (al2,ar2) diagram2 <- v .: "diagram2"
        if ar1==al2 then return $ TID (al1,ar2) (Compose diagram1 diagram2)
        else fail "Invalid InputDiagram Compose"
      "Tensor" -> do
        TID (al1,ar1) diagram1 <- v .: "diagram1"
        TID (al2,ar2) diagram2 <- v .: "diagram2"
        return $ TID (al1+al2,ar1+ar2) (Tensor diagram1 diagram2)
      _ -> fail "Invalid InputDiagram type"

  parseJSON _ = fail "Invalid InputDiagram"
    
readInputDiagram :: FilePath -> IO (Maybe InputDiagram)
readInputDiagram path = do
    js <- B.readFile path
    let maybeTuple = decode js :: Maybe TupleDiagram
    let myDiagram (TID _ diagram) = diagram
    return (myDiagram <$> maybeTuple)
