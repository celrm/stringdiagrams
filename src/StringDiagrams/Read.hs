{-# LANGUAGE OverloadedStrings #-}

module StringDiagrams.Read (
    readInputDiagram
) where

import Data.Tree ( Tree(..) )
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import StringDiagrams.Types ( InputDiagram, BlockType(..), Arity )

newtype TupleDiagram = TID (Arity, InputDiagram)

isPerm :: Maybe [Int] -> Int -> Bool
isPerm Nothing _ = True
isPerm (Just []) 0 = True
isPerm (Just []) _ = False
isPerm (Just (x:xs)) n = isPerm (Just xs) (n-1) && 0<=x

instance FromJSON TupleDiagram where
  parseJSON (Object v) = do
    typeName <- v .: "type"
    case typeName :: String of
      "Morphism" -> do
        arity <- v .: "arity"
        label <- v .: "label"
        return $ TID (arity, Node (Morphism arity label) [])
      "MorphismWNames" -> do
        arityL <- v .: "arityL"
        arityR <- v .: "arityR"
        label <- v .: "label"
        return $ TID ((fromIntegral . length $ arityL, fromIntegral . length $ arityR), 
          Node (MorphismWNames (arityL,arityR) label) [])
      "Crossing" -> do
        a <- v .: "arity"
        jps <- v .:? "permutation"
        if isPerm jps (floor a) then 
          return $ TID ((a,a), Node (Crossing a jps) [])
        else fail "Invalid InputDiagram Crossing"
      "Compose" -> do
        TID ((al1,ar1), t1) <- v .: "diagram1"
        TID ((al2,ar2), t2) <- v .: "diagram2"
        if ar1==al2 then 
          return $ TID ((al1,ar2), Node Compose [t1, t2])
        else fail "Invalid InputDiagram Compose"
      "Tensor" -> do
        TID ((al1,ar1), t1) <- v .: "diagram1"
        TID ((al2,ar2), t2) <- v .: "diagram2"
        return $ TID ((al1+al2,ar1+ar2), Node Tensor [t1, t2])
      _ -> fail "Invalid InputDiagram type"

  parseJSON _ = fail "Invalid InputDiagram"
    
readInputDiagram :: FilePath -> IO (Maybe InputDiagram)
readInputDiagram path = do
    js <- B.readFile path
    let maybeTuple = decode js
    let myDiagram (TID (_, diagram)) = diagram
    return (myDiagram <$> maybeTuple)
