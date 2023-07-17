{-# LANGUAGE OverloadedStrings #-}

module StringDiagrams.Read (
    readInputDiagram, readInputDiagramWN
) where

import Data.Tree ( Tree(..) )
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import StringDiagrams.Types ( InputDiagram, BlockType(..), Arity, NamedArity)
import Data.List (sort)

newtype TupleDiagram = TID (Arity, InputDiagram)

isPerm :: [Int] -> Bool
isPerm xs = sort xs == [0..k-1]
    where k = (fromIntegral . length) xs

applyPerm :: [Int] -> [a] -> [a]
applyPerm p xs = map (xs !!) p

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
        jps <- v .: "permutation"
        if isPerm jps then 
          return $ TID ((fromIntegral . length $ jps, fromIntegral . length $ jps), 
          Node (Crossing jps) [])
        else fail "Invalid InputDiagram Crossing"
      "CrossingWNames" -> do
        a <- v .: "arity"
        jps <- v .: "permutation"
        if isPerm jps then 
          return $ TID ((fromIntegral . length $ a, fromIntegral . length $ a), 
            Node (CrossingWNames a jps) [])
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
    
readInputDiagram :: FilePath -> IO (Either String InputDiagram)
readInputDiagram path = do
    js <- B.readFile path
    let maybeTuple = eitherDecode js
    let myDiagram (TID (_, diagram)) = diagram
    return (myDiagram <$> maybeTuple)

newtype NamedTupleDiagram = NID (NamedArity, InputDiagram)

instance FromJSON NamedTupleDiagram where
  parseJSON (Object v) = do
    typeName <- v .: "type"
    case typeName :: String of
      "MorphismWNames" -> do
        arityL <- v .: "arityL"
        arityR <- v .: "arityR"
        label <- v .: "label"
        return $ NID ((arityL, arityR), 
          Node (MorphismWNames (arityL,arityR) label) [])
      "CrossingWNames" -> do
        a <- v .: "arity"
        jps <- v .: "permutation"
        if isPerm jps then 
          return $ NID ((a, applyPerm jps a), 
            Node (CrossingWNames a jps) [])
        else fail "Invalid InputDiagram Crossing"
      "Compose" -> do
        NID ((al1,ar1), t1) <- v .: "diagram1"
        NID ((al2,ar2), t2) <- v .: "diagram2"
        if ar1==al2 then 
          return $ NID ((al1,ar2), Node Compose [t1, t2])
        else fail "Invalid InputDiagram Compose"
      "Tensor" -> do
        NID ((al1,ar1), t1) <- v .: "diagram1"
        NID ((al2,ar2), t2) <- v .: "diagram2"
        return $ NID ((al1++al2,ar1++ar2), Node Tensor [t1, t2])
      _ -> fail "Invalid InputDiagram type"

  parseJSON _ = fail "Invalid InputDiagram"
    
readInputDiagramWN :: FilePath -> IO (Either String InputDiagram)
readInputDiagramWN path = do
    js <- B.readFile path
    let maybeTuple = eitherDecode js
    let myDiagram (NID (_, diagram)) = diagram
    return (myDiagram <$> maybeTuple)
