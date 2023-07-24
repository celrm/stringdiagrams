{-# LANGUAGE OverloadedStrings #-}

module StringDiagrams.Read (
    LeafType(..), NodeType(..), Arity, NamedArity,
    readInputDiagram, readInputDiagramWN
) where

import Data.Tree ( Tree(..) )
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (sort)

------------------------------------------------------------
--  InputDiagram type  -------------------------------------
------------------------------------------------------------

-- | A binary tree-like structure where leaves are either boxes or string crossings
--   and internal nodes have either composition or tensoring.

type Arity = (Double, Double)
type NamedArity = ([String], [String])

data LeafType = Morphism Arity String
    | MorphismWNames NamedArity String
    | Crossing [Int]
    | CrossingWNames [String] [Int]

data NodeType = Leaf LeafType
    | Compose
    | Tensor

------------------------------------------------------------
--  Reading InputDiagram from JSON  -------------------------
------------------------------------------------------------

newtype TupleDiagram = TID (Arity, Tree NodeType)

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
        return $ TID (arity, Node (Leaf $ Morphism arity label) [])
      "MorphismWNames" -> do
        arityL <- v .: "arityL"
        arityR <- v .: "arityR"
        label <- v .: "label"
        return $ TID ((fromIntegral . length $ arityL, fromIntegral . length $ arityR), 
          Node (Leaf $ MorphismWNames (arityL,arityR) label) [])
      "Crossing" -> do
        jps <- v .: "permutation"
        if isPerm jps then 
          return $ TID ((fromIntegral . length $ jps, fromIntegral . length $ jps), 
          Node (Leaf $ Crossing jps) [])
        else fail "Invalid InputDiagram Crossing"
      "CrossingWNames" -> do
        a <- v .: "arity"
        jps <- v .: "permutation"
        if isPerm jps then 
          return $ TID ((fromIntegral . length $ a, fromIntegral . length $ a), 
            Node (Leaf $ CrossingWNames a jps) [])
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
    
readInputDiagram :: FilePath -> IO (Either String (Tree NodeType))
readInputDiagram path = do
    js <- B.readFile path
    let maybeTuple = eitherDecode js
    let myDiagram (TID (_, diagram)) = diagram
    return (myDiagram <$> maybeTuple)

newtype NamedTupleDiagram = NID (NamedArity, Tree NodeType)

instance FromJSON NamedTupleDiagram where
  parseJSON (Object v) = do
    typeName <- v .: "type"
    case typeName :: String of
      "MorphismWNames" -> do
        arityL <- v .: "arityL"
        arityR <- v .: "arityR"
        label <- v .: "label"
        return $ NID ((arityL, arityR), 
          Node (Leaf $ MorphismWNames (arityL,arityR) label) [])
      "CrossingWNames" -> do
        a <- v .: "arity"
        jps <- v .: "permutation"
        if isPerm jps then 
          return $ NID ((a, applyPerm jps a), 
            Node (Leaf $ CrossingWNames a jps) [])
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
    
readInputDiagramWN :: FilePath -> IO (Either String (Tree NodeType))
readInputDiagramWN path = do
    js <- B.readFile path
    let maybeTuple = eitherDecode js
    let myDiagram (NID (_, diagram)) = diagram
    return (myDiagram <$> maybeTuple)
