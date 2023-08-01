{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-orphans           #-}

module StringDiagrams.Draw.MatrixDiagram (MatrixDiagram) where

import StringDiagrams.Draw (Compilable (..), Drawable (..))
import StringDiagrams.Read (LeafType (..), leafArity)
import StringDiagrams.MonCatDiagram (MonCatDiagram)

import Data.Matrix (Matrix (nrows, ncols), fromLists, matrix, joinBlocks, zero)


import Diagrams.Prelude
import Diagrams.Backend.SVG (B)

newtype Labels = Labels [Located (Diagram B)]
type instance N Labels = Double
type instance V Labels = V2
instance Semigroup Labels where
    (<>) (Labels a) (Labels b) = Labels (a <> b)
instance Deformable Labels Labels where
    deform' _ = deform
    deform t (Labels ls) = Labels $ map (deformLoc t) ls
        where deformLoc t' (Loc o s) = Loc (deform t' o) s
instance Drawable Labels where
    strokeDrawing (Labels ls) = 
        mconcat . map (\(Loc o s) -> moveOriginTo (-o) s) $ ls
    draw (Morphism (al, ar) s) = Labels
        [ Loc c $ text s # fontSizeG 0.25 ]
        where c = 0.5 ^& (0.5+(0.25*(ar-1))+(0.25*(al-1)))
    draw l@(MorphismWNames _ s) = draw (Morphism (leafArity l) s)
    draw _ = Labels []

stringToMatrix :: String -> Matrix Double
stringToMatrix = fromLists . readMatrix
    where readMatrix = read :: String -> [[Double]]

permToMatrix :: [Int] -> Matrix Double
permToMatrix p = matrix (length p) (length p)
    $ \(i,j) -> if p!!(i-1) == j-1 then 1 else 0

instance Compilable (Matrix Double) where
    baseCase (Morphism _ s) = stringToMatrix s
    baseCase (Crossing p) = permToMatrix p
    baseCase (MorphismWNames _ s) = stringToMatrix s
    baseCase (CrossingWNames _ p) = permToMatrix p

    tensorOp a b = joinBlocks (a, z1, z2, b)
        where z1 = Data.Matrix.zero (nrows a) (ncols b)
              z2 = Data.Matrix.zero (nrows b) (ncols a)
    composeOp = (*)

type MatrixDiagram = MonCatDiagram () ()