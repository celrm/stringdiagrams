{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# OPTIONS_GHC -Wno-orphans           #-}

module StringDiagrams.Draw.WiresDiagram (WiresDiagram) where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( drawWires, drawCrossingWires, Drawable (..) )
import StringDiagrams.Read (LeafType(..), leafArity)
import StringDiagrams.BrickWrapper (BrickWrapper)

------------------------------------------------------------
--  Drawing (hiding the implementation)  -------------------
------------------------------------------------------------

instance Drawable (Path V2 Double) where
    strokeDrawing = strokePath

    draw (Morphism a _) = drawWires a
    draw (Crossing p) = drawCrossingWires p
    draw l@(MorphismWNames _ _) =
        draw $ Morphism (leafArity l) ""
    draw (CrossingWNames _ p) = 
        draw $ Crossing p

type WiresDiagram = BrickWrapper (Path V2 Double)