{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StringDiagrams.Draw.WireDiagram (WireDiagram) where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( OutputClass(..), drawWires, drawCrossingWires )
import StringDiagrams.Read (LeafType(..), leafArity)
import StringDiagrams.Draw.BrickWrapper (BrickWrapper)

------------------------------------------------------------
--  Drawing (hiding the implementation)  -------------------
------------------------------------------------------------

instance OutputClass (Path V2 Double) where
    strokeOutput = strokePath

    drawLeaf (Morphism a _) = drawWires a
    drawLeaf (Crossing mf) = drawCrossingWires mf
    drawLeaf l@(MorphismWNames _ _) =
        drawLeaf $ Morphism (leafArity l) ""
    drawLeaf (CrossingWNames _ mf) = 
        drawLeaf $ Crossing mf

type WireDiagram = BrickWrapper (Path V2 Double)