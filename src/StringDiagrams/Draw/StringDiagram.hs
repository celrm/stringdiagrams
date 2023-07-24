{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StringDiagrams.Draw.StringDiagram () where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( OutputClass(..),
      pinch,
      drawWires,
      drawCrossingWires ) 
import StringDiagrams.Read (LeafType(..))

------------------------------------------------------------
--  Drawing (hiding the implementation)  -------------------
------------------------------------------------------------

cornersPath :: Path V2 Double
cornersPath = toPath [ FLinear (p2 p + 0.00001) (p2 p) | p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]

instance OutputClass (Path V2 Double) where
    strokeOutput = strokePath

    drawLeaf (Morphism a@(al, ar) _) =
        (cornersPath <> drawWires a) # pinch (-al) # pinch ar

    drawLeaf (MorphismWNames (als, ars) _) = drawLeaf $
        Morphism (fl als, fl ars) "" where fl = fromIntegral . length

    drawLeaf (Crossing mf) =
        (cornersPath <> drawCrossingWires mf) # pinch (-k) # pinch k
        where k = (fromIntegral . length) mf

    drawLeaf (CrossingWNames _ mf) = drawLeaf $ Crossing mf