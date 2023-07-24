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

------------------------------------------------------------
--  Drawing (hiding the implementation)  -------------------
------------------------------------------------------------

cornersPath :: Path V2 Double
cornersPath = toPath [ FLinear (p2 p + 0.00001) (p2 p) | p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]

instance OutputClass (Path V2 Double) where
    drawMorphism a@(al, ar) _ =
        (cornersPath <> drawWires a)
        # pinch (-al) # pinch ar

    drawCrossing mf =
        (cornersPath <> drawCrossingWires mf) 
        # pinch (-k) # pinch k
        where k = (fromIntegral . length) mf

    strokeOutput = strokePath

    drawMorphismWNames (als, ars) _ = drawMorphism ((fromIntegral . length) als, (fromIntegral . length) ars) ""

    drawCrossingWNames _ = drawCrossing