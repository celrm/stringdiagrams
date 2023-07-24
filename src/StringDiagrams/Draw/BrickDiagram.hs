{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StringDiagrams.Draw.BrickDiagram where

import Diagrams.Prelude
import StringDiagrams.Draw (OutputClass(..), pinch)

instance OutputClass (Path V2 Double) where
    drawMorphism (al, ar) _ = unitSquare # alignBL # pinch (-al) # pinch ar
    strokeOutput = strokePath

    drawCrossing mf = drawMorphism (k, k) "" where k = (fromIntegral . length) mf
    drawMorphismWNames (als, ars) _ = drawMorphism ((fromIntegral . length) als, (fromIntegral . length) ars) ""
    drawCrossingWNames _ = drawCrossing