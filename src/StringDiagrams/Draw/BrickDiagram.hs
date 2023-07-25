{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StringDiagrams.Draw.BrickDiagram where

import Diagrams.Prelude
import StringDiagrams.Draw (OutputClass(..), pinch)
import StringDiagrams.Read (LeafType(..))

instance OutputClass (Path V2 Double) where

    strokeOutput = strokePath

    drawLeaf (Morphism (al, ar) _) = 
        unitSquare # alignBL # pinch (-al) # pinch ar
    drawLeaf (MorphismWNames (als, ars) _) = drawLeaf $
        Morphism (fl als, fl ars) "" where fl = fromIntegral . length
    drawLeaf (Crossing mf) = drawLeaf $ 
        Morphism (k, k) "" where k = (fromIntegral . length) mf
    drawLeaf (CrossingWNames _ mf) = drawLeaf $ Crossing mf