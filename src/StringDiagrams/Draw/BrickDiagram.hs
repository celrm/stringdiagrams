{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# OPTIONS_GHC -Wno-orphans           #-}

module StringDiagrams.Draw.BrickDiagram where

import Diagrams.Prelude
import StringDiagrams.Draw (OutputClass(..), pinch, arity)
import StringDiagrams.Read (LeafType(..), leafArity)

instance OutputClass (Path V2 Double) where
    strokeOutput = strokePath
    drawLeaf (Morphism (al, ar) _) = 
        unitSquare # alignBL # pinch (-al) # pinch ar
    drawLeaf l = drawLeaf $ Morphism (leafArity l) ""

    tensor od1 od2 = alignB $
        od1 # scaleX (mw/w1) # shearY ((a2#snd - a2#fst)/mw)
        <> od2 # scaleX (mw/w2) # snugT
        where [w1, w2] = [width od1, width od2]
              mw = max w1 w2
              a2 = od2 # arity