{-# LANGUAGE NoMonomorphismRestriction    #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# OPTIONS_GHC -Wno-orphans              #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module StringDiagrams.Draw.NaiveDiagram where

import Diagrams.Prelude
import StringDiagrams.Draw (FoldableDiagram(..), pinch, arity)
import StringDiagrams.Read (LeafType(..), leafArity)

instance FoldableDiagram (Path V2 Double) where
    strokeOutput = strokePath
    leaf (Morphism (al, ar) _) = 
        unitSquare # alignBL # pinch (-al) # pinch ar
    leaf l = leaf $ Morphism (leafArity l) ""

    tensor d1 d2 = alignB $ d1 # t1 <> d2 # t2 # snugT
        where [w1, w2] = [width d1, width d2]
              mw = max w1 w2
              sh = (d2 # arity # snd - d2 # arity # fst)/mw
              (t1, t2) = (shearY sh . scaleX (mw/w1), scaleX (mw/w2))