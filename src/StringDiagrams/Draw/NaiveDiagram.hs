{-# LANGUAGE NoMonomorphismRestriction    #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# OPTIONS_GHC -Wno-orphans              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}

module StringDiagrams.Draw.NaiveDiagram where

import Diagrams.Prelude
import StringDiagrams.Draw (FoldableDiagram(..), pinch, tensorOps)
import StringDiagrams.Read (leafArity)

instance FoldableDiagram (Path V2 Double) where
    strokeOutput = strokePath
    leaf l = let (al, ar) = leafArity l in
        unitSquare # alignBL # pinch (-al) # pinch ar

    tensor :: Path V2 Double -> Path V2 Double -> Path V2 Double
    tensor d1 d2 = alignB $ d1 # t1 <> d2 # t2 # snugT
        where (t1, t2) = tensorOps d1 d2