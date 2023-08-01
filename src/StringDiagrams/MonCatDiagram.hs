{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-orphans           #-}
{-# LANGUAGE InstanceSigs #-}

module StringDiagrams.MonCatDiagram ( MonCatDiagram, getCat, getDrawing, getBrick ) where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( FoldableDiagram(..), pinch, tensorOps, composeOps, Drawable (..), Compilable (..) )
import StringDiagrams.Read (leafArity)

type instance N () = Double
type instance V () = V2
instance Deformable () () where deform' _ _ = id; deform _ = id
instance Drawable () where draw _ = (); strokeDrawing _ = mempty
instance Compilable () where
    baseCase _ = (); tensorOp _ _ = (); composeOp _ _ = ()

------------------------------------------------------------
--  (MonCatDiagram a b) type  ------------------------------
------------------------------------------------------------

data MonCatDiagram a b = MD { _wrapper :: Path V2 Double, _drawing :: a, _category :: b }
$(makeLenses ''MonCatDiagram)

type instance V (MonCatDiagram a b) = V2
type instance N (MonCatDiagram a b) = Double

instance (Semigroup a) => Semigroup (MonCatDiagram a b) where
    (<>) od1 od2 = od1 # over wrapper (<> od2^.wrapper) # over drawing (<> od2^.drawing)

instance (InSpace V2 Double a, Deformable a a, r ~ MonCatDiagram a b) => Deformable (MonCatDiagram a b) r where
    deform' e t = over wrapper (deform' e t) . over drawing (deform' e t)
    deform t = over wrapper (deform' 0.0001 t) . over drawing (deform' 0.0001 t)

------------------------------------------------------------
--  (MonCatDiagram a b) instances --------------------------
------------------------------------------------------------

instance (InSpace V2 Double a, Deformable a a) => Transformable (MonCatDiagram a b) where
    transform = deform . asDeformation

instance (InSpace V2 Double a, Deformable a a) => HasOrigin (MonCatDiagram a b) where
    moveOriginTo p = deform (Deformation $ moveOriginTo p)

instance Alignable (MonCatDiagram a b) where
    defaultBoundary v = defaultBoundary v . (^.wrapper)

instance Traced (MonCatDiagram a b) where
    getTrace = getTrace . (^.wrapper)

instance Enveloped (MonCatDiagram a b) where
    getEnvelope = getEnvelope . (^.wrapper)

instance (InSpace V2 Double a, Deformable a a) => Juxtaposable (MonCatDiagram a b) where
    juxtapose = juxtaposeByTrace

-- from Diagrams.Core.Juxtapose.juxtaposeDefault (slightly changed)
juxtaposeByTrace :: (Traced a, HasOrigin a, Num (N a)) => Vn a -> a -> a -> a
juxtaposeByTrace v a1 a2 =
  case (mv1, mv2) of
    (Just v1, Just v2) -> moveOriginBy (v1 ^+^ v2) a2
    _                  -> a2
  where mv1 = negated <$> maxTraceV origin v a1
        mv2 = maxTraceV origin (negated v) a2

------------------------------------------------------------
--  (MonCatDiagram a b) is FoldableDiagram -----------------
------------------------------------------------------------    

instance (Drawable a, Compilable b) => FoldableDiagram (MonCatDiagram a b) where
    strokeOutput d = strokeDrawing (d^.drawing)

    leaf l = MD (unitSquare # alignBL # pinch (-al) # pinch ar) 
        (draw l) (baseCase l) where (al, ar) = leafArity l

    tensor d1 d2 =  (d1 # t1 === d2 # t2) # alignB
        # set category (tensorOp (d1^.category) (d2^.category))
        where (t1, t2) = tensorOps d1 d2
    
    compose d1 d2 =  (d1 # t1 ||| d2 # t2)
        # set category (composeOp (d1^.category) (d2^.category))
        where (t1, t2) = composeOps d1 d2

getCat :: MonCatDiagram a b -> b
getCat = view category

getDrawing :: MonCatDiagram a b -> a
getDrawing = view drawing

getBrick :: MonCatDiagram a b -> Path V2 Double
getBrick = view wrapper
