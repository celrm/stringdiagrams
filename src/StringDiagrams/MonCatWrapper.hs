{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-orphans           #-}
{-# LANGUAGE InstanceSigs #-}

module StringDiagrams.MonCatWrapper ( MonCatWrapper, getSemantics, getDrawing, getWrapper, Drawable(..), Compilable(..) ) where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( FoldableDiagram(..), pinch, tensorOps, composeOps )
import StringDiagrams.Read (leafArity, LeafType)
import StringDiagrams.BrickWrapper (Drawable (..))

class Compilable a where
    baseCase :: LeafType -> a
    tensorOp :: a -> a -> a
    composeOp :: a -> a -> a

type instance N () = Double
type instance V () = V2
instance Deformable () () where deform' _ _ = id; deform _ = id
instance Drawable () where draw _ = (); strokeDrawing _ = mempty
instance Compilable () where
    baseCase _ = (); tensorOp _ _ = (); composeOp _ _ = ()

------------------------------------------------------------
--  (MonCatWrapper a b) type  ------------------------------
------------------------------------------------------------

data MonCatWrapper a b = MD { _wrapper :: Path V2 Double, _drawing :: a, _semantics :: b }
$(makeLenses ''MonCatWrapper)

type instance V (MonCatWrapper a b) = V2
type instance N (MonCatWrapper a b) = Double

instance (Semigroup a) => Semigroup (MonCatWrapper a b) where
    (<>) od1 od2 = MD (od1^.wrapper <> od2^.wrapper) (od1^.drawing <> od2^.drawing) (od1^.semantics)

instance (InSpace V2 Double a, Deformable a a, r ~ MonCatWrapper a b) => Deformable (MonCatWrapper a b) r where
    deform' e t = over wrapper (deform' e t) . over drawing (deform' e t)
    deform t = over wrapper (deform' 0.0001 t) . over drawing (deform' 0.0001 t)

------------------------------------------------------------
--  (MonCatWrapper a b) instances --------------------------
------------------------------------------------------------

instance (InSpace V2 Double a, Deformable a a) => Transformable (MonCatWrapper a b) where
    transform = deform . asDeformation

instance (InSpace V2 Double a, Deformable a a) => HasOrigin (MonCatWrapper a b) where
    moveOriginTo p = deform (Deformation $ moveOriginTo p)

instance Alignable (MonCatWrapper a b) where
    defaultBoundary v = defaultBoundary v . (^.wrapper)

instance Traced (MonCatWrapper a b) where
    getTrace = getTrace . (^.wrapper)

instance Enveloped (MonCatWrapper a b) where
    getEnvelope = getEnvelope . (^.wrapper)

instance (InSpace V2 Double a, Deformable a a) => Juxtaposable (MonCatWrapper a b) where
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
--  (MonCatWrapper a b) is FoldableDiagram -----------------
------------------------------------------------------------    

instance (Drawable a, Compilable b)
    => FoldableDiagram (MonCatWrapper a b) where
    strokeOutput d = strokeDrawing (d^.drawing)

    leaf l = MD (unitSquare # alignBL # pinch (-al) # pinch ar) 
        (draw l) (baseCase l) where (al, ar) = leafArity l

    tensor d1 d2 =  (d1 # t1 === d2 # t2) # alignB
        # set semantics (tensorOp (d1^.semantics) (d2^.semantics))
        where (t1, t2) = tensorOps d1 d2
    
    compose d1 d2 =  (d1 # t1 ||| d2 # t2)
        # set semantics (composeOp (d1^.semantics) (d2^.semantics))
        where (t1, t2) = composeOps d1 d2

getSemantics :: MonCatWrapper a b -> b
getSemantics = view semantics

getDrawing :: MonCatWrapper a b -> a
getDrawing = view drawing

getWrapper :: MonCatWrapper a b -> Path V2 Double
getWrapper = view wrapper