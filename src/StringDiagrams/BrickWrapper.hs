{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

module StringDiagrams.BrickWrapper ( 
    BrickWrapper, 
    strokeBrick, 
    unwrap ) where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( FoldableDiagram(..), pinch, Drawable (..) )
import StringDiagrams.Read (leafArity)

------------------------------------------------------------
--  BrickWrapper type  -------------------------------------
------------------------------------------------------------

data BrickWrapper a = BW { _wrapper :: Path V2 Double, _user :: a }
$(makeLenses ''BrickWrapper)

type instance V (BrickWrapper a) = V2
type instance N (BrickWrapper a) = Double

instance (Semigroup a) => Semigroup (BrickWrapper a) where
    (<>) od1 od2 = od1 # over wrapper (<> od2^.wrapper) # over user (<> od2^.user)

instance (InSpace V2 Double a, Deformable a a, r ~ BrickWrapper a) => Deformable (BrickWrapper a) r where
    deform' e t = over wrapper (deform' e t) . over user (deform' e t)
    deform t = over wrapper (deform' 0.0001 t) . over user (deform' 0.0001 t)

------------------------------------------------------------
--  BrickWrapper instances ----------------------------
------------------------------------------------------------

instance (w~BrickWrapper a, Deformable w w) => Transformable (BrickWrapper a) where
    transform = deform . asDeformation

instance (w~BrickWrapper a, Deformable w w) => HasOrigin (BrickWrapper a) where
    moveOriginTo p = deform (Deformation $ moveOriginTo p)

instance Alignable (BrickWrapper a) where
    defaultBoundary v = defaultBoundary v . (^.wrapper)

instance Traced (BrickWrapper a) where
    getTrace = getTrace . (^.wrapper)

instance Enveloped (BrickWrapper a) where
    getEnvelope = getEnvelope . (^.wrapper)

instance (InSpace V2 Double a, Deformable a a) => Juxtaposable (BrickWrapper a) where
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
--  BrickWrapper is FoldableDiagram ----------------------------
------------------------------------------------------------

instance (Drawable a) => FoldableDiagram (BrickWrapper a) where
    strokeOutput = strokeDrawing . (^.user)
    leaf l = let (al, ar) = leafArity l in BW { _user = draw l, 
        _wrapper = unitSquare # alignBL # pinch (-al) # pinch ar }

strokeBrick :: Renderable (Path V2 Double) b => BrickWrapper a -> QDiagram b V2 Double Any
strokeBrick = stroke . (^.wrapper)

unwrap :: BrickWrapper a -> a
unwrap = (^.user)