{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

module StringDiagrams.BrickWrapper ( BrickWrapper, strokeBrick, deformPath, unwrap ) where

import Diagrams.Prelude

import StringDiagrams.Draw
    ( FoldableDiagram(..), pinch, flatCubic, Drawable (..) )
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

------------------------------------------------------------
--  BrickWrapper is Deformable ----------------------------
------------------------------------------------------------

-- from Diagrams.Deform.approx (slightly changed)
deformFixedSeg :: (Fractional n, R1 u, Metric u) =>
    Deformation v u n -> FixedSegment v n -> FixedSegment u n
deformFixedSeg t (FLinear p0 p1) = FLinear (deform t p0) (deform t p1)
deformFixedSeg t (FCubic p0 _ _ p1) = flatCubic (deform t p0) (deform t p1)

-- This is a custom "deformation" for Paths such that only the endpoints (and control points) are moved
deformPath :: (Floating n, Ord n, R1 u, Metric u, Metric v) => 
    Deformation v u n -> Path v n -> Path u n
deformPath t = toPath . map (map (deformFixedSeg t . mkFixedSeg)) . pathLocSegments

instance (InSpace V2 Double a, Deformable a a, r ~ BrickWrapper a) => Deformable (BrickWrapper a) r where
    deform' _ = deform
    deform t = over wrapper (deformPath t) . over user (deform t)

------------------------------------------------------------
--  Other immediate instances  -----------------------------
------------------------------------------------------------

instance (w~BrickWrapper a, Deformable w w) => Transformable (BrickWrapper a) where
    transform = deform . asDeformation

instance (w~BrickWrapper a, Deformable w w) => HasOrigin (BrickWrapper a) where
    moveOriginTo p = deform (Deformation $ moveOriginTo p)

instance Alignable (BrickWrapper a) where
    defaultBoundary v od = defaultBoundary v (od^.wrapper)

instance Traced (BrickWrapper a) where
    getTrace od = getTrace (od^.wrapper)

instance Enveloped (BrickWrapper a) where
    getEnvelope od = getEnvelope (od^.wrapper)

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

    leaf l = BW
        { _wrapper = unitSquare # alignBL # pinch (-al) # pinch ar,
        _user = draw l } where (al, ar) = leafArity l

strokeBrick :: Renderable (Path V2 Double) b => BrickWrapper a -> QDiagram b V2 Double Any
strokeBrick = stroke . (^.wrapper)

unwrap :: BrickWrapper a -> a
unwrap = (^.user)