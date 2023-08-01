{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE InstanceSigs              #-}

module StringDiagrams.Draw.LabelsDiagram (LabelsDiagram, strokeWires) where

import Data.List ( nubBy )

import Diagrams.Prelude
import Diagrams.Backend.SVG ( B )

import StringDiagrams.Draw
    ( connectionPoints, drawWires, drawCrossingWires,
    Drawable (..), flatCubic )
import StringDiagrams.Read (LeafType(..))
import StringDiagrams.BrickWrapper (BrickWrapper, unwrap)

------------------------------------------------------------
--  UserDiagram type  ------------------------------------
------------------------------------------------------------

-- | A custom object made up of: 
--   1) paths, which are modified only on their end or control points (string diagrams)
--   2) located objects, which are modified only on their origin point (text labels or SD boxes)

data UserDiagram = UD { _sd :: Path V2 Double, _ls :: [Located (Diagram B)] }
$(makeLenses ''UserDiagram)

type instance V UserDiagram = V2
type instance N UserDiagram = Double
instance Semigroup UserDiagram where
    (<>) od1 od2 = UD (od1^.sd <> od2^.sd) (od1^.ls <> od2^.ls)

-- from Diagrams.Deform.approx (slightly changed)
deformFixedSeg :: (Fractional n, R1 u, Metric u) =>
    Deformation v u n -> FixedSegment v n -> FixedSegment u n
deformFixedSeg t (FLinear p0 p1) = FLinear (deform t p0) (deform t p1)
deformFixedSeg t (FCubic p0 _ _ p1) = flatCubic (deform t p0) (deform t p1)

-- This is a custom "deformation" for Paths such that only the endpoints (and control points) are moved
deformPath :: (Floating n, Ord n, R1 u, Metric u, Metric v) =>
    Deformation v u n -> Path v n -> Path u n
deformPath t = toPath . map (map (deformFixedSeg t . mkFixedSeg)) . pathLocSegments

instance r ~ UserDiagram => Deformable UserDiagram r where
    deform' _ = deform
    deform t = over sd (deformPath t) . over (ls . traverse) (deformLoc t)
        where deformLoc t' (Loc o s) = Loc (deform t' o) s

------------------------------------------------------------
--  UserDiagram is Drawable ---------------------------------
------------------------------------------------------------

instance Drawable UserDiagram where
    strokeDrawing od = mconcat -- adding nubBy to remove duplicate wire names
            $  [moveOriginTo (-o) s | (Loc o s) <-
                    nubBy (\a b -> distance (loc a) (loc b) < 0.00001) (od^.ls)]
            ++ [od^.sd # strokePath]

    draw :: LeafType -> UserDiagram
    draw (Morphism (al, ar) s) = UD
        (drawWires (al,ar))
        [ Loc c $ (text s # fontSizeG 0.25 # translateY (-0.0625))
            <> (square 0.3 # scaleY 2 # bg white) ]
        where c = 0.5 ^& (0.25*al + 0.25*ar)

    draw (Crossing p) = UD (drawCrossingWires p) []

    draw (MorphismWNames (als, ars) s) = UD
        (drawWires (al,ar))
        (Loc c ((text s # fontSizeG 0.25 # translateY (-0.0625))
            <> (square 0.3 # scaleY 2 # fc white))
            : wireNames)
        where c = 0.5 ^& (0.25*al + 0.25*ar)
              a@(al, ar) = ((fromIntegral . length) als, (fromIntegral . length) ars)
              (ptsl,ptsr) = connectionPoints a
              funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
              wireNames = funct ptsl als ++ funct ptsr ars

    draw (CrossingWNames ks p) = UD
        (drawCrossingWires p)
        wireNames
        where k = (fromIntegral . length) p
              (ptsl,ptsr) = connectionPoints (k, k)
              funct = zipWith (\c n -> Loc c (text n # fontSizeG 0.25 # translateY 0.0625))
              wireNames = funct ptsl ks ++ funct ptsr (map (ks !!) p)

type LabelsDiagram = BrickWrapper UserDiagram

strokeWires :: Renderable (Path V2 Double) b => LabelsDiagram -> QDiagram b V2 Double Any
strokeWires = stroke . (^.sd) . unwrap