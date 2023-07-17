{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StringDiagrams.Types (
    Arity,
    NamedArity,
    BlockType(..), 
    InputDiagram,
    Paths(..),
    Locatables(..),
    OutputDiagram(..),
    drawCubic
) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )
import Data.Tree (Tree)

------------------------------------------------------------
--  InputDiagram type  -------------------------------------
------------------------------------------------------------

-- | A binary tree-like structure where leaves are either boxes or string crossings
--   and internal nodes have either composition or tensoring.

type Arity = (Double, Double)
type NamedArity = ([String], [String])
data BlockType =
    Morphism Arity String
    | MorphismWNames NamedArity String
    | Crossing [Int]
    | CrossingWNames [String] [Int]
    | Compose
    | Tensor

type InputDiagram = Tree BlockType

------------------------------------------------------------
--  OutputDiagram type  ------------------------------------
------------------------------------------------------------

-- | A custom object made up of: 
--   1) paths, which are modified only on their end or control points (brick or string diagrams)
--   2) located objects, which are modified only on their origin point (text labels or SD boxes)

data Paths = Paths { _bd :: Path V2 Double, _sd :: Path V2 Double }
$(makeLenses ''Paths)
type instance V Paths = V2
type instance N Paths = Double
instance Semigroup Paths where
    (<>) ps1 ps2 = ps1 # over bd (<> ps2^.bd) # over sd (<> ps2^.sd)

data Locatables = Locs { _labels :: [Located (Diagram B)], _boxes :: [Located (Diagram B)] }
$(makeLenses ''Locatables)
type instance V Locatables = V2
type instance N Locatables = Double
instance Semigroup Locatables where
    (<>) ls1 ls2 = ls1 # over labels (<> ls2^.labels) # over boxes (<> ls2^.boxes)

data OutputDiagram = OD { _ps :: Paths, _ls :: Locatables }
$(makeLenses ''OutputDiagram)
type instance V OutputDiagram = V2
type instance N OutputDiagram = Double
instance Semigroup OutputDiagram where
    (<>) od1 od2 = od1 # over ps (<> od2^.ps) # over ls (<> od2^.ls)

------------------------------------------------------------
--  OutputDiagram is Deformable ----------------------------
------------------------------------------------------------

-- Chooses control points in the same Y-coord than the endpoints
drawCubic :: (R1 v, Metric v, Fractional n) => Point v n -> Point v n -> FixedSegment v n
drawCubic o f = FCubic o (c o f) (c f o) f
    where c x y = x .+^ (0.5 *^ project unitX (y .-. x))

-- This is a custom "deformation" for Paths such that only the endpoints (and control points) are moved
instance r ~ Paths => Deformable Paths r where
    deform' _ = deform
    deform t = over bd deformPath . over sd deformPath
        where -- do not want to make Paths, FixedSeg or Located as Deformable, but we could
              deformPath = toPath . map (map (deformFixedSeg . mkFixedSeg)) . pathLocSegments
              -- from Diagrams.Deform.approx (slightly changed)
              deformFixedSeg (FLinear p0 p1) = FLinear (deform t p0) (deform t p1)
              deformFixedSeg (FCubic p0 _ _ p1) = drawCubic (deform t p0) (deform t p1)

-- This is a custom "deformation" for Located objects such that only the origin is moved
instance r ~ Locatables => Deformable Locatables r where
    deform' _ = deform
    deform t = over (labels . traverse) deformLoc . over (boxes . traverse) deformLoc
        where deformLoc (Loc o s) = Loc (deform t o) s

instance r ~ OutputDiagram => Deformable OutputDiagram r where
    deform' _ = deform
    deform t = over ps (deform t) . over ls (deform t)

------------------------------------------------------------
--  Other immediate instances  -----------------------------
------------------------------------------------------------

instance Transformable OutputDiagram where
    transform = deform . asDeformation

instance HasOrigin OutputDiagram where
    moveOriginTo p = deform (Deformation $ moveOriginTo p)

instance Alignable OutputDiagram where
    defaultBoundary v od = defaultBoundary v (od^.ps.bd)

instance Traced OutputDiagram where
    getTrace od = getTrace (od^.ps.bd)

instance Enveloped OutputDiagram where
    getEnvelope od = getEnvelope (od^.ps.bd)

instance Juxtaposable OutputDiagram where
    juxtapose = juxtaposeByTrace

-- from Diagrams.Core.Juxtapose.juxtaposeDefault (slightly changed)
juxtaposeByTrace :: (Traced a, HasOrigin a, Num (N a)) => Vn a -> a -> a -> a
juxtaposeByTrace v a1 a2 =
  case (mv1, mv2) of
    (Just v1, Just v2) -> moveOriginBy (v1 ^+^ v2) a2
    _                  -> a2
  where mv1 = negated <$> maxTraceV origin v a1
        mv2 = maxTraceV origin (negated v) a2