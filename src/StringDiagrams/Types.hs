{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module StringDiagrams.Types (
    InputDiagram,
    BlockType(..),
    Arity, NamedArity,
    OutputDiagram,
    arity,
    pinch,
    drawMorphism,
    drawMorphismWNames,
    drawCrossing,
    drawCrossingWNames,
    outputToStringDiagram,
    outputToBrickDiagram,
) where

import Data.Tree ( Tree )
import Data.List ( nubBy )

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )

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
    | Crossing Double [Int]
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

------------------------------------------------------------
--  Utility functions  -------------------------------------
------------------------------------------------------------

-- Finds the OutputDiagram's arity
arity :: OutputDiagram -> Arity
arity od = (od # findHeight, od # alignR # findHeight)
    where findHeight = maybe 0 (^._y) . maxRayTraceP origin unitY

-- Deformation that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinch :: Double -> OutputDiagram -> OutputDiagram
pinch k od = od # deform (Deformation $ \pt -> pt # scaleY ((m' * pt^._x + n') / (m * pt^._x + n)))
    where (al,ar) = od # arity
          w = od # width
          lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
          (m,n) = lineEquation (0,al) (w,ar)
          (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

------------------------------------------------------------
--  Base cases  --------------------------------------------
------------------------------------------------------------

getSidePoints :: Arity -> ([P2 Double],[P2 Double])
getSidePoints (al,ar) = (ptsl,ptsr)
    where ptsl = reverse [0 ^& (0.5+i) | i <-[0..al-1]]
          ptsr = reverse [1 ^& (0.5+i) | i <-[0..ar-1]]

drawMorphism :: Arity -> String -> OutputDiagram
drawMorphism (al,ar) s = OD
    { _ps = Paths { _bd = unitSquare # alignBL, _sd = cubics }
    , _ls = Locs
        { _labels = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    }
    # pinch (-al) # pinch ar
    where ctr = 0.5 ^& 0.5
          (sptsl,sptsr) = getSidePoints (al, ar)
          (ptsl,ptsr) = (sptsl # scaleY (1/al),sptsr # scaleY (1/ar))
          cubics = toPath . map (drawCubic ctr) $ (ptsl++ptsr)

drawMorphismWNames :: NamedArity -> String -> OutputDiagram
drawMorphismWNames (als,ars) s =
    drawMorphism (al, ar) s
    # over (ls . labels) (++ wireNames)
    where (al, ar) = ((fromIntegral . length) als, (fromIntegral . length) ars)
          (ptsl,ptsr) = getSidePoints (al, ar)
          funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
          wireNames = funct ptsl als ++ funct ptsr ars
          -- they get drawn twice

drawCrossing :: Double -> [Int] -> OutputDiagram
drawCrossing k mf =
    drawMorphism (k,k) ""
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+ applyPerm i)) | i <-[0..k-1]]
          applyPerm i = fromIntegral $ mf !! floor i


drawCrossingWNames :: [String] -> [Int] -> OutputDiagram
drawCrossingWNames ks mf =
    drawCrossing k mf
    # over (ls . labels) (++ wireNames)
    where k = (fromIntegral . length) ks
          (ptsl,ptsr) = getSidePoints (k, k)
          funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
          wireNames = funct ptsl ks ++ funct ptsr (map (ks !!) mf)

------------------------------------------------------------
--  Drawing (hiding the implementation)  -------------------
------------------------------------------------------------

-- Put together an OutputDiagram into a Diagram B
outputToStringDiagram :: OutputDiagram -> Diagram B
outputToStringDiagram od = mconcat
            -- adding nubBy to remove duplicate wire names
            $  [moveOriginTo (-o) s | (Loc o s) <- 
                    nubBy (\a b -> distance (loc a) (loc b) < 0.00001) (od^.ls.labels)]
            ++ [moveOriginTo (-o) s | (Loc o s) <- od^.ls.boxes]
            ++ [od^.ps.sd # strokePath]

-- Put together an OutputDiagram into a Diagram B
outputToBrickDiagram :: OutputDiagram -> Diagram B
outputToBrickDiagram od = mconcat
            $  [moveOriginTo (-o) s | (Loc o s) <- od^.ls.labels]
            ++ [od^.ps.bd # strokePath]