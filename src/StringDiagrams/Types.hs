{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StringDiagrams.Types (
    InputDiagram,
    BlockType(..),
    Arity,
    OutputDiagram,
    arity,
    drawMorphism,
    drawCrossing,
    drawCubic,
    pinch,
    outputToStringDiagram,
    outputToBrickDiagram,
) where

import Data.Tree ( Tree )
import Safe ( atMay )
import Data.Maybe ( fromMaybe )
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )

------------------------------------------------------------
--  InputDiagram type  -------------------------------------
------------------------------------------------------------

-- | A binary tree-like structure where leaves are either boxes or string crossings
--   and internal nodes have either composition or tensoring.

-- data InputDiagram = 
--     Crossing Double (Maybe [Int]) 
--     | Morphism Arity String 
--     | Compose InputDiagram InputDiagram 
--     | Tensor InputDiagram InputDiagram

type Arity = (Double, Double)
data BlockType = 
    Crossing Double (Maybe [Int])
    | Morphism Arity String
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

data Locatables = Locs { _texts :: [Located (Diagram B)], _boxes :: [Located (Diagram B)] }
$(makeLenses ''Locatables)
type instance V Locatables = V2
type instance N Locatables = Double
instance Semigroup Locatables where
    (<>) ls1 ls2 = ls1 # over texts (<> ls2^.texts) # over boxes (<> ls2^.boxes)

data OutputDiagram = OD { _ps :: Paths, _ls :: Locatables }
$(makeLenses ''OutputDiagram)
type instance V OutputDiagram = V2
type instance N OutputDiagram = Double
instance Semigroup OutputDiagram where
    (<>) od1 od2 = od1 # over ps (<> od2^.ps) # over ls (<> od2^.ls)

-- Finds the OutputDiagram's arity
arity :: OutputDiagram -> Arity
arity od = (od # findHeight, od # align unitX # findHeight)
    where
        maybeFindLimit dd = maxRayTraceP origin unitY $ dd^.ps.bd
        findHeight dd = maybe 0 (^._y) (dd # maybeFindLimit)

------------------------------------------------------------
--  Deforming OutputDiagram (internal)  --------------------
------------------------------------------------------------

-- Chooses control points in the same Y-coord than the endpoints
drawCubic :: (R1 v, Metric v, Fractional n) => Point v n -> Point v n -> FixedSegment v n
drawCubic o f = FCubic o (c o f) (c f o) f
    where c x y = x .+^ (0.5 *^ project unitX (y .-. x))

-- This is a custom "deformation" for Paths such that only the endpoints (and control points) are moved
instance r ~ Paths => Deformable Paths r where
    deform' _ = deform
    deform t = over bd deformPath . over sd deformPath
        where
            -- do not want to make Paths, FixedSeg or Located as Deformable, but we could
            deformPath = toPath . map (map (deformFixedSeg . mkFixedSeg)) . pathLocSegments
            -- from Diagrams.Deform.approx (slightly changed)
            deformFixedSeg (FLinear p0 p1) = FLinear (deform t p0) (deform t p1)
            deformFixedSeg (FCubic p0 _ _ p1) = drawCubic (deform t p0) (deform t p1)

-- This is a custom "deformation" for Located objects such that only the origin is moved
instance r ~ Locatables => Deformable Locatables r where
    deform' _ = deform
    deform t = over (texts . traverse) deformLoc . over (boxes . traverse) deformLoc
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
--  Base cases  --------------------------------------------
------------------------------------------------------------

-- Deformation that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinch :: Double -> OutputDiagram -> OutputDiagram
pinch k od = od # deform (Deformation $ \pt -> pt # scaleY ((m' * pt^._x + n') / (m * pt^._x + n)))
    where
        (al,ar) = od # arity
        w = od # width
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

drawMorphism :: Arity -> String -> OutputDiagram
drawMorphism (al,ar) s = OD
    { _ps = Paths { _bd = unitSquare # alignBL, _sd = toPath (ptsl++ptsr) }
    , _ls = Locs
        { _texts = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    }
    # pinch (-al)
    # pinch ar
    where ctr = 0.5 ^& 0.5
          ptsl = [drawCubic ctr (0 ^& ((0.5+i)/al)) | i <-[0..al-1]]
          ptsr = [drawCubic ctr (1 ^& ((0.5+i)/ar)) | i <-[0..ar-1]]

drawCrossing :: Integral a => Double -> Maybe [a] -> OutputDiagram
drawCrossing k mf =
    drawMorphism (k,k) ""
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where
        pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+f i)) | i <-[0..k-1]]
        f i = fromIntegral $ fromMaybe 0 $ mf >>= \ys -> ys `atMay` n where n = floor i

-- Put together an OutputDiagram into a Diagram B
outputToStringDiagram :: OutputDiagram -> Diagram B
outputToStringDiagram od = mconcat 
            $  [moveOriginTo (-o) s | (Loc o s) <- od^.ls.texts]
            ++ [moveOriginTo (-o) s | (Loc o s) <- od^.ls.boxes]
            ++ [od^.ps.sd # strokePath]

-- Put together an OutputDiagram into a Diagram B
outputToBrickDiagram :: OutputDiagram -> Diagram B
outputToBrickDiagram od = mconcat 
            $  [moveOriginTo (-o) s | (Loc o s) <- od^.ls.texts]
            ++ [od^.ps.bd # strokePath]