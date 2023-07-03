{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StringDiagrams.Draw (
    OutputDiagram,
    inputToOutput,
    outputToDiagram,
    drawBrickDiagram,
    drawStringDiagram,
    scaleOD,
    scaleXOD,
    scaleYOD,
    rectangify,
    squarify,
    isoscelify
) where

import Safe ( atMay )
import Data.Maybe ( fromMaybe )
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )
import StringDiagrams.Read ( InputDiagram(..), Arity )

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
arity od = (od # findHeight, od # alignOD unitX # findHeight)
    where
        maybeFindLimit dd = maxRayTraceP origin unitY $ dd^.ps.bd
        findHeight dd = maybe 0 (^._y) (dd # maybeFindLimit)

-- Finds the OutputDiagram's width
width' :: OutputDiagram -> Double
width' od = od^.ps.bd # width

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

-- Deformation that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinchOD :: Double -> OutputDiagram -> OutputDiagram
pinchOD k od = od # deform (Deformation $ \pt -> pt # scaleY ((m' * pt^._x + n') / (m * pt^._x + n)))
    where
        (al,ar) = od # arity
        w = od # width'
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

-- Deformation that translates the origin to the furthest boundary in v's direction
alignOD :: V2 Double -> OutputDiagram -> OutputDiagram
alignOD v od = od # deform (Deformation $ moveOriginTo newOrigin)
    where Just newOrigin = maxRayTraceP origin v $ od^.ps.bd

------------------------------------------------------------
--  Constructing OutputDiagram  ----------------------------
------------------------------------------------------------

-- The main constructor of OutputDiagram
inputToOutput :: InputDiagram -> OutputDiagram
inputToOutput (Morphism (al,ar) s) = OD
    { _ps = Paths { _bd = unitSquare # alignBL, _sd = toPath (ptsl++ptsr) }
    , _ls = Locs
        { _texts = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    }
    # pinchOD (-al)
    # pinchOD ar
    where
        ctr = 0.5 ^& 0.5
        ptsl = [drawCubic ctr (0 ^& ((0.5+i)/al)) | i <-[0..al-1]]
        ptsr = [drawCubic ctr (1 ^& ((0.5+i)/ar)) | i <-[0..ar-1]]

-- Only for SD (for BD it gets an empty box)
inputToOutput (Crossing k mf) =
    inputToOutput (Morphism (k,k) "")
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where
        pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+f i)) | i <-[0..k-1]]
        f i = fromIntegral $ fromMaybe 0 $ mf >>= \ys -> ys `atMay` n where n = floor i

-- Composing 2 OutputDiagrams
inputToOutput (Compose bd1 bd2) = alignOD (-unitX) $ nod1 <> nod2
    where
        [od1,od2] = map inputToOutput [bd1,bd2]
        middle = (od1 # arity # fst + od2 # arity # snd)/2
        nod1 = od1 # pinchOD middle # alignOD unitX
        nod2 = od2 # pinchOD (-middle)

-- Tensoring 2 OutputDiagrams
inputToOutput (Tensor bd1 bd2) = alignOD (-unitY) $ nod1 <> nod2
    where
        [od1,od2] = map inputToOutput [bd1,bd2]
        [w1,w2] = width . view (ps.bd) <$> [od1,od2]
        mw = max w1 w2
        nod1 = od1 # deform (Deformation
            $ scaleX (w1/mw)
            . shearY ((od2 # arity # snd - od2 # arity # fst)/mw))
        nod2 = od2 # deform (Deformation
            $ scaleX (w2/mw))
            # alignOD unitY

------------------------------------------------------------
--  Drawing OutputDiagram  ---------------------------------
------------------------------------------------------------

-- Put together a OutputDiagram into a Diagram B
outputToDiagram :: String -> OutputDiagram -> Diagram B
outputToDiagram tp od = mconcat diagrams
    where
        nod = od # isoscelify
        diagrams = [moveOriginTo (-o) s | (Loc o s) <- nod^.ls.texts]
            ++ [moveOriginTo (- o) s    | tp /= "bd", (Loc o s) <- nod ^. ls . boxes]
            ++ [nod^.ps.sd # strokePath | tp /= "bd"]
            ++ [nod^.ps.bd # strokePath | tp /= "sd"]

-- From InputDiagram to Diagram B directly (BD format)
drawBrickDiagram :: InputDiagram -> Diagram B
drawBrickDiagram = outputToDiagram "bd" . inputToOutput

-- From InputDiagram to Diagram B directly (SD format)
drawStringDiagram :: InputDiagram -> Diagram B
drawStringDiagram = outputToDiagram "sd" . inputToOutput

------------------------------------------------------------
--  Deforming OutputDiagram (external)  --------------------
------------------------------------------------------------

scaleXOD :: Double -> OutputDiagram -> OutputDiagram
scaleXOD k =
    deform (Deformation $ scaleX k)

scaleYOD :: Double -> OutputDiagram -> OutputDiagram
scaleYOD k =
    deform (Deformation $ scaleY k)

scaleOD :: Double -> OutputDiagram -> OutputDiagram
scaleOD k = scaleXOD k . scaleYOD k

rectangify :: OutputDiagram -> OutputDiagram
rectangify od = od
    # pinchOD (-maxArity)
    # pinchOD maxArity
    where
        (al,ar) = od # arity
        maxArity = max al ar

squarify :: OutputDiagram -> OutputDiagram
squarify od = od
    # rectangify
    # scaleXOD (maxArity/(od^.ps.bd # width))
    where
        (al,ar) = od # arity
        maxArity = max al ar

isoscelify :: OutputDiagram -> OutputDiagram
isoscelify od = od # deform (Deformation $ shearY sh)
    where sh = (od # arity # fst - od # arity # snd)/(2*(od^.ps.bd # width))