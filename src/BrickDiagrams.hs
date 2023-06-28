{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module BrickDiagrams (
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

import Safe (atMay)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import StringDiagrams.Read (InputDiagram(..),Arity)

------------------------------------------------------------
--  OutputDiagram type  ------------------------------------
------------------------------------------------------------

-- | A custom object made up of: 
--   1) paths, which are modified only on their end or control points (brick or string diagrams)
--   2) located objects, which are modified only on their origin point (text labels or SD boxes)
--   3) properties: the arity, the width, and the composition warnings

data Paths = Paths { _bd :: Path V2 Double, _sd :: Path V2 Double }
data Locatables = Locs { _texts :: [Located (Diagram B)], _boxes :: [Located (Diagram B)] }
data Properties = Props { _arity :: Arity, _dw :: Double, _warnings :: String }
data OutputDiagram = OD { _ps :: Paths, _ls :: Locatables, _pp :: Properties }
$(makeLenses ''Paths)
$(makeLenses ''Locatables)
$(makeLenses ''Properties)
$(makeLenses ''OutputDiagram)

instance Semigroup Paths where
    (<>) ps1 ps2 = ps1 # over bd (<> ps2^.bd) # over sd (<> ps2^.sd)

instance Semigroup Locatables where
    (<>) ls1 ls2 = ls1 # over texts (<> ls2^.texts) # over boxes (<> ls2^.boxes)

------------------------------------------------------------
--  Deforming OutputDiagram (internal)  --------------------
------------------------------------------------------------

-- This is a custom "deformation" for paths such that only the endpoints (and control points) are moved
deformPath :: (Metric v, Metric u, R1 u, OrderedField n) => Deformation v u n -> Path v n -> Path u n
deformPath t = toPath . map (map (approx t . mkFixedSeg)) . pathLocSegments

-- from Diagrams.Deform (slightly changed)
approx :: (R1 u, Metric u, Fractional n) => Deformation v u n -> FixedSegment v n -> FixedSegment u n
approx t (FLinear p0 p1)      = FLinear (deform t p0) (deform t p1)
approx t (FCubic p0 _ _ p1) = drawCubic (deform t p0) (deform t p1)

-- Chooses control points in the same Y-coord than the endpoints
drawCubic :: (R1 v, Metric v, Fractional n) => Point v n -> Point v n -> FixedSegment v n
drawCubic o f = FCubic o (c o f) (c f o) f
    where c x y = x .+^ (0.5 *^ project unitX (y .-. x))

-- This is a custom "deformation" for Located objects such that only the origin is moved
deformLoc :: Deformation (V a) (V a) (N a) -> Located a -> Located a
deformLoc t (Loc o s) = Loc (deform t o) s

-- Deforms an OutputDiagram (its paths and its located objects)
deformOD :: Deformation V2 V2 Double -> OutputDiagram -> OutputDiagram
deformOD t = over (ps . bd) (deformPath t) . over (ps . sd) (deformPath t)
    . over (ls . texts . traverse) (deformLoc t) . over (ls . boxes . traverse) (deformLoc t)

-- A type of deformOD that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinchOD :: Double -> OutputDiagram -> OutputDiagram
pinchOD k od = od # deformOD (Deformation $ \pt -> pt # scaleY ((m' * pt^._x + n') / (m * pt^._x + n)))
    where 
        (al,ar) = od^.pp.arity
        w = od^.pp.dw
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

-- A type of deformOD that translates the origin to the furthest boundary in v's direction
alignOD :: V2 Double -> OutputDiagram -> OutputDiagram
alignOD v od = od # deformOD (Deformation $ moveOriginTo newOrigin)
    where Just newOrigin = maxRayTraceP origin v $ od^.ps.bd

------------------------------------------------------------
--  Constructing OutputDiagram  ----------------------------
------------------------------------------------------------
 
joinTwoOD :: (Arity -> Arity -> Arity) -> (Double -> Double -> Double) -> OutputDiagram -> OutputDiagram -> OutputDiagram
joinTwoOD farity fwidth od1 od2 = OD
    { _ps = od1^.ps <> od2^.ps, _ls = od1^.ls <> od2^.ls -- Semigroup instances
    , _pp = Props -- This depends on composition/tensoring
        { _arity = farity (od1^.pp.arity) (od2^.pp.arity)
        , _dw = fwidth (od1^.pp.dw) (od2^.pp.dw)
        , _warnings = od1^.pp.warnings ++ od2^.pp.warnings } }

-- The main constructor of OutputDiagram
inputToOutput :: InputDiagram -> OutputDiagram

-- Only for SD (for BD it gets an empty box)
inputToOutput (Crossing k mf) = 
    inputToOutput (Morphism (k,k) "") 
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where
        pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+f i)) | i <-[0..k-1]]
        f i = fromIntegral $ maybe 0 id $ mf >>= \ys -> ys `atMay` n where n = floor i

-- A square, pinched in both upper vertices according to its arity
inputToOutput (Morphism (al,ar) s) = OD
    { _ps = Paths 
        { _bd = unitSquare # alignBL
        , _sd = toPath (ptsl++ptsr) }
    , _ls = Locs 
        { _texts = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    , _pp = Props 
        { _arity = (1,1), _dw = 1, _warnings = [] } } 
    # pinchOD (-al) # set (pp . arity) (al,1) 
    # pinchOD ar # set (pp . arity) (al,ar)
    where 
        ctr = 0.5 ^& 0.5
        ptsl = [drawCubic ctr (0 ^& ((0.5+i)/al)) | i <-[0..al-1]]
        ptsr = [drawCubic ctr (1 ^& ((0.5+i)/ar)) | i <-[0..ar-1]]

-- Composing 2 CustomDiagrams
inputToOutput (Compose bd1 bd2) = 
    joinTwoOD farity (+) nod1 nod2
    # alignOD (-unitX)
    # set (pp . warnings) wng
    where
        [od1,od2] = map inputToOutput [bd1,bd2]
        middle = (fst (od1^.pp.arity) + snd (od2^.pp.arity))/2
        nod1 = od1 # pinchOD middle # alignOD unitX
        nod2 = od2 # pinchOD (-middle)
        farity (al1,_) (_,ar2) = (al1,ar2)
        wng = if abs (snd (od1^.pp.arity) - fst (od2^.pp.arity)) < 0.001 then 
                "Error while composing" else "" -- TODO give more detail

-- Tensoring 2 CustomDiagrams
inputToOutput (Tensor bd1 bd2) = 
    joinTwoOD farity max nod1 nod2
    # alignOD (-unitY)
    where
        [od1,od2] = map inputToOutput [bd1,bd2]
        [w1,w2] = view (pp.dw) <$> [od1,od2]
        maxWidth = max w1 w2
        nod1 = od1 # deformOD (Deformation 
            $ scaleX (w1/maxWidth) 
            . shearY ((snd (od2^.pp.arity)-fst (od2^.pp.arity))/maxWidth))
        nod2 = od2 # deformOD (Deformation
            $ scaleX (w2/maxWidth))
            # alignOD unitY
        farity (al1,ar1) (al2,ar2) = (al1+al2,ar1+ar2)

------------------------------------------------------------
--  Drawing OutputDiagram  ---------------------------------
------------------------------------------------------------

-- Put together a OutputDiagram into a Diagram B
outputToDiagram :: String -> OutputDiagram -> Diagram B
outputToDiagram tp od = mconcat diagrams
    where
        nod = od # isoscelify
        diagrams = [moveOriginTo (-o) s | (Loc o s) <- nod^.ls.texts]
            ++ (if tp/="sd" then [nod^.ps.bd # strokePath] else [])
            ++ (if tp/="bd" then [moveOriginTo (-o) s | (Loc o s) <- nod^.ls.boxes]
            ++ [nod^.ps.sd # strokePath] else [])

-- From InputDiagram to Diagram B directly (BD format)
drawBrickDiagram :: InputDiagram -> Diagram B
drawBrickDiagram = (outputToDiagram "bd") . inputToOutput

-- From InputDiagram to Diagram B directly (SD format)
drawStringDiagram :: InputDiagram -> Diagram B
drawStringDiagram = (outputToDiagram "sd") . inputToOutput

------------------------------------------------------------
--  Deforming OutputDiagram (external)  --------------------
------------------------------------------------------------

scaleXOD :: Double -> OutputDiagram -> OutputDiagram
scaleXOD k = 
    deformOD (Deformation $ scaleX k)
    . over (pp . dw) (* k)

scaleYOD :: Double -> OutputDiagram -> OutputDiagram
scaleYOD k = 
    deformOD (Deformation $ scaleY k)
    . over (pp . arity) (\(al,ar) -> (k*al,k*ar))

scaleOD :: Double -> OutputDiagram -> OutputDiagram
scaleOD k = (scaleXOD k) . (scaleYOD k)

rectangify :: OutputDiagram -> OutputDiagram
rectangify od = od 
    # pinchOD (-maxArity) # set (pp.arity) (maxArity,ar) 
    # pinchOD maxArity # set (pp.arity) (maxArity,maxArity)
    where
        (al,ar) = od^.pp.arity
        maxArity = max al ar

squarify :: OutputDiagram -> OutputDiagram
squarify od = od
    # rectangify
    # scaleXOD (maxArity/(od^.pp.dw))
    where
        (al,ar) = od^.pp.arity
        maxArity = max al ar

isoscelify :: OutputDiagram -> OutputDiagram
isoscelify od = od # deformOD (Deformation $ shearY sh)
    where sh = (fst (od^.pp.arity)-snd (od^.pp.arity))/(2*(od^.pp.dw))