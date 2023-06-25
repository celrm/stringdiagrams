{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module BrickDiagrams where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data BrickDiagram = Crossing Double (Maybe (Double -> Double)) | Morphism Arity String | Compose BrickDiagram BrickDiagram | Tensor BrickDiagram BrickDiagram
type Arity = (Double, Double)

data Paths = Paths { _bd :: Path V2 Double, _sd :: Path V2 Double }
data Locatables = Locs { _texts :: [Located (Diagram B)], _boxes :: [Located (Diagram B)] }
data Properties = Props { _arity :: Arity, _dw :: Double, _warnings :: String }
data CustomDiagram = CD { _ps :: Paths, _ls :: Locatables, _pp :: Properties }
$(makeLenses ''Paths)
$(makeLenses ''Locatables)
$(makeLenses ''Properties)
$(makeLenses ''CustomDiagram)

instance Semigroup Paths where
    (<>) ps1 ps2 = ps1 # over bd (<> ps2^.bd) # over sd (<> ps2^.sd)

instance Semigroup Locatables where
    (<>) ls1 ls2 = ls1 # over texts (<> ls2^.texts) # over boxes (<> ls2^.boxes)

drawCubic :: (R1 v, Metric v, Fractional n) => Point v n -> Point v n -> FixedSegment v n
drawCubic o f = FCubic o (c o f) (c f o) f
    where c x y = x .+^ (0.5 *^ project unitX (y .-. x))

-- from Diagrams.Deform (slightly changed)
approx :: (R1 u, Metric u, Fractional n) => Deformation v u n -> FixedSegment v n -> FixedSegment u n
approx t (FLinear p0 p1)      = FLinear (deform t p0) (deform t p1)
approx t (FCubic p0 _ _ p1) = drawCubic (deform t p0) (deform t p1)

-- This is a custom "deformation" for paths such that only the endpoints (and control points) are moved
deformPath :: (Metric v, Metric u, R1 u, OrderedField n) => Deformation v u n -> Path v n -> Path u n
deformPath t = toPath . map (map (approx t . mkFixedSeg)) . pathLocSegments

-- This is a custom "deformation" for Located objects such that only the origin is moved
deformLoc :: Deformation (V a) (V a) (N a) -> Located a -> Located a
deformLoc t (Loc o s) = Loc (deform t o) s

-- Deforms a Custom Diagram (its paths and its located objects)
deformCD :: Deformation V2 V2 Double -> CustomDiagram -> CustomDiagram
deformCD t = over (ps . bd) (deformPath t) . over (ps . sd) (deformPath t)
    . over (ls . texts . traverse) (deformLoc t) . over (ls . boxes . traverse) (deformLoc t)

-- A type of deformCD that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinchCD :: Double -> CustomDiagram -> CustomDiagram
pinchCD k cd = cd # deformCD (Deformation $ \pt -> pt # scaleY ((m' * pt^._x + n') / (m * pt^._x + n)))
    where 
        (al,ar) = cd^.pp.arity
        w = cd^.pp.dw
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

-- A type of deformCD that translates the origin to the furthest boundary in v's direction
alignCD :: V2 Double -> CustomDiagram -> CustomDiagram
alignCD v cd = cd # deformCD (Deformation $ moveOriginTo newOrigin)
    where Just newOrigin = maxRayTraceP origin v $ cd^.ps.bd

joinTwoCD :: (Arity -> Arity -> Arity) -> (Double -> Double -> Double) -> CustomDiagram -> CustomDiagram -> CustomDiagram
joinTwoCD farity fwidth cd1 cd2 = CD
    { _ps = cd1^.ps <> cd2^.ps, _ls = cd1^.ls <> cd2^.ls -- Semigroup instances
    , _pp = Props -- This depends on composition/tensoring
        { _arity = farity (cd1^.pp.arity) (cd2^.pp.arity)
        , _dw = fwidth (cd1^.pp.dw) (cd2^.pp.dw)
        , _warnings = cd1^.pp.warnings ++ cd2^.pp.warnings } }

-- The main constructor of CustomDiagrams
brickToCustom :: BrickDiagram -> CustomDiagram

-- Only for SD (for BD it gets an empty box)
brickToCustom (Crossing k mf) = 
    brickToCustom (Morphism (k,k) "") 
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where
        pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+f i)) | i <-[0..k-1]]
        f = case mf of 
                Nothing -> \i -> k-i-1
                Just jf -> jf

-- A square, pinched in both upper vertices according to its arity
brickToCustom (Morphism (al,ar) s) = CD
    { _ps = Paths 
        { _bd = unitSquare # alignBL
        , _sd = toPath (ptsl++ptsr) }
    , _ls = Locs 
        { _texts = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))]
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    , _pp = Props 
        { _arity = (1,1), _dw = 1, _warnings = [] } } 
    # pinchCD (-al) # set (pp . arity) (al,1) 
    # pinchCD ar # set (pp . arity) (al,ar)
    where 
        ctr = 0.5 ^& 0.5
        ptsl = [drawCubic ctr (0 ^& ((0.5+i)/al)) | i <-[0..al-1]]
        ptsr = [drawCubic ctr (1 ^& ((0.5+i)/ar)) | i <-[0..ar-1]]

-- Composing 2 CustomDiagrams
brickToCustom (Compose bd1 bd2) = 
    joinTwoCD farity (+) ncd1 ncd2
    # alignCD (-unitX)
    # set (pp . warnings) wng
    where
        [cd1,cd2] = map brickToCustom [bd1,bd2]
        middle = (fst (cd1^.pp.arity) + snd (cd2^.pp.arity))/2
        ncd1 = cd1 # pinchCD middle # alignCD unitX
        ncd2 = cd2 # pinchCD (-middle)
        farity (al1,_) (_,ar2) = (al1,ar2)
        wng = if abs (snd (cd1^.pp.arity) - fst (cd2^.pp.arity)) < 0.001 then 
                "Error while composing" else "" -- TODO give more detail

-- Tensoring 2 CustomDiagrams
brickToCustom (Tensor bd1 bd2) = 
    joinTwoCD farity max ncd1 ncd2
    # alignCD (-unitY)
    where
        [cd1,cd2] = map brickToCustom [bd1,bd2]
        [w1,w2] = view (pp.dw) <$> [cd1,cd2]
        maxWidth = max w1 w2
        ncd1 = cd1 # deformCD (Deformation 
            $ scaleX (w1/maxWidth) 
            . shearY ((snd (cd2^.pp.arity)-fst (cd2^.pp.arity))/maxWidth))
        ncd2 = cd2 # deformCD (Deformation
            $ scaleX (w2/maxWidth))
            # alignCD unitY
        farity (al1,ar1) (al2,ar2) = (al1+al2,ar1+ar2)

scaleXCD :: Double -> CustomDiagram -> CustomDiagram
scaleXCD k cd = cd 
    # deformCD (Deformation $ scaleX k)
    # over (pp . dw) (* k)

scaleYCD :: Double -> CustomDiagram -> CustomDiagram
scaleYCD k cd = cd 
    # deformCD (Deformation $ scaleY k)
    # over (pp . arity) (\(al,ar) -> (k*al,k*ar))

scaleCD :: Double -> CustomDiagram -> CustomDiagram
scaleCD k = (scaleXCD k) . (scaleYCD k)

rectangify :: CustomDiagram -> CustomDiagram
rectangify cd = cd 
    # pinchCD (-maxArity) # set (pp.arity) (maxArity,ar) 
    # pinchCD maxArity # set (pp.arity) (maxArity,maxArity)
    where
        (al,ar) = cd^.pp.arity
        maxArity = max al ar

squarify :: CustomDiagram -> CustomDiagram
squarify cd = cd
    # rectangify
    # scaleXCD (maxArity/(cd^.pp.dw))
    where
        (al,ar) = cd^.pp.arity
        maxArity = max al ar

-- Put together a CustomDiagram into a Diagram B
customToDiagram :: String -> CustomDiagram -> Diagram B
customToDiagram tp cd = mconcat diagrams
    where
        ncd = cd # rectangify
        diagrams = [moveOriginTo (-o) s | (Loc o s) <- ncd^.ls.texts]
            ++ (if tp/="sd" then [ncd^.ps.bd # strokePath] else [])
            ++ (if tp/="bd" then [moveOriginTo (-o) s | (Loc o s) <- ncd^.ls.boxes]
            ++ [ncd^.ps.sd # strokePath] else [])

-- From BrickDiagram to Diagram B directly (BD format)
drawBrickDiagram :: BrickDiagram -> Diagram B
drawBrickDiagram = (customToDiagram "bd") . brickToCustom

-- From BrickDiagram to Diagram B directly (SD format)
drawStringDiagram :: BrickDiagram -> Diagram B
drawStringDiagram = (customToDiagram "sd") . brickToCustom