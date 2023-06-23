{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module BrickDiagrams where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data BrickDiagram = Morphism Arity String | Compose BrickDiagram BrickDiagram | Tensor BrickDiagram BrickDiagram
type Arity = (Double, Double)

data CustomDiagram = CD { _p :: Path V2 Double, _a :: Arity, _ls :: [Located (Diagram B)] }
$(makeLenses ''CustomDiagram)

-- from Diagrams.Deform
approx :: Deformation v u n -> FixedSegment v n -> FixedSegment u n
approx t (FLinear p0 p1)      = FLinear (deform t p0) (deform t p1)
approx t (FCubic p0 c1 c2 p1) = FCubic (deform t p0) (deform t c1) (deform t c2) (deform t p1)

-- This is a custom "deformation" for paths such that only the endpoints (and control points) are moved
deformPath :: (Metric v, Metric u, OrderedField n) => Deformation v u n -> Path v n -> Path u n
deformPath t = toPath . map (map (approx t . mkFixedSeg)) . pathLocSegments

-- This is a custom "deformation" for Located objects such that only the origin is moved
deformLoc :: Deformation (V a) (V a) (N a) -> Located a -> Located a
deformLoc t (Loc o s) = Loc (deform t o) s

-- Deforms a Custom Diagram (its path and its located objects)
deformCD :: Deformation V2 V2 Double -> CustomDiagram -> CustomDiagram
deformCD t = over p (deformPath t) . over (ls . traverse) (deformLoc t)

-- A type of deformCD that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinchCD :: Double -> CustomDiagram -> CustomDiagram
pinchCD k cd = deformCD (Deformation (\pt -> pt # scaleY ((m' * (pt^._x) + n') / (m * (pt^._x) + n)))) cd
    where 
        (al,ar) = view a $ cd
        w = width . view p $ cd
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

-- A type of deformCD that translates the origin to the furthest boundary in v's direction
alignCD :: V2 Double -> CustomDiagram -> CustomDiagram
alignCD v cd = deformCD (Deformation $ moveOriginTo newOrigin) cd
    where Just newOrigin = maxRayTraceP origin v $ cd ^. p

-- The main constructor of CustomDiagrams
drawCD :: BrickDiagram -> CustomDiagram

-- A square, pinched in both upper vertices, with added text
drawCD (Morphism (al,ar) s) = basicCD 
    # set ls [Loc (centerPoint (basicCD^.p)) (text s # fontSizeG 0.25)]
    # pinchCD (-al) # set a (al,1) 
    # pinchCD ar # set a (al,ar)
    where basicCD = CD { _p = unitSquare # alignBL, _a = (1,1), _ls = []}

-- Composing 2 CustomDiagrams
drawCD (Compose bd1 bd2) = CD
    { _p = ncd1^.p <> ncd2^.p
    , _a = (al, ar)
    , _ls = ncd1^.ls ++ ncd2^.ls}
    # alignCD (-unitX)
    where
        [cd1,cd2] = map drawCD [bd1,bd2]
        [al,ar] = [fst (cd1^.a), snd (cd2^.a)]
        middle = (al+ar)/2
        ncd1 = cd1 # pinchCD middle # alignCD unitX
        ncd2 = cd2 # pinchCD (-middle)

-- Tensoring 2 CustomDiagrams
drawCD (Tensor bd1 bd2) = CD
    { _p = ncd1^.p <> ncd2^.p
    , _a = (al1+al2, ar1+ar2)
    , _ls = ncd1^.ls ++ ncd2^.ls}
    # alignCD (-unitY)
    where
        [cd1,cd2] = map drawCD [bd1,bd2]
        [al1,ar1,al2,ar2] = [fst (cd1^.a),snd (cd1^.a),fst (cd2^.a),snd (cd2^.a)]
        
        [w1,w2] = width . view p <$> [cd1,cd2]
        maxWidth = max w1 w2
        ncd1 = cd1 # deformCD (Deformation $ scaleX (w1/maxWidth) . shearY ((ar2-al2)/maxWidth))
        ncd2 = cd2 # deformCD (Deformation $ scaleX (w2/maxWidth)) # alignCD unitY

-- Put together a CustomDiagram into a Diagram B
drawBrickDiagram :: BrickDiagram -> Diagram B
drawBrickDiagram bd = cd^.p # strokePath <> mconcat [moveOriginTo (-o) s | (Loc o s) <- cd^.ls]
    where cd = drawCD bd