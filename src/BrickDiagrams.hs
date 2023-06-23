{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}

module BrickDiagrams where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data BrickDiagram = Morphism Arity String | Compose BrickDiagram BrickDiagram | Tensor BrickDiagram BrickDiagram
type Arity = (Double, Double)

data CustomDiagram = CD { _p :: Path V2 Double, _a :: Arity, _ts :: [Located (Diagram B)] }
$(makeLenses ''CustomDiagram)

-- This is a custom "deformation" for paths such that only the endpoints (and control points) are moved
deformPath :: (Metric v, Metric u, OrderedField n) => Deformation v u n -> Path v n -> Path u n
deformPath mv = toPath . map (map (approx mv . mkFixedSeg)) . pathLocSegments

-- from Diagrams.Deform
approx :: Deformation v u n -> FixedSegment v n -> FixedSegment u n
approx t (FLinear p0 p1)      = FLinear (deform t p0) (deform t p1)
approx t (FCubic p0 c1 c2 p1) = FCubic (deform t p0) (deform t c1) (deform t c2) (deform t p1)

-- This is a custom "deformation" for Located objects such that only the origin is moved
deformLoc :: Deformation (V a) (V a) (N a) -> Located a -> Located a
deformLoc t (Loc o s) = Loc (deform t o) s

deformCD :: Deformation V2 V2 Double -> CustomDiagram -> CustomDiagram
deformCD t = over p (deformPath t) . over (ts . traverse) (deformLoc t)

pinch :: Double -> CustomDiagram -> CustomDiagram
pinch k cd = deformCD mv cd
    where 
        (al,ar) = view a $ cd
        w = width . view p $ cd
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)
        mv = Deformation (\p -> p # scaleY ((m' * (p^._x) + n') / (m * (p^._x) + n)))

alignCD :: V2 Double -> CustomDiagram -> CustomDiagram
alignCD v cd = deformCD (Deformation (moveOriginTo newOrigin)) cd
    where Just newOrigin = maxRayTraceP origin v (cd^.p)

drawCD :: BrickDiagram -> CustomDiagram
drawCD (Morphism (al,ar) s) = basicCD 
    # set ts [Loc (centerPoint (basicCD^.p)) (text s # fontSizeG 0.25)]
    # pinch (-al) # set a (al,1) 
    # pinch ar # set a (al,ar)
    where basicCD = CD { _p = unitSquare # alignBL, _a = (1,1), _ts = []}

drawCD (Compose bd1 bd2) = CD
    { _p = ncd1^.p <> ncd2^.p
    , _a = (al,ar)
    , _ts = ncd1^.ts ++ ncd2^.ts} 
    where
        [cd1,cd2] = map drawCD [bd1,bd2]
        [al,ar] = [fst (cd1^.a),snd (cd2^.a)]
        middle = (al+ar)/2
        ncd1 = cd1 # pinch middle
        ncd2 = cd2 # pinch (-middle) # alignCD unitX

drawCD (Tensor bd1 bd2) = CD
    { _p = ncd1^.p <> ncd2^.p
    , _a = (al1+al2,ar1+ar2)
    , _ts = ncd1^.ts ++ ncd2^.ts} # alignCD (-unitY)
    where
        [cd1,cd2] = map drawCD [bd1,bd2]
        [al1, ar1, al2, ar2] = [fst (cd1^.a),snd (cd1^.a),fst (cd2^.a),snd (cd2^.a)]
        [w1,w2] = width . view p <$> [cd1, cd2]
        maxWidth = max w1 w2
        ncd1 = cd1 # deformCD (Deformation (\p -> p # scaleX (w1/maxWidth) # shearY ((ar2-al2)/maxWidth)))
        ncd2 = cd2 # deformCD (Deformation (\p -> p # scaleX (w2/maxWidth))) # alignCD unitY

drawBrickDiagram :: BrickDiagram -> Diagram B
drawBrickDiagram bd = path # strokePath <> mconcat [drawText t | t<-ts]
    where
        CD {_p=path, _a=_, _ts=ts} = drawCD bd
        drawText t = s # translate (r2 (unp2 p)) where (p,s) = viewLoc t