{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module BrickDiagrams where

import Diagrams.Prelude

data BrickDiagram = Morphism Arity | Compose BrickDiagram BrickDiagram | Tensor BrickDiagram BrickDiagram
type Arity = (Integer, Integer)
        
changeAllPoints :: (P2 Double -> P2 Double) -> Path V2 Double -> Path V2 Double
changeAllPoints newP path = path
    # pathLocSegments
    # map (map mkFixedSeg)
    # map (map (\(FLinear point1 point2) -> FLinear (newP point1) (newP point2)))
    # toPath

pinch :: Double -> Arity -> Path V2 Double -> Path V2 Double
pinch k (al',ar') path = path
    # changeAllPoints (\p -> (p^._x) ^& ((p^._y) * (m' * (p^._x) + n') / (m * (p^._x) + n)))
    where 
        [w,al,ar] = width path : map fromInteger [al',ar']
        lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineEquation (0,al) (w,ar)
        (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

drawPath :: BrickDiagram -> (Path V2 Double, Arity)
drawPath (Morphism (al',ar')) = 
    (unitSquare # alignBL # pinch (-al) (1,1) # pinch ar (al',1),
    (al',ar')) where [al,ar] = map fromInteger [al',ar']

drawPath (Compose bd1 bd2) =
    (path1 # pinch middle a1 
    ||| path2 # pinch (-middle) a2,
    (al1',ar2'))
    where
        (path1, a1@(al1',ar1')) = drawPath bd1
        (path2, a2@(al2',ar2')) = drawPath bd2
        middle = (al1+ar2)/2 where [al1,ar2] = map fromInteger [al1',ar2']

drawPath (Tensor bd1 bd2) =
    (alignB $ 
    path1 # scaleX (maxWidth/w1) # shearY ((ar2-al2)/maxWidth)
    <> path2 # scaleX (maxWidth/w2) # alignT,
    (al1'+al2',ar1'+ar2'))
    where
        (path1,(al1',ar1')) = drawPath bd1
        (path2,(al2',ar2')) = drawPath bd2
        [w1,w2,al2,ar2] = map width [path1,path2] ++ map fromInteger [al2',ar2']
        maxWidth = max w1 w2
