{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.TwoD.Align
-- import Diagrams.Path
-- import Diagrams.Segment

data BrickDiagram = Morphism Arity | Compose BrickDiagram BrickDiagram | Tensor BrickDiagram BrickDiagram
type Arity = (Integer, Integer)

changeLine :: (P2 Double -> Double) -> (P2 Double -> Double) -> Path V2 Double -> Path V2 Double
changeLine ifOnLine newpY path = path
    # pathLocSegments
    # map (map mkFixedSeg)
    # map (map (\(FLinear p1 p2) -> FLinear (cond p1) (cond p2)))
    # toPath
    where cond p = if abs (ifOnLine p) <= 0.001 then (p^._x) ^& (newpY p) else p

pinch :: Double -> Arity -> Path V2 Double -> Path V2 Double
pinch k (al',ar') path = path
    # changeLine ifLeft moveLeft 
    # changeLine ifRight moveRight
    # changeLine ifTop moveTop
    where 
        [w,al,ar] = width path : map fromInteger [al',ar']
        lineMN (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
        (m,n) = lineMN (0,al) (w,ar)
        (m',n') = if k<0 then lineMN (0,-k) (w,ar) else lineMN (0,al) (w,k)
        ifTop p = (p^._y) - (m * (p^._x) + n)
        moveTop p = m' * (p^._x) + n'
        ifLeft p = (p^._x) - 0
        moveLeft p = (p^._y) * n' / n
        ifRight p = (p^._x) - w
        moveRight p = (p^._y) * (m' * (p^._x) + n') / ar
        
drawPath :: BrickDiagram -> (Path V2 Double, Arity)
drawPath (Morphism (al',ar')) = 
    (unitSquare # alignBL # pinch (-al) (1,1) # pinch ar (al',1),
    (al',ar')) where [al,ar] = map fromInteger [al',ar']

drawPath (Compose bd1 bd2) =
    (newP1 ||| newP2, (al1',ar2'))
    where
        (p1, a1@(al1',ar1')) = drawPath bd1
        (p2, a2@(al2',ar2')) = drawPath bd2
        [al1,ar1,al2,ar2] = map fromInteger [al1',ar1',al2',ar2']
        middle = (al1+ar2)/2
        (newP1,newP2) = if middle > ar1 then moveMiddle else moveSides
        moveMiddle = (pinch middle a1 p1, pinch (-middle) a2 p2)
        moveSides =
            (p1 # pinch (-x*al1/ar2) a1 # scaleY (ar2/x),
            p2 # pinch x a2 # scaleY (ar2/x))
            where x = ar1 / (1 - (width p2)*(1 - al1/ar2)/(width p1+width p2))

drawPath (Tensor bd1 bd2) =
    ((p1 # scaleX (maxW/w1) # shearY ((ar2-al2)/maxW) 
    <> p2 # scaleX (maxW/w2) # snugT) # alignBL,
    (al1'+al2',ar1'+ar2'))
    where
        (p1,(al1',ar1')) = drawPath bd1
        (p2,(al2',ar2')) = drawPath bd2
        [al1,ar1,al2,ar2] = map fromInteger [al1',ar1',al2',ar2']
        [w1,w2] = map width [p1,p2]
        maxW = max w1 w2 -- could take min?

example :: BrickDiagram
example = Compose (Tensor (Morphism (1, 2)) (Morphism (1, 1))) (Tensor (Morphism (1, 1)) (Morphism (2, 1)))

main = mainWith $ (path # strokePath :: Diagram B) # frame 1
    where (path, arity) = drawPath example