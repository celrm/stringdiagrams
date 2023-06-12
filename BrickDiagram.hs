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

lineMN :: (Double,Double) -> (Double,Double) -> (Double,Double)
lineMN (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)

changeLine :: (P2 Double -> Bool) -> (P2 Double -> P2 Double) -> Path V2 Double -> Path V2 Double
changeLine ifOnLine moveToNew path = toPath moveSegments
    where 
        cond p = if ifOnLine p then moveToNew p else p
        getSegments = map (map mkFixedSeg) (pathLocSegments path)
        moveSegments = map (map (\(FLinear p1 p2) -> FLinear (cond p1) (cond p2))) getSegments

changeTopLine :: (Double,Double) -> (Path V2 Double, Arity) -> Path V2 Double
changeTopLine (m',n') (path, (al,ar)) = (top . left . right) path
    where 
        (m,n) = lineMN (0,fromInteger al) (width path,fromInteger ar)
        ---
        ifTop p = abs ((p^._y) - (m * (p^._x) + n)) <= 0.001
        moveTop p = (p^._x) ^& (m' * (p^._x) + n')
        top = changeLine ifTop moveTop
        ----
        ifLeft p = abs (p^._x) <= 0.001
        moveLeft p = (p^._x) ^& ((p^._y)*n'/n)
        left = changeLine ifLeft moveLeft
        ----
        ifRight p = abs ((p^._x) - width path) <= 0.001
        moveRight p = (p^._x) ^& ((p^._y)*(m' * (p^._x) + n')/(fromInteger ar))
        right = changeLine ifRight moveRight
        
drawPath :: BrickDiagram -> (Path V2 Double, Arity)
drawPath (Morphism (al,ar)) = 
    (changeTopLine 
        (lineMN (0,fromInteger al) (1,fromInteger ar)) 
        (unitSquare # alignBL, 
        (1,1)),
    (al,ar))

drawPath (Compose bd1 bd2) =
    (newP1 ||| newP2,
    (al1',ar2'))
    where
        d1@(p1,(al1',ar1')) = drawPath bd1
        d2@(p2,(al2',ar2')) = drawPath bd2
        [al1,ar1,al2,ar2] = map fromInteger [al1',ar1',al2',ar2']
        [w1,w2] = map width [p1,p2]
        middle = (al1+ar2)/2
        (newP1,newP2) = if middle > ar1 then moveMiddle else moveSides
        moveMiddle = 
            (changeTopLine (lineMN (0,al1) (w1,middle)) d1,
            changeTopLine (lineMN (0,middle) (w2,ar2)) d2)
        moveSides =
            (changeTopLine (lineMN (0,x*al1/ar2) (w2,ar1)) d1 # scaleY (ar2/x),
            changeTopLine (lineMN (0,ar1) (w2,x)) d2 # scaleY (ar2/x))
            where x = ar1 / (1 - w2*(1 - al1/ar2)/(w1+w2))

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
example = Compose (Tensor (Morphism (1, 1)) (Morphism (1, 2))) (Tensor (Morphism (2, 1)) (Morphism (1, 1)))

main = mainWith $ (path # strokePath :: Diagram B) # frame 1
    where (path, arity) = drawPath example