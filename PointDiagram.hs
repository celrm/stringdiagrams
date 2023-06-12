{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List

data StringDiagram = Object | Morphism | Compose StringDiagram StringDiagram | Tensor StringDiagram StringDiagram

dhw :: StringDiagram -> [Integer]
dhw Object = [1,1,1]
dhw Morphism = [1,1,1]
dhw (Compose a b) = [1 + m1, m2, 1 + m3] 
    where [m1,m2,m3] = zipWith max (dhw a) (dhw b)
dhw (Tensor a b) = [1 + m1, 1 + m2, m3] 
    where [m1,m2,m3] = zipWith max (dhw a) (dhw b)

boxPositions :: StringDiagram -> ([(Double,Double)], [(Double,Double)], [(Double,Double)], [[(Double,Double)]])
boxPositions Object = ([],[],[],[])

boxPositions Morphism = ([(0,0)],[(0,0)],[(0,0)],[])

boxPositions (Compose sd1 sd2) =
    (a1++a2,l1,r2,t1++t2++transpose [l2, r1])
    where
        (a1o,l1o,r1o,t1o) = boxPositions sd1
        (a2o,l2o,r2o,t2o) = boxPositions sd2
        [a1,l1,r1] = map (map (\(x,y) -> (0.5*x-0.25,y))) [a1o,l1o,r1o]
        t1 = map (map (\(x,y) -> (0.5*x-0.25,y))) t1o
        [a2,l2,r2] = map (map (\(x,y) -> (-0.5*x+0.25,y))) [a2o,l2o,r2o]
        t2 = map (map (\(x,y) -> (-0.5*x+0.25,y))) t2o

boxPositions (Tensor sd1 sd2) =
    (a1++a2,l1++l2,r1++r2,t1++t2)
    where
        (a1o,l1o,r1o,t1o) = boxPositions sd1
        (a2o,l2o,r2o,t2o) = boxPositions sd2
        [a1,l1,r1] = map (map (\(x,y) -> (x,-0.5*y+0.25))) [a1o,l1o,r1o]
        t1 = map (map (\(x,y) -> (x,-0.5*y+0.25))) t1o
        [a2,l2,r2] = map (map (\(x,y) -> (x,0.5*y-0.25))) [a2o,l2o,r2o]
        t2 = map (map (\(x,y) -> (x,0.5*y-0.25))) t2o

drawDiagram :: StringDiagram -> Diagram B
drawDiagram sd =
    mconcat $
    (map (\(x,y) -> rect size (size*2) # fc white # translate (x ^& y)) b) --  
    ++ (map (fromVertices . map p2) t)
    ++ (map (\(x,y) -> fromVertices [p2 (x,y),p2 (-0.5,y)]) bl)
    ++ (map (\(x,y) -> fromVertices [p2 (x,y),p2 (0.5,y)]) br)
    -- ++ (map (\(x,y) -> circle 0.01 # fc black # translate (x ^& y)) bl)
    where
        (b,bl,br,t) = boxPositions sd
        [d,h,w] = dhw sd
        size = 0.33 / ((fromInteger d)+1) -- what size

example :: StringDiagram
example = Compose
    (Tensor 
    (Compose (Tensor Morphism Morphism) (Tensor Morphism Morphism)) 
    (Compose Morphism Morphism))
    (Tensor 
    (Compose Morphism Morphism)
    (Compose (Tensor Morphism Morphism) (Tensor Morphism Morphism)) )

mainDiagram :: Diagram B
mainDiagram = unitSquare <> drawDiagram example

main = mainWith mainDiagram