{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data StringDiagram = Object | Morphism | Compose StringDiagram StringDiagram | Tensor StringDiagram StringDiagram
type DataTransfer = (Integer,Integer)

drawDiagram :: StringDiagram -> (Diagram B, DataTransfer)
drawDiagram Object = 
    (unitSquare # opacity 0 <> 
    hrule 1, 
    (1,1))

drawDiagram Morphism = 
    (unitSquare # opacity 0 <> 
    (hrule 0.35 ||| rect 0.3 0.6 ||| hrule 0.35) # centerXY, 
    (1,1))

drawDiagram (Compose sd1 sd2) =
    (unitSquare # opacity 0 <> 
    (d1 # scale 0.5 ||| d2 # scale 0.5) # centerXY, 
    (in1,out2))
    where
        (d1,(in1,out1)) = drawDiagram sd1
        (d2,(in2,out2)) = drawDiagram sd2

drawDiagram (Tensor sd1 sd2) =
    (unitSquare # opacity 0 <> 
    d <> mconcat (extendLeft ++ extendRight), 
    (in1 + in2, out1 + out2))
    where
        (d1,(in1,out1)) = drawDiagram sd1
        (d2,(in2,out2)) = drawDiagram sd2
        d = (d1 # scale 0.5 === d2 # scale 0.5) # centerXY
        extendLeft = extendLines in1 in2 (-0.25) (-0.5)
        extendRight = extendLines out1 out2 0.25 0.5

extendLines :: Integer -> Integer -> Double -> Double -> [Diagram B]
extendLines i1 i2 xS xE = trails
    where
        n1 = fromInteger i1
        n2 = fromInteger i2
        startP = [ p2 (xS,0.5-i*0.5/(n1+1)) | i<-[1..n1] ] ++ [ p2 (xS,-i*0.5/(n2+1)) | i<-[1..n2] ]
        endP = [ p2 (xE,0.5-i/(n1+n2+1)) | i<-[1..n1+n2] ]
        trails = map (\(s,e) -> fromVertices [s,e]) (zip startP endP)
        
example :: StringDiagram
example = Tensor (Compose Morphism Morphism) (Tensor Morphism Morphism)

mainDiagram :: Diagram B
mainDiagram = unitSquare <> (fst $ drawDiagram example)

main = mainWith mainDiagram

-- TODOS
-- line width
-- box size
-- centerXY