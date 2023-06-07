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
    (d1 # scaleX 0.5 ||| d2 # scaleX 0.5) # centerXY, 
    (in1,out2))
    where
        (d1,(in1,out1)) = drawDiagram sd1
        (d2,(in2,out2)) = drawDiagram sd2

drawDiagram (Tensor sd1 sd2) =
    (unitSquare # opacity 0 <> 
    (d1 # scaleY 0.5 === d2 # scaleY 0.5) # centerXY, 
    (in1 + in2, out1 + out2))
    where
        (d1,(in1,out1)) = drawDiagram sd1
        (d2,(in2,out2)) = drawDiagram sd2
        
example = Tensor (Compose Morphism Morphism) (Tensor Morphism Morphism)

mainDiagram :: Diagram B
mainDiagram = unitSquare <> (fst $ drawDiagram example)

main = mainWith mainDiagram