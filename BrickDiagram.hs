{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Align

data BrickDiagram = Morphism Arity | Compose BrickDiagram BrickDiagram | Tensor BrickDiagram BrickDiagram
type Arity = (Integer, Integer)

drawDiagram :: BrickDiagram -> (Diagram B, Arity)

drawDiagram (Morphism (aL, aR)) = (unitSquare, (aL, aR))

drawDiagram (Compose sd1 sd2) =
    ((d1 # scaleX 0.5 ||| d2 # scaleX 0.5) # centerXY,
    (a1L,a2R))
    where
        (d1,(a1L,a1R)) = drawDiagram sd1
        (d2,(a2L,a2R)) = drawDiagram sd2

drawDiagram (Tensor sd1 sd2) =
    ((d1 # scaleY (fromInteger aL) ||| d2 # scaleX (fromInteger aR)) # centerXY,
    (a1L,a2R))
    where
        (d1,(a1L,a1R)) = drawDiagram sd1
        (d2,(a2L,a2R)) = drawDiagram sd2
        (aL,aR) = (a1L+a2L,a1R+a2R)

extendLines :: Integer -> Integer -> Double -> Double -> [Diagram B]
extendLines i1 i2 xS xE = trails
    where
        n1 = fromInteger i1
        n2 = fromInteger i2
        startP = [ p2 (xS,0.5-i*0.5/(n1+1)) | i<-[1..n1] ] ++ [ p2 (xS,-i*0.5/(n2+1)) | i<-[1..n2] ]
        endP = [ p2 (xE,0.5-i/(n1+n2+1)) | i<-[1..n1+n2] ]
        trails = map (\(s,e) -> fromVertices [s,e]) (zip startP endP)
        
example :: BrickDiagram
example = Compose (Tensor (Morphism (1, 1)) (Morphism (1, 2))) (Tensor (Morphism (2, 1)) (Morphism (1, 1)))

data Side = LeftSide | RightSide
-- converts square into hill-like quadrilateral
-- f is stretch factor.
wibble :: Side -> Double -> Deformation V2 V2 Double
wibble LeftSide f = Deformation $ \p ->
  (p^._x) ^& ((p^._y)+(p^._y)*(p^._x-1)*(1-f))
wibble RightSide f = Deformation $ \p ->
  (p^._x) ^& ((p^._y)*((p^._x)*x_0 + (1-(p^._x))*x_1))
    where 
        x_0 = 1
        x_1 = f


-- mylines :: Path V2 Double
-- mylines = mconcat $ [p2 (i,0) ~~ p2 (i,1) | i<-[0,0.1..1]] # alignBL
mylines :: Path V2 Double
mylines = (mconcat $ [square i | i<-[0.1,0.2..1]]) # alignBL

quad :: Side -> Double -> Path V2 Double
quad x f = (mylines # alignBL :: Path V2 Double) # deform (wibble x f)

tensor :: Diagram B
tensor = (ff1) # alignBL # strokeP # showOrigin
    where 
        f1 = quad LeftSide 0.5
        ff1 = f1 # deform (wibble LeftSide 2)
        (d1,(al1,ar1)) = (quad LeftSide 2,(2,1)) -- 
        (d2,(al2,ar2)) = (quad RightSide 2,(1,2)) --
        leftDiagram = d1 # shearY (ar2-al2)
        rightDiagram = d2 # translateY (-al2)

compose :: Diagram B
compose = (leftDiagram ||| rightDiagram) # alignBL # strokeP # showOrigin
    where 
        (d1,(al1,ar1)) = (quad LeftSide 3,(3,1)) -- 
        (d2,(al2,ar2)) = (quad RightSide 1,(1,1)) --
        middle = (al1+ar2)/(ar1*2)
        leftDiagram = d1 # deform (wibble RightSide (middle))
        rightDiagram = d2 # deform (wibble LeftSide middle)

mainShape :: Diagram B
mainShape = unitSquare # alignBL <> (tensor)

mainDiagram :: Diagram B
mainDiagram = unitSquare <> (fst $ drawDiagram example)

main = mainWith mainShape