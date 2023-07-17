{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StringDiagrams.Draw (
    InputDiagram,
    OutputDiagram,
    OutputClass(..),
    outputToStringDiagram,
    outputToBrickDiagram,
    outputToStrings,
    rectangify, squarify, isoscelify
) where

import Data.Tree ( Tree, foldTree )
import Data.List ( nubBy )

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )
import StringDiagrams.Types

$(makeLenses ''Paths)
$(makeLenses ''Locatables)
$(makeLenses ''OutputDiagram)

------------------------------------------------------------
--  Utility functions  -------------------------------------
------------------------------------------------------------

-- Finds the diagram's arity (height of each side)
arity :: (V a ~ V2, Traced a, RealFrac (N a), Alignable a, HasOrigin a) => a -> (N a, N a)
arity od = (od # findHeight, od # alignR # findHeight)
    where findHeight = fromInteger . ceiling . (\x -> x-0.1) . maybe 1 (^._y) . maxRayTraceP origin unitY

-- Deformation that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinchDeformation :: (V a ~ V2, HasOrigin a, Alignable a, RealFrac (N a), Traced a,
 Enveloped a) => N a -> a -> Deformation (V a) (V a) (N a)
pinchDeformation k od = Deformation $ \pt -> pt # scaleY ((m' * pt^._x + n') / (m * pt^._x + n))
    where (al,ar) = od # arity
          w = od # width
          lineEquation (x1,y1) (x2,y2) = (slope,y1-slope*x1) where slope = (y2-y1)/(x2-x1)
          (m,n) = lineEquation (0,al) (w,ar)
          (m',n') = if k<0 then lineEquation (0,-k) (w,ar) else lineEquation (0,al) (w,k)

pinch :: (V a ~ V2, Deformable a a, HasOrigin a, Alignable a,
 Traced a, Enveloped a, RealFrac (N a)) => N a -> a -> a
pinch k od = od # deform (pinchDeformation k od)

------------------------------------------------------------
--  Drawing (hiding the implementation)  -------------------
------------------------------------------------------------

getSidePoints :: Arity -> ([Point V2 Double], [Point V2 Double])
getSidePoints (al,ar) = (ptsl,ptsr)
    where ptsl = reverse [0 ^& (0.5+i) | i <-[0..al-1]]
          ptsr = reverse [1 ^& (0.5+i) | i <-[0..ar-1]]

drawWires :: Arity -> Path V2 Double
drawWires (al,ar) = toPath . map (drawCubic (0.5 ^& 0.5)) $ (ptsl++ptsr)
    where (sptsl,sptsr) = getSidePoints (al, ar)
          (ptsl,ptsr) = (sptsl # scaleY (1/al),sptsr # scaleY (1/ar))

drawCrossingWires :: [Int] -> Path V2 Double
drawCrossingWires mf = toPath [drawCubic (0 ^& ((0.5+i) / k))
    (1 ^& ((0.5 + fromIntegral (mf !! floor i)) / k)) | i <-[0..k-1]]
    where k = (fromIntegral . length) mf

class (Juxtaposable a, Enveloped a, Semigroup a
    , Deformable a a, HasOrigin a, Transformable a
    , V a ~ V2, Alignable a, RealFrac (N a), Traced a)
    => OutputClass a where
    drawMorphism :: Arity -> String -> a
    drawMorphismWNames :: NamedArity -> String -> a
    drawCrossing :: [Int] -> a
    drawCrossingWNames :: [String] -> [Int] -> a
    compose :: a -> a -> a
    compose od1 od2 = od1 # pinch middle ||| od2 # pinch (-middle)
        where [w1,w2] = width <$> [od1,od2]
              [h1,h2] = [od1 # arity # fst, od2 # arity # snd]
              middle = (w1*h2+w2*h1)/(w1+w2)
    tensor :: a -> a -> a
    tensor od1 od2 = snugB $
        od1 # scaleX (mw/w1) # shearY ((a2 # snd - a2 # fst)/mw) # snugB
        <>
        od2 # scaleX (mw/w2) # snugT
        where [w1,w2] = width <$> [od1,od2]
              mw = max w1 w2
              a2 = od2 # arity
    foldOutput :: BlockType -> [a] -> a
    foldOutput (Morphism a s) _ = drawMorphism a s
    foldOutput (MorphismWNames a s) _ = drawMorphismWNames a s
    foldOutput (Crossing mf) _ = drawCrossing mf
    foldOutput (CrossingWNames ks mf) _ = drawCrossingWNames ks mf
    foldOutput Compose [od1,od2] = compose od1 od2
    foldOutput Tensor [od1,od2] = tensor od1 od2
    inputToOutput :: Tree BlockType -> a
    inputToOutput = foldTree foldOutput
    strokeOutput :: a -> Diagram B

instance OutputClass (Path V2 Double) where
    drawMorphism a@(al, ar) _ =
        ((toPath [ FLinear (p2 p + 0.00001) (p2 p) | p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]) <>
        drawWires a) # pinch (-al) # pinch ar

    drawCrossing mf =
        ((toPath [ FLinear (p2 p + 0.00001) (p2 p) | p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]) <>
        drawCrossingWires mf) # pinch (-k) # pinch k
        where k = (fromIntegral . length) mf

    strokeOutput = strokePath

    drawMorphismWNames (als, ars) _ = drawMorphism ((fromIntegral . length) als, (fromIntegral . length) ars) ""

    drawCrossingWNames _ = drawCrossing

instance OutputClass OutputDiagram where
    drawMorphism (al, ar) s = OD
        { _ps = Paths { _bd = unitSquare # alignBL, _sd = drawWires (al,ar) }
        , _ls = Locs
            { _labels = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
            , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
        } # pinch (-al) # pinch ar
        where ctr = 0.5 ^& 0.5

    drawMorphismWNames (als, ars) s =
        drawMorphism (al, ar) s
        # over (ls . labels) (++ wireNames)
        where (al, ar) = ((fromIntegral . length) als, (fromIntegral . length) ars)
              (ptsl,ptsr) = getSidePoints (al, ar)
              funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
              wireNames = funct ptsl als ++ funct ptsr ars
              -- they get drawn twice

    drawCrossing mf = OD
        { _ps = Paths { _bd = unitSquare # alignBL
        , _sd = drawCrossingWires mf }
        , _ls = Locs { _labels = [], _boxes = [] }
        } # pinch (-k) # pinch k
        where k = (fromIntegral . length) mf

    drawCrossingWNames ks mf =
        drawCrossing mf
        # over (ls . labels) (++ wireNames)
        where k = (fromIntegral . length) mf
              (ptsl,ptsr) = getSidePoints (k, k)
              funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
              wireNames = funct ptsl ks ++ funct ptsr (map (ks !!) mf)

    strokeOutput = outputToStringDiagram

-- Put together an OutputDiagram into a Diagram B
outputToStringDiagram :: OutputDiagram -> Diagram B
outputToStringDiagram od = mconcat
            -- adding nubBy to remove duplicate wire names
            $  [moveOriginTo (-o) s | (Loc o s) <-
                    nubBy (\a b -> distance (loc a) (loc b) < 0.00001) (od^.ls.labels)]
            ++ [moveOriginTo (-o) s | (Loc o s) <- od^.ls.boxes]
            ++ [od^.ps.sd # strokePath]

-- Put together an OutputDiagram into a Diagram B
outputToBrickDiagram :: OutputDiagram -> Diagram B
outputToBrickDiagram od = mconcat
            $  [moveOriginTo (-o) s | (Loc o s) <- od^.ls.labels]
            ++ [od^.ps.bd # strokePath]

outputToStrings :: OutputDiagram -> Diagram B
outputToStrings od = mconcat [od^.ps.sd # strokePath]

------------------------------------------------------------
--  Deforming OutputDiagram (externally)  ------------------
------------------------------------------------------------

rectangify :: (V a ~ V2, Deformable a a, HasOrigin a, Alignable a, Traced a,
 Enveloped a, RealFrac (N a)) => a -> a
rectangify od = od
    # pinch (-maxArity)
    # pinch maxArity
    where (al,ar) = od # arity
          maxArity = max al ar

squarify :: (V a ~ V2, Enveloped a, Deformable a a, HasOrigin a, Alignable a,
 RealFrac (N a), Traced a, Transformable a) => a -> a
squarify od = od
    # rectangify
    # scaleX (maxArity/(od # width))
    where (al,ar) = od # arity
          maxArity = max al ar

isoscelify :: (V a ~ V2, Transformable a, Traced a, RealFrac (N a), Alignable a,
 HasOrigin a, Enveloped a) => a -> a
isoscelify od = od
    # shearY ((od # arity # fst - od # arity # snd)/(2*(od # width)))