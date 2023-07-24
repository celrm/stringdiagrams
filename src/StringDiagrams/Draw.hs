{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds #-}

module StringDiagrams.Draw (
    arity, pinch,
    OutputClass(..),
    rectangify, squarify, isoscelify,
    drawCubic,
    getSidePoints,
    drawWires,
    drawCrossingWires
) where

import Data.Tree ( foldTree )

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )
import StringDiagrams.Read (Arity, BlockType (..), InputDiagram, NamedArity)

------------------------------------------------------------
--  Diagrams utilities  ------------------------------------
------------------------------------------------------------

type ArityClass a = (InSpace V2 Double a, Traced a, Alignable a, HasOrigin a)

-- Finds the diagram's arity (height of each side)
arity :: ArityClass a => a -> (N a, N a)
arity od = (od # findHeight, od # alignR # findHeight)
    where findHeight = fromInteger . ceiling . (\x -> x-0.1) . maybe 0 (^._y) . maxRayTraceP origin unitY

-- Deformation that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinch :: (Deformable a a, Enveloped a, ArityClass a) => N a -> a -> a
pinch h od = od # deform (Deformation $ \pt ->
    pt # scaleY (fxb pt / fx a pt))
    where a@(al, ar) = od # arity
          fx (l, r) pt =  pt^._x * (r - l) / (od # width) + l
          fxb = if h < 0 then fx (-h, ar) else fx (al, h)

------------------------------------------------------------
-- Drawing utilities  --------------------------------------
------------------------------------------------------------

-- Chooses control points in the same Y-coord than the endpoints
drawCubic :: (R1 v, Metric v, Fractional n) => Point v n -> Point v n -> FixedSegment v n
drawCubic o f = FCubic o (c o f) (c f o) f
    where c x y = x .+^ (0.5 *^ project unitX (y .-. x))

-- Gets equally spaced lists of points for the left and right sides according to the arity
getSidePoints :: Arity -> ([Point V2 Double], [Point V2 Double])
getSidePoints (al,ar) = (ptsl,ptsr)
    where ptsl = reverse [0 ^& (0.5+i) | i <-[0..al-1]]
          ptsr = reverse [1 ^& (0.5+i) | i <-[0..ar-1]]

-- Draws the wires of a morphism
drawWires :: Arity -> Path V2 Double
drawWires (al,ar) = toPath . map (drawCubic (0.5 ^& 0.5)) $ ptsl++ptsr
    where (sptsl,sptsr) = getSidePoints (al, ar)
          (ptsl,ptsr) = (sptsl # scaleY (1/al),sptsr # scaleY (1/ar))

-- Draws the wires of a crossing
drawCrossingWires :: [Int] -> Path V2 Double
drawCrossingWires mf = toPath [drawCubic (0 ^& ((0.5+i) / k))
    (1 ^& ((0.5 + fromIntegral (mf !! floor i)) / k)) | i <-[0..k-1]]
    where k = (fromIntegral . length) mf

------------------------------------------------------------
--  OutputClass typeclass  ---------------------------------
------------------------------------------------------------

class (Deformable a a, Enveloped a, ArityClass a, Transformable a, Juxtaposable a, Semigroup a)
    => OutputClass a where
    drawMorphism :: Arity -> String -> a
    drawMorphismWNames :: NamedArity -> String -> a
    drawCrossing :: [Int] -> a
    drawCrossingWNames :: [String] -> [Int] -> a

    compose :: a -> a -> a
    compose od1 od2 = od1 # pinch middle ||| od2 # pinch (-middle)
        where [w1, w2] = [width od1, width od2]
              [h1, h2] = [od1 # arity # fst, od2 # arity # snd]
              middle = (w1*h2 + w2*h1)/(w1 + w2)

    tensor :: a -> a -> a
    tensor od1 od2 = alignB $
        od1 # scaleX (mw/w1) # shearY ((a2 # snd - a2 # fst)/mw)
        <> od2 # scaleX (mw/w2) # snugT
        where [w1, w2] = [width od1, width od2]
              mw = max w1 w2
              a2 = od2 # arity

    foldOutput :: BlockType -> [a] -> a
    foldOutput (Morphism a s) _ = drawMorphism a s
    foldOutput (MorphismWNames a s) _ = drawMorphismWNames a s
    foldOutput (Crossing mf) _ = drawCrossing mf
    foldOutput (CrossingWNames ks mf) _ = drawCrossingWNames ks mf
    foldOutput Compose [od1,od2] = compose od1 od2
    foldOutput Tensor [od1,od2] = tensor od1 od2

    inputToOutput :: InputDiagram -> a
    inputToOutput = foldTree foldOutput

    strokeOutput :: a -> Diagram B


------------------------------------------------------------
--  External utilities  -------------------------------------
------------------------------------------------------------

rectangify :: (Deformable a a, Enveloped a, ArityClass a) => a -> a
rectangify od = od
    # pinch (-maxArity)
    # pinch maxArity
    where (al,ar) = od # arity
          maxArity = max al ar

squarify :: (Deformable a a, Enveloped a, ArityClass a, Transformable a) => a -> a
squarify od = od
    # rectangify
    # scaleX (maxArity/(od # width))
    where (al,ar) = od # arity
          maxArity = max al ar

isoscelify :: (Enveloped a, ArityClass a, Transformable a) => a -> a
isoscelify od = od
    # shearY ((od # arity # fst - od # arity # snd)/(2*(od # width)))