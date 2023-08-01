{-# LANGUAGE NoMonomorphismRestriction    #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module StringDiagrams.Draw (
    arity, pinch,
    FoldableDiagram(..),
    Drawable(..), Compilable(..),
    inputToOutput,
    rectangify, squarify, isoscelify,
    flatCubic,
    connectionPoints,
    drawWires,
    drawCrossingWires,
    tensorOps, composeOps
) where

import Data.Tree ( Tree, foldTree )

import Diagrams.Prelude
import StringDiagrams.Read (Arity, NodeType (..), LeafType)
import Diagrams.Backend.SVG (B)

------------------------------------------------------------
--  Diagrams utilities  ------------------------------------
------------------------------------------------------------

type AType a=(InSpace V2 Double a, Traced a, Alignable a, HasOrigin a)

-- | Finds the diagram's arity (height of each side)
arity :: AType a => a -> (N a, N a)
arity d = (d # findHeight, d # alignR # findHeight)
    where findHeight = maybe 0 (^._y) . maxRayTraceP origin unitY

-- | Deformation that moves a quadrilateral's upper vertex to |h| (h<0 => left, h>0 => right)
pinch :: (Deformable a a, Enveloped a, AType a) => N a -> a -> a
pinch h d = d # deform (Deformation $ \p -> scaleY (fxb p / fx a p) p)
    where a@(al, ar) = d # arity
          fx (l, r) pt =  pt^._x * (r - l) / (d # width) + l
          fxb = if h < 0 then fx (-h, ar) else fx (al, h)

tensorOps :: (Deformable a a, Enveloped a, AType a, Transformable a) => a -> a -> (a -> a, a -> a)
tensorOps d1 d2 = (shearY sh . scaleX (mw/w1), scaleX (mw/w2))
    where [w1, w2, mw] = [width d1, width d2, max w1 w2]
          sh = (d2 # arity # snd - d2 # arity # fst)/mw


composeOps :: (Deformable a a, Enveloped a, AType a) => a -> a -> (a -> a, a -> a)
composeOps d1 d2 = (pinch middle, pinch (-middle))
    where [w1, w2] = [width d1, width d2]
          [h1, h2] = [d1 # arity # fst, d2 # arity # snd]
          middle = (w1*h2 + w2*h1)/(w1 + w2)
------------------------------------------------------------
--  FoldableDiagram typeclass  ---------------------------------
------------------------------------------------------------

inputToOutput :: FoldableDiagram a => Tree NodeType -> a
inputToOutput = foldTree drawNode
    where drawNode (Leaf l) _ = leaf l
          drawNode Compose [d1,d2] = compose d1 d2
          drawNode Tensor [d1,d2] = tensor d1 d2

-- | Typeclass for output diagrams
--   Minimal complete definition: leaf, strokeOutput
--   Default definitions: compose, tensor (should not be overridden usually)
class (Deformable a a, Enveloped a, AType a, Transformable a, 
    Juxtaposable a, Semigroup a) => FoldableDiagram a where
    compose :: a -> a -> a
    compose d1 d2 = d1 # t1 ||| d2 # t2
        where (t1, t2) = composeOps d1 d2

    tensor :: a -> a -> a
    tensor d1 d2 = alignB $ d1 # t1 === d2 # t2
        where (t1, t2) = tensorOps d1 d2

    leaf :: LeafType -> a
    strokeOutput :: a -> Diagram B

class (InSpace V2 Double a, Deformable a a, Semigroup a) => Drawable a where
    draw :: LeafType -> a
    strokeDrawing :: a -> Diagram B

class Compilable a where
    baseCase :: LeafType -> a
    tensorOp :: a -> a -> a
    composeOp :: a -> a -> a

------------------------------------------------------------
-- Drawing utilities  --------------------------------------
------------------------------------------------------------

-- | Chooses control points in the same Y-coord as the endpoints
flatCubic :: (R1 v, Metric v, Fractional n) => Point v n -> Point v n -> FixedSegment v n
flatCubic o f = FCubic o (c o f) (c f o) f
    where c x y = x .+^ (0.5 *^ project unitX (y .-. x))

-- | Gets equally spaced lists of points for the left and right sides according to the arity
connectionPoints :: Arity -> ([Point V2 Double], [Point V2 Double])
connectionPoints (al, ar) = (ptsl, ptsr)
    where ptsl = reverse [0 ^& (0.5+i) | i <-[0..al-1]]
          ptsr = reverse [1 ^& (0.5+i) | i <-[0..ar-1]]

-- | Draws the wires of a morphism
drawWires :: Arity -> Path V2 Double
drawWires a@(al, ar) = toPath . map (flatCubic c) $ ptsl++ptsr
    where (ptsl, ptsr) = connectionPoints a
          c = 0.5 ^& (0.25*al + 0.25*ar)

-- | Draws the wires of a crossing
drawCrossingWires :: [Int] -> Path V2 Double
drawCrossingWires p = toPath [flatCubic (0 ^& (0.5+i))
    (1 ^& (0.5 + fromIntegral (p !! floor i))) | i <-[0..k-1]]
    where k = (fromIntegral . length) p

------------------------------------------------------------
--  External utilities  -------------------------------------
------------------------------------------------------------

-- | Deformation that makes a diagram rectangular
rectangify :: (Deformable a a, Enveloped a, AType a) => a -> a
rectangify d = d
    # pinch (-maxArity)
    # pinch maxArity
    where (al,ar) = d # arity
          maxArity = max al ar

-- | Deformation that makes a diagram square
squarify :: (Deformable a a, Enveloped a, AType a, Transformable a) => a -> a
squarify d = d
    # rectangify
    # scaleX (maxArity/(d # width))
    where (al,ar) = d # arity
          maxArity = max al ar

-- | Deformation that makes a diagram an isosceles trapezoid
isoscelify :: (Enveloped a, AType a, Transformable a) => a -> a
isoscelify d = d
    # shearY ((d # arity # fst - d # arity # snd)/(2*(d # width)))