{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ConstraintKinds             #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Module for drawing string diagrams
--  The main typeclass is OutputClass, which is used to draw the diagrams
--  but has to be instantiated. A default instance OutputDiagram is provided
--  at StringDiagrams.Draw.OutputDiagram.

-- Example usage:
-- > main = do
-- >  inputDiagram <- readInputDiagram "example.json"
-- >  case inputDiagram of
-- >    Left e -> putStrLn e
-- >    Right inp ->  mainWith $ 
-- >      (inp # inputToOutput :: OutputDiagram) # strokeOutput

module StringDiagrams.Draw (
    arity, pinch,
    OutputClass(..),
    Drawable(..),
    rectangify, squarify, isoscelify,
    flatCubic,
    connectionPoints,
    drawWires,
    drawCrossingWires
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
arity od = (od # findHeight, od # alignR # findHeight)
    where findHeight = maybe 0 (^._y) . maxRayTraceP origin unitY

-- | Deformation that moves a quadrilateral's upper vertex to |k| (k<0 => left, k>0 => right)
pinch :: (Deformable a a, Enveloped a, AType a) => N a -> a -> a
pinch h od = od # deform (Deformation $ \pt ->
    pt # scaleY (fxb pt / fx a pt))
    where a@(al, ar) = od # arity
          fx (l, r) pt =  pt^._x * (r - l) / (od # width) + l
          fxb = if h < 0 then fx (-h, ar) else fx (al, h)

------------------------------------------------------------
--  OutputClass typeclass  ---------------------------------
------------------------------------------------------------

-- | Typeclass for output diagrams
--   Minimal complete definition: drawLeaf, strokeOutput
--   Default definitions: compose, tensor, drawNode, inputToOutput (should not be overriden)
class (Deformable a a, Enveloped a, AType a, Transformable a, Juxtaposable a, Semigroup a)
    => OutputClass a where
    compose :: a -> a -> a
    compose od1 od2 = od1 # pinch middle ||| od2 # pinch (-middle)
        where [w1, w2] = [width od1, width od2]
              [l_1, r_2] = [od1 # arity # fst, od2 # arity # snd]
              middle = (w1*r_2 + w2*l_1)/(w1 + w2)

    tensor :: a -> a -> a
    tensor od1 od2 = alignB $
        od1 # scaleX (mw/w1) # shearY ((a2#snd - a2#fst)/mw)
        === od2 # scaleX (mw/w2)
        where [w1, w2] = [width od1, width od2]
              mw = max w1 w2
              a2 = od2 # arity

    inputToOutput :: Tree NodeType -> a
    inputToOutput = foldTree drawNode

    drawNode :: NodeType -> [a] -> a
    drawNode (Leaf l) _ = drawLeaf l
    drawNode Compose [od1,od2] = compose od1 od2
    drawNode Tensor [od1,od2] = tensor od1 od2

    drawLeaf :: LeafType -> a

    strokeOutput :: a -> Diagram B

class (InSpace V2 Double a, Deformable a a, Semigroup a) => Drawable a where
    draw :: LeafType -> a
    strokeDrawing :: a -> Diagram B

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
          c = 0.5 ^& (0.125 * (al + 1) * (ar + 1))

-- | Draws the wires of a crossing
drawCrossingWires :: [Int] -> Path V2 Double
drawCrossingWires mf = toPath [flatCubic (0 ^& (0.5+i))
    (1 ^& (0.5 + fromIntegral (mf !! floor i))) | i <-[0..k-1]]
    where k = (fromIntegral . length) mf

------------------------------------------------------------
--  External utilities  -------------------------------------
------------------------------------------------------------

-- | Deformation that makes a diagram rectangular
rectangify :: (Deformable a a, Enveloped a, AType a) => a -> a
rectangify od = od
    # pinch (-maxArity)
    # pinch maxArity
    where (al,ar) = od # arity
          maxArity = max al ar

-- | Deformation that makes a diagram square
squarify :: (Deformable a a, Enveloped a, AType a, Transformable a) => a -> a
squarify od = od
    # rectangify
    # scaleX (maxArity/(od # width))
    where (al,ar) = od # arity
          maxArity = max al ar

-- | Deformation that makes a diagram an isosceles trapezoid
isoscelify :: (Enveloped a, AType a, Transformable a) => a -> a
isoscelify od = od
    # shearY ((od # arity # fst - od # arity # snd)/(2*(od # width)))