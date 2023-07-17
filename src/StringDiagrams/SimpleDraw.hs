{-# LANGUAGE NoMonomorphismRestriction       #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE TypeFamilies                    #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module StringDiagrams.SimpleDraw (
    InputDiagram,
    inputToOutput,
    drawStringDiagram,
    drawBrickDiagram,
    rectangify,
    squarify,
    isoscelify
) where

import Data.Tree ( foldTree )
import Diagrams.Prelude
import StringDiagrams.Types (BlockType (..), InputDiagram, drawSDMorphism, pinch, arity, drawSDCrossing)

------------------------------------------------------------
--  Constructing OutputDiagram  ----------------------------
------------------------------------------------------------

-- Patterns for [b] assume good type
foldOutput :: (a~BlockType,b~Path V2 Double) => a -> [b] -> b
foldOutput (Morphism a@(al,ar) _) _ =
    ((toPath [ FLinear (p2 p + 0.00001) (p2 p) |
        p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]) <>
    drawSDMorphism a)
    # pinch (-al) # pinch ar

foldOutput (MorphismWNames (als,ars) _) _ = 
    foldOutput (Morphism a "") []
    where a = ((fromIntegral . length) als, (fromIntegral . length) ars)

foldOutput (Crossing mf) _ =
    ((toPath [ FLinear (p2 p + 0.00001) (p2 p) |
        p <- [(0, 0), (0, 1), (1, 1), (1, 0)]]) <>
    drawSDCrossing mf)
    # pinch (-k) # pinch k
    where k = (fromIntegral . length) mf

foldOutput (CrossingWNames _ mf) _ =
    foldOutput (Crossing mf) []

foldOutput Compose [od1,od2] =
    od1 # pinch middle ||| od2 # pinch (-middle)
    where [w1,w2] = width <$> [od1,od2]
          [h1,h2] = [od1 # arity # fst, od2 # arity # snd]
          middle = (w1*h2+w2*h1)/(w1+w2)

foldOutput Tensor [od1,od2] = snugB $
    od1 # scaleX (mw/w1) # shearY ((a2 # snd - a2 # fst)/mw) # snugB
    <>
    od2 # scaleX (mw/w2) # snugT
    where [w1,w2] = width <$> [od1,od2]
          mw = max w1 w2
          a2 = od2 # arity

-- The main fold of OutputDiagram
inputToOutput :: InputDiagram -> Path V2 Double
inputToOutput = foldTree foldOutput

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

------------------------------------------------------------
--  Drawing OutputDiagram  ---------------------------------
------------------------------------------------------------

-- From InputDiagram to Diagram directly (BD format)
drawBrickDiagram :: Renderable (Path V2 Double) b => InputDiagram -> QDiagram b V2 Double Any
drawBrickDiagram = stroke . isoscelify . inputToOutput

-- From InputDiagram to Diagram directly (SD format)
drawStringDiagram :: Renderable (Path V2 Double) b => InputDiagram -> QDiagram b V2 Double Any
drawStringDiagram = stroke . isoscelify . inputToOutput