{-# LANGUAGE NoMonomorphismRestriction       #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE TypeFamilies                    #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module StringDiagrams.Draw (
    InputDiagram,
    OutputDiagram,
    inputToOutput,
    outputToStringDiagram,
    drawStringDiagram,
    outputToBrickDiagram,
    drawBrickDiagram,
    rectangify,
    squarify,
    isoscelify
) where

import Data.Tree ( foldTree )
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )
import StringDiagrams.Types

------------------------------------------------------------
--  Constructing OutputDiagram  ----------------------------
------------------------------------------------------------

-- Patterns for [b] assume good type
foldOutput :: (a~BlockType,b~OutputDiagram) => a -> [b] -> b
foldOutput (Morphism a s) _ = drawMorphism a s

foldOutput (MorphismWNames a s) _ = drawMorphismWNames a s

foldOutput (Crossing k mf) _ = drawCrossing k mf

foldOutput Compose [od1,od2] =
    od1 # pinch middle ||| od2 # pinch (-middle)
    where [w1,w2] = width <$> [od1,od2]
          [h1,h2] = [od1 # arity # fst, od2 # arity # snd]
          middle = (w1*h2+w2*h1)/(w1+w2)

foldOutput Tensor [od1,od2] = alignB $
    od1 # scaleX (mw/w1) # shearY ((a2 # snd - a2 # fst)/mw)
    ===
    od2 # scaleX (mw/w2)
    where [w1,w2] = width <$> [od1,od2]
          mw = max w1 w2
          a2 = od2 # arity

-- The main fold of OutputDiagram
inputToOutput :: InputDiagram -> OutputDiagram
inputToOutput = foldTree foldOutput

------------------------------------------------------------
--  Deforming OutputDiagram (externally)  ------------------
------------------------------------------------------------

rectangify :: OutputDiagram -> OutputDiagram
rectangify od = od
    # pinch (-maxArity)
    # pinch maxArity
    where (al,ar) = od # arity
          maxArity = max al ar

squarify :: OutputDiagram -> OutputDiagram
squarify od = od
    # rectangify
    # scaleX (maxArity/(od # width))
    where (al,ar) = od # arity
          maxArity = max al ar

isoscelify :: OutputDiagram -> OutputDiagram
isoscelify od = od 
    # shearY ((od # arity # fst - od # arity # snd)/(2*(od # width)))

------------------------------------------------------------
--  Drawing OutputDiagram  ---------------------------------
------------------------------------------------------------

-- From InputDiagram to Diagram directly (BD format)
drawBrickDiagram :: InputDiagram -> QDiagram B V2 Double Any
drawBrickDiagram = outputToBrickDiagram . isoscelify . inputToOutput

-- From InputDiagram to Diagram directly (SD format)
drawStringDiagram :: InputDiagram -> QDiagram B V2 Double Any
drawStringDiagram = outputToStringDiagram . isoscelify . inputToOutput