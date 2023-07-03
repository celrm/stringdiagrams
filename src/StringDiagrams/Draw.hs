{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module StringDiagrams.Draw (
    OutputDiagram,
    inputToOutput,
    outputToDiagram,
    drawBrickDiagram,
    drawStringDiagram,
    rectangify,
    squarify,
    isoscelify
) where

import Data.Tree ( foldTree )
import Safe ( atMay )
import Data.Maybe ( fromMaybe )
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( B )
import StringDiagrams.DiagramTypes
$(makeLenses ''Paths)
$(makeLenses ''Locatables)
$(makeLenses ''OutputDiagram)

------------------------------------------------------------
--  Constructing OutputDiagram  ----------------------------
------------------------------------------------------------

-- Patterns for [] assume good type
foldOutput :: (a~BlockType,b~OutputDiagram) => a -> [b] -> b
foldOutput (Morphism (al,ar) s) _ = OD
    { _ps = Paths { _bd = unitSquare # alignBL, _sd = toPath (ptsl++ptsr) }
    , _ls = Locs
        { _texts = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    }
    # pinch (-al)
    # pinch ar
    where
        ctr = 0.5 ^& 0.5
        ptsl = [drawCubic ctr (0 ^& ((0.5+i)/al)) | i <-[0..al-1]]
        ptsr = [drawCubic ctr (1 ^& ((0.5+i)/ar)) | i <-[0..ar-1]]

foldOutput (Crossing k mf) _ =
    foldOutput (Morphism (k,k) "") []
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where
        pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+f i)) | i <-[0..k-1]]
        f i = fromIntegral $ fromMaybe 0 $ mf >>= \ys -> ys `atMay` n where n = floor i

foldOutput Compose [od1,od2] =
    od1 # pinch middle ||| od2 # pinch (-middle)
    where middle = (od1 # arity # fst + od2 # arity # snd)/2

foldOutput Tensor [od1,od2] = alignB $
    od1 # scaleX (w1/mw) # shearY ((od2 # arity # snd - od2 # arity # fst)/mw)
    === 
    od2 # scaleX (w2/mw)
    where
        [w1,w2] = width <$> [od1,od2]
        mw = max w1 w2

-- The main fold of OutputDiagram
inputToOutput :: InputDiagram -> OutputDiagram
inputToOutput = foldTree foldOutput

------------------------------------------------------------
--  Drawing OutputDiagram  ---------------------------------
------------------------------------------------------------

-- Put together an OutputDiagram into a Diagram B
outputToDiagram :: String -> OutputDiagram -> Diagram B
outputToDiagram tp od = mconcat 
            $  [moveOriginTo (-o) s | (Loc o s) <- od^.ls.texts]
            ++ [moveOriginTo (-o) s | tp /= "bd", (Loc o s) <- od^.ls.boxes]
            ++ [od^.ps.sd # strokePath | tp /= "bd"]
            ++ [od^.ps.bd # strokePath | tp /= "sd"]

-- From InputDiagram to Diagram B directly (BD format)
drawBrickDiagram :: InputDiagram -> Diagram B
drawBrickDiagram = outputToDiagram "bd" . isoscelify . inputToOutput

-- From InputDiagram to Diagram B directly (SD format)
drawStringDiagram :: InputDiagram -> Diagram B
drawStringDiagram = outputToDiagram "sd" . isoscelify . inputToOutput

------------------------------------------------------------
--  Deforming OutputDiagram (externally)  ------------------
------------------------------------------------------------

rectangify :: OutputDiagram -> OutputDiagram
rectangify od = od
    # pinch (-maxArity)
    # pinch maxArity
    where
        (al,ar) = od # arity
        maxArity = max al ar

squarify :: OutputDiagram -> OutputDiagram
squarify od = od # rectangify
    # scaleX (maxArity/(od # width))
    where
        (al,ar) = od # arity
        maxArity = max al ar

isoscelify :: OutputDiagram -> OutputDiagram
isoscelify od = od # shearY ((od # arity # fst - od # arity # snd)/(2*(od # width)))