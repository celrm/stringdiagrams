{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StringDiagrams.Draw (
    OutputDiagram,
    inputToOutput,
    outputToDiagram,
    drawBrickDiagram,
    drawStringDiagram,
    scaleOD,
    scaleXOD,
    scaleYOD,
    rectangify,
    squarify,
    isoscelify
) where

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

-- The main constructor of OutputDiagram
inputToOutput :: InputDiagram -> OutputDiagram
inputToOutput (Morphism (al,ar) s) = OD
    { _ps = Paths { _bd = unitSquare # alignBL, _sd = toPath (ptsl++ptsr) }
    , _ls = Locs
        { _texts = [Loc ctr (text s # fontSizeG 0.25 # translateY (-0.0625))] -- TODO fit inside boxes
        , _boxes = [Loc ctr (square 0.3 # scaleY 1.5 # fc white)] } -- TODO clip instead
    }
    # pinchOD (-al)
    # pinchOD ar
    where
        ctr = 0.5 ^& 0.5
        ptsl = [drawCubic ctr (0 ^& ((0.5+i)/al)) | i <-[0..al-1]]
        ptsr = [drawCubic ctr (1 ^& ((0.5+i)/ar)) | i <-[0..ar-1]]

-- Only for SD (for BD it gets an empty box)
inputToOutput (Crossing k mf) =
    inputToOutput (Morphism (k,k) "")
    # set (ls . boxes) []
    # set (ps . sd) (toPath pts)
    where
        pts = [drawCubic (0 ^& (0.5+i)) (1 ^& (0.5+f i)) | i <-[0..k-1]]
        f i = fromIntegral $ fromMaybe 0 $ mf >>= \ys -> ys `atMay` n where n = floor i

-- Composing 2 OutputDiagrams
inputToOutput (Compose bd1 bd2) = alignOD (-unitX) $ nod1 <> nod2
    where
        [od1,od2] = map inputToOutput [bd1,bd2]
        middle = (od1 # arity # fst + od2 # arity # snd)/2
        nod1 = od1 # pinchOD middle # alignOD unitX
        nod2 = od2 # pinchOD (-middle)

-- Tensoring 2 OutputDiagrams
inputToOutput (Tensor bd1 bd2) = alignOD (-unitY) $ nod1 <> nod2
    where
        [od1,od2] = map inputToOutput [bd1,bd2]
        [w1,w2] = width . view (ps.bd) <$> [od1,od2]
        mw = max w1 w2
        nod1 = od1 # deform (Deformation
            $ scaleX (w1/mw)
            . shearY ((od2 # arity # snd - od2 # arity # fst)/mw))
        nod2 = od2 # deform (Deformation
            $ scaleX (w2/mw))
            # alignOD unitY

------------------------------------------------------------
--  Drawing OutputDiagram  ---------------------------------
------------------------------------------------------------

-- Put together a OutputDiagram into a Diagram B
outputToDiagram :: String -> OutputDiagram -> Diagram B
outputToDiagram tp od = mconcat diagrams
    where
        nod = od # isoscelify
        diagrams = [moveOriginTo (-o) s | (Loc o s) <- nod^.ls.texts]
            ++ [moveOriginTo (- o) s    | tp /= "bd", (Loc o s) <- nod ^. ls . boxes]
            ++ [nod^.ps.sd # strokePath | tp /= "bd"]
            ++ [nod^.ps.bd # strokePath | tp /= "sd"]

-- From InputDiagram to Diagram B directly (BD format)
drawBrickDiagram :: InputDiagram -> Diagram B
drawBrickDiagram = outputToDiagram "bd" . inputToOutput

-- From InputDiagram to Diagram B directly (SD format)
drawStringDiagram :: InputDiagram -> Diagram B
drawStringDiagram = outputToDiagram "sd" . inputToOutput

------------------------------------------------------------
--  Deforming OutputDiagram (external)  --------------------
------------------------------------------------------------

scaleXOD :: Double -> OutputDiagram -> OutputDiagram
scaleXOD k =
    deform (Deformation $ scaleX k)

scaleYOD :: Double -> OutputDiagram -> OutputDiagram
scaleYOD k =
    deform (Deformation $ scaleY k)

scaleOD :: Double -> OutputDiagram -> OutputDiagram
scaleOD k = scaleXOD k . scaleYOD k

rectangify :: OutputDiagram -> OutputDiagram
rectangify od = od
    # pinchOD (-maxArity)
    # pinchOD maxArity
    where
        (al,ar) = od # arity
        maxArity = max al ar

squarify :: OutputDiagram -> OutputDiagram
squarify od = od
    # rectangify
    # scaleXOD (maxArity/(od^.ps.bd # width))
    where
        (al,ar) = od # arity
        maxArity = max al ar

isoscelify :: OutputDiagram -> OutputDiagram
isoscelify od = od # deform (Deformation $ shearY sh)
    where sh = (od # arity # fst - od # arity # snd)/(2*(od^.ps.bd # width))