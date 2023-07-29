{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE InstanceSigs              #-}

module StringDiagrams.Draw.LabelsDiagram (LabelsDiagram, strokeWires) where

import Data.List ( nubBy )

import Diagrams.Prelude
import Diagrams.Backend.SVG ( B )

import StringDiagrams.Draw
    ( connectionPoints,
      drawWires,
      drawCrossingWires, Drawable (..) )
import StringDiagrams.Read (LeafType(..))
import StringDiagrams.BrickWrapper (deformPath, BrickWrapper, unwrap)

------------------------------------------------------------
--  UserDiagram type  ------------------------------------
------------------------------------------------------------

-- | A custom object made up of: 
--   1) paths, which are modified only on their end or control points (string diagrams)
--   2) located objects, which are modified only on their origin point (text labels or SD boxes)

data UserDiagram = UD { _sd :: Path V2 Double, _ls :: [Located (Diagram B)] }
$(makeLenses ''UserDiagram)

type instance V UserDiagram = V2
type instance N UserDiagram = Double
instance Semigroup UserDiagram where
    (<>) od1 od2 = od1 # over sd (<> od2^.sd) # over ls (<> od2^.ls)

instance r ~ UserDiagram => Deformable UserDiagram r where
    deform' _ = deform
    deform t = over sd (deformPath t) . over (ls . traverse) (deformLoc t)
        where deformLoc t' (Loc o s) = Loc (deform t' o) s

------------------------------------------------------------
--  UserDiagram is Drawable ---------------------------------
------------------------------------------------------------

instance Drawable UserDiagram where
    strokeDrawing od = mconcat -- adding nubBy to remove duplicate wire names
            $  [moveOriginTo (-o) s | (Loc o s) <-
                    nubBy (\a b -> distance (loc a) (loc b) < 0.00001) (od^.ls)]
            ++ [od^.sd # strokePath]

    draw :: LeafType -> UserDiagram
    draw (Morphism (al, ar) s) = UD
        { _sd = drawWires (al,ar)
        , _ls = [ Loc c
            $ (text s # fontSizeG 0.25 # translateY (-0.0625)) -- TODO fit inside boxes
            <> (square 0.3 # scaleY 1.5 # fc white)] -- TODO clip instead
        } where c = 0.5 ^& (0.125 * (al + 1) * (ar + 1))

    draw (Crossing mf) = UD { _sd = drawCrossingWires mf , _ls = [] }
    
    draw (MorphismWNames (als, ars) s) = UD
        { _sd = drawWires (al,ar)
        , _ls = [ Loc c
            $ (text s # fontSizeG 0.25 # translateY (-0.0625)) -- TODO fit inside boxes
            <> (square 0.3 # scaleY 2 # fc white)] -- TODO clip instead
        } # over ls (++ wireNames) 
        where c = 0.5 ^& (0.125 * (al + 1) * (ar + 1))
              a@(al, ar) = ((fromIntegral . length) als, (fromIntegral . length) ars)
              (ptsl,ptsr) = connectionPoints a
              funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
              wireNames = funct ptsl als ++ funct ptsr ars
              -- they get drawn twice
              
    draw (CrossingWNames ks mf) = UD 
        { _sd = drawCrossingWires mf 
        , _ls = wireNames }
        where k = (fromIntegral . length) mf
              (ptsl,ptsr) = connectionPoints (k, k)
              funct = zipWith (\p n -> Loc p (text n # fontSizeG 0.25 # translateY 0.0625))
              wireNames = funct ptsl ks ++ funct ptsr (map (ks !!) mf)

type LabelsDiagram = BrickWrapper UserDiagram

strokeWires :: Renderable (Path V2 Double) b => LabelsDiagram -> QDiagram b V2 Double Any
strokeWires = stroke . (^.sd) . unwrap