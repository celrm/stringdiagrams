module StringDiagrams (
    readInputDiagram, readInputDiagramWN, leafArity,
    LeafType, NodeType, Tree, FoldableDiagram, Drawable,
    inputToOutput, strokeOutput, 
    pinch, rectangify, squarify, isoscelify,
    BrickWrapper, strokeBrick, unwrap, 
    LabelsDiagram, strokeWires,
    MonCatDiagram, getCat, getDrawing, getBrick,
) where

import StringDiagrams.Read (LeafType, NodeType, leafArity, readInputDiagram, readInputDiagramWN)
import Data.Tree (Tree)
import StringDiagrams.Draw (FoldableDiagram(strokeOutput), inputToOutput, pinch, Drawable, rectangify, squarify, isoscelify)

import StringDiagrams.Draw.LabelsDiagram (LabelsDiagram , strokeWires)
import StringDiagrams.BrickWrapper (strokeBrick, BrickWrapper, unwrap)
import StringDiagrams.MonCatDiagram (MonCatDiagram, getCat, getDrawing, getBrick)

-- | Library for drawing string diagrams from JSON.
--  The main typeclass is FoldableDiagram, which has to be instantiated. 
--  Alternatively, one can instantiate Drawable a and create a type BrickWrapper a.
--  A default instance LabelsDiagram is provided.

-- Example usage:
-- > main = do
-- >  inputDiagram <- readInputDiagram "example.json"
-- >  case inputDiagram of
-- >    Left e -> putStrLn e
-- >    Right inp ->  mainWith $ 
-- >      (inp # inputToOutput :: LabelsDiagram) # strokeOutput