module StringDiagrams (
    readInputDiagram, readInputDiagramWN, leafArity,
    LeafType, NodeType, Tree, FoldableDiagram, Drawable, Compilable,
    inputToOutput, strokeOutput, 
    pinch, rectangify, squarify, isoscelify,
    BrickWrapper, strokeBrick, unwrap, 
    MonCatWrapper, getSemantics, getDrawing, getWrapper,
) where

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

import Data.Tree (Tree)
import StringDiagrams.Read (LeafType, NodeType, leafArity, readInputDiagram, readInputDiagramWN)
import StringDiagrams.Draw (FoldableDiagram(strokeOutput), inputToOutput, pinch, rectangify, squarify, isoscelify)

import StringDiagrams.BrickWrapper (BrickWrapper, Drawable, unwrap, strokeBrick)
import StringDiagrams.MonCatWrapper (MonCatWrapper, Compilable, getSemantics, getDrawing, getWrapper)