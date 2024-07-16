module Graph.Dot
  ( Dot
  , DotLine
  -- constructors
  , opNode
  , dataNode
  , edge
  -- printing
  , plotPng
  ) where

import Data.List (intercalate) 
import System.Process (callProcess)

data Node
  = OpNode String
  | DataNode String String String

newtype NodeId = NodeId Int

data DotLine
  = DotNode NodeId Node
  | DotEdge NodeId NodeId

type Dot = [DotLine]


-------------------
-- Constructors -- 
-------------------

opNode :: Int -> String -> DotLine
opNode nodeId label = DotNode (NodeId nodeId) (OpNode label)

dataNode :: Int -> String -> Double -> Double -> DotLine 
dataNode nodeId label value grad =
  let node = DataNode label (show value) (show grad)
  in DotNode (NodeId nodeId) node

edge :: Int -> Int -> DotLine
edge a b = DotEdge (NodeId a) (NodeId b)


--------------
-- Printing --
--------------

printNode :: Node -> String
printNode (OpNode label) = "label = \"" <> label <> "\""
printNode (DataNode label value grad) = intercalate ", "
  [ "shape = record"
  , "label = \"" <> label <> " | " <> value <> " | " <> grad <> "\""
  ]

printLine :: DotLine -> String
printLine (DotNode (NodeId nid) node) = show nid <> " [" <> printNode node <> "]"
printLine (DotEdge (NodeId a) (NodeId b)) = show a <> " -> " <> show b

printDot :: Dot -> String
printDot dotLines =
  unlines (start <> fmap (("\t" <>) . printLine) dotLines <> end)
  where
    start = ["digraph G {", "rankdir = LR", ""]
    end = ["", "}"]

plotPng :: FilePath -> Dot -> IO FilePath
plotPng fp dot = do
  let dotFile = fp <> ".dot"
      pngFile = fp <> ".png"
  writeFile dotFile (printDot dot)
  callProcess "dot" ["-Tpng", "-o", pngFile, dotFile]
  pure pngFile
