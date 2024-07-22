module Graph.Dot
  ( DotId
  , DotLine
  , Dot
  , PlotGraph
  -- constructors
  , opNode
  , dataNode
  , graphToDot
  -- printing
  , plotSVG
  ) where

import Data.List (intercalate) 
import qualified Data.Map as M
import qualified Graph as G
import System.Process (callProcess)
import Text.Printf (printf)

-----------
-- Types --
-----------

data PlotNode
  = DataNode String String String
  | OpNode String
  deriving (Eq)

type PlotGraph = G.Graph PlotNode

newtype DotId = DotId Int

data DotLine
  = DotNode DotId PlotNode
  | DotEdge DotId DotId

type Dot = [DotLine]

-------------------
-- Constructors -- 
-------------------

dataNode :: String -> Double -> Double -> PlotNode
dataNode label val grad =
  DataNode label (printf "%.04f" val) (printf "%.04f" grad)

opNode :: String -> PlotNode
opNode = OpNode

--------------
-- Printing --
--------------

graphToDot :: PlotGraph -> Dot
graphToDot graph = nodes <> edges
  where
    toDotId :: G.NodeId -> DotId
    toDotId = DotId . G.idToInt

    nodeToDotLine :: G.NodeId -> PlotNode -> DotLine
    nodeToDotLine nid node = DotNode (toDotId nid) node

    nodes :: Dot
    nodes = fmap (uncurry nodeToDotLine) (M.toList (G.nodes graph))

    edgeToDotLine :: (G.NodeId, G.NodeId) -> DotLine
    edgeToDotLine (nid1, nid2) = DotEdge (toDotId nid1) (toDotId nid2)

    edges :: Dot
    edges = fmap edgeToDotLine (G.edges graph)


printPlotNode :: PlotNode -> String
printPlotNode (OpNode label) = "label = \"" <> label <> "\""
printPlotNode (DataNode label value grad) = intercalate ", "
  [ "shape = record"
  , "label = \"{" <> label <> " | " <> value <> " | " <> grad <> "}\""
  ]

printDotLine :: DotLine -> String
printDotLine (DotNode (DotId nid) node) =
  show nid <> " [" <> printPlotNode node <> "]"
printDotLine (DotEdge (DotId a) (DotId b)) =
  show a <> " -> " <> show b

printDot :: Dot -> String
printDot dotLines =
  unlines (start <> fmap (("\t" <>) . printDotLine) dotLines <> end)
  where
    start = ["digraph G {", "rankdir = LR", ""]
    end = ["", "}"]

plotSVG :: FilePath -> Dot -> IO FilePath
plotSVG fp dot = do
  let dotFile = fp <> ".dot"
      pngFile = fp <> ".svg"
  writeFile dotFile (printDot dot)
  callProcess "dot" ["-Tsvg", "-o", pngFile, dotFile]
  pure pngFile
