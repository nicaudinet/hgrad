{-# LANGUAGE RecordWildCards #-}

module Engine.Visualize
  ( plotGraphSVG
  ) where

import Control.Monad.Trans.State
import Data.List (intercalate) 
import qualified Data.Map as M
import System.Process (callProcess)
import Text.Printf (printf)

import qualified Engine as E
import qualified Graph as G

-------------
-- * Types --
-------------

data PlotNode
  = DataNode
      { dataNodeLabel :: String
      , dataNodeVal :: String
      , dataNodeGrad :: String
      }
  | OpNode String
  deriving (Eq)

type PlotGraph = G.Graph PlotNode

newtype DotId = DotId Int

data DotLine
  = DotNode DotId PlotNode
  | DotEdge DotId DotId

type Dot = [DotLine]

-- | Map from a node in the computational graph to a node in the plot graph
type NodeMap = M.Map G.NodeId G.NodeId

-- | The state required for building the plot graph
data PlotState =
  PlotState
    { plotGraph :: PlotGraph
    , dataNodeMap :: NodeMap
    , opNodeMap :: NodeMap
    }
type PlotM = State PlotState

---------------------------------
-- * Converting to a PlotGrpah --
---------------------------------

dataNode :: E.Payload -> PlotNode
dataNode payload =
  DataNode
    { dataNodeLabel = E.nodeLabel payload
    , dataNodeVal = printf "%.04f" (E.nodeVal payload)
    , dataNodeGrad = printf "%.04f" (E.nodeGrad payload)
    }

opNode :: E.Payload -> PlotNode
opNode payload = OpNode $
  case E.nodeType payload of
    E.ValueOp -> error "ValueOp is not an operation"
    E.AddOp -> "+"
    E.ShiftOp c -> "+ " <> printf "%.03f" c
    E.MulOp -> "*"
    E.ScaleOp c -> "* " <> printf "%.03f" c
    E.ReLUOp -> "ReLU"
    E.TanhOp -> "tanh"

-- | Convert a CGraph to a PlotGraph which:
--   - Categorize nodes into data nodes and operation nodes
--   - Split intermediate nodes in CGraph into an operation and a data node
toPlotGraph :: E.CGraph -> PlotGraph
toPlotGraph cGraph = plotGraph (execState createPlotGraph initPlotState)
  where
    initPlotState :: PlotState
    initPlotState = PlotState G.empty M.empty M.empty

    createPlotGraph :: PlotM ()
    createPlotGraph = insertDataNodes >> insertOpNodes >> insertEdges

    insertDataNode :: G.NodeId -> E.Payload -> PlotM ()
    insertDataNode nid payload = do
      PlotState{..} <- get
      let (nid', plotGraph') = G.insertNode (dataNode payload) plotGraph
      put $
        PlotState
          { plotGraph = plotGraph'
          , dataNodeMap = M.insert nid nid' dataNodeMap
          , opNodeMap = opNodeMap
          }

    insertDataNodes :: PlotM ()
    insertDataNodes =
      mapM_ (uncurry insertDataNode) (M.toList (G.nodes cGraph))

    insertOpNode :: G.NodeId -> E.Payload -> PlotM ()
    insertOpNode nid payload =
      case E.nodeType payload of
        E.ValueOp -> pure ()
        _ -> do
          PlotState{..} <- get
          let (nid', plotGraph') = G.insertNode (opNode payload) plotGraph
          put $
            PlotState
              { plotGraph = plotGraph'
              , dataNodeMap = dataNodeMap
              , opNodeMap = M.insert nid nid' opNodeMap
              }

    insertOpNodes :: PlotM ()
    insertOpNodes = mapM_ (uncurry insertOpNode) (M.toList (G.nodes cGraph))

    -- Insert edge that already existed in the back propagation graph
    insertPrevEdge :: G.NodeId -> G.NodeId -> PlotM ()
    insertPrevEdge nid1 nid2 = do
      PlotState{..} <- get
      let nid1' = dataNodeMap M.! nid1
      let nid2' = opNodeMap M.! nid2
      put $
        PlotState
          { plotGraph = G.insertEdge nid1' nid2' plotGraph
          , dataNodeMap = dataNodeMap
          , opNodeMap = opNodeMap
          }

    -- Insert new edge to connect the op nodes to their respective data nodes 
    insertOpEdge :: G.NodeId -> G.NodeId -> PlotM ()
    insertOpEdge old new = do
      PlotState{..} <- get
      put $
        PlotState
          { plotGraph = G.insertEdge new (dataNodeMap M.! old) plotGraph
          , dataNodeMap = dataNodeMap
          , opNodeMap = opNodeMap
          }

    insertEdges :: PlotM ()
    insertEdges = do
      mapM_ (uncurry insertPrevEdge) (G.edges cGraph)
      opMap <- gets opNodeMap
      mapM_ (uncurry insertOpEdge) (M.toList opMap)

----------------------------
-- * Printing a PlotGraph --
----------------------------

graphToDot :: PlotGraph -> Dot
graphToDot graph = nodes <> edges
  where
    toDotId :: G.NodeId -> DotId
    toDotId = DotId . G.nodeIdToInt

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

-- | Convert a CGraph to a Dot diagram and render it as an SVG
plotGraphSVG :: FilePath -> E.CGraph -> IO FilePath
plotGraphSVG fp = plotSVG fp . graphToDot . toPlotGraph
