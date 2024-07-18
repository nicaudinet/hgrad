{-# LANGUAGE RecordWildCards #-}

module Engine
  -- types
  ( BPGraph
  , GraphNode(..)
  -- constructors
  , makeGraph
  , value
  , add
  , mul
  , tanh
  -- evaluation
  , forward
  , backprop
  -- plotting
  , plotGraphPng
  )
where

import qualified Prelude as P (tanh)
import Prelude hiding (tanh)

import Control.Monad (forM, forM_)
import Control.Monad.Trans.State
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Graph as G
import Graph.Dot

-----------
-- Types --
-----------

type Label = String

-- | Types of nodes
data NodeType
  = ValueNode
  | AddNode
  | MulNode
  | TanhNode
  deriving (Eq)

-- | Data type for the contents of a graph node
data GraphNode =
  GraphNode
    { nodeType :: NodeType
    , nodeLabel :: Label
    , nodeVal :: Double
    , nodeGrad :: Double
    }
  deriving (Eq)

-- | Back Propagation Graph
type BPGraph = G.Graph GraphNode

----------------------
-- Graph Primitives --
----------------------

makeGraph :: State (G.Graph a) b -> G.Graph a
makeGraph = flip execState G.empty

addNode :: a -> State (G.Graph a) G.NodeId
addNode node = do
  graph <- get
  let (nid, graph') = G.addNode node graph
  put graph'
  pure nid

addEdge :: Eq a => G.NodeId -> G.NodeId -> State (G.Graph a) ()
addEdge nid1 nid2 = modify (G.addEdge nid1 nid2)

------------------
-- Constructors --
------------------

value :: Label -> Double -> State BPGraph G.NodeId
value label val = addNode (GraphNode ValueNode label val 0.0)

add :: Label -> G.NodeId -> G.NodeId -> State BPGraph G.NodeId 
add label a b = do
  nid <- addNode (GraphNode AddNode label 0.0 0.0)
  addEdge a nid
  addEdge b nid
  pure nid

mul :: Label -> G.NodeId -> G.NodeId -> State BPGraph G.NodeId
mul label a b = do
  nid <- addNode (GraphNode MulNode label 0.0 0.0)
  addEdge a nid
  addEdge b nid
  pure nid

tanh :: Label -> G.NodeId -> State BPGraph G.NodeId
tanh label a = do
  nid <- addNode (GraphNode TanhNode label 0.0 0.0)
  addEdge a nid
  pure nid

----------------
-- Evaluation --
----------------

forward :: BPGraph -> BPGraph
forward graph = execState (mapM_ go (G.terminal graph)) graph
  where
    go :: G.NodeId -> State BPGraph ()
    go nid = do
      GraphNode op label _ grad <- fromJust <$> gets (G.getNode nid)
      case op of
        ValueNode -> pure ()
        AddNode -> do
          parents <- gets (G.parents nid)
          vals <- forM parents $ \pid -> do
            go pid
            GraphNode _ _ val _ <- fromJust <$> gets (G.getNode pid)
            return val
          let newNode = GraphNode op label (sum vals) grad
          modify (G.setNode nid newNode)
        MulNode -> do
          parents <- gets (G.parents nid)
          vals <- forM parents $ \pid -> do
            go pid
            GraphNode _ _ val _ <- fromJust <$> gets (G.getNode pid)
            return val
          let newNode = GraphNode op label (product vals) grad
          modify (G.setNode nid newNode)
        TanhNode -> do
          parents <- gets (G.parents nid)
          case parents of
            [pid] -> do
              go pid
              GraphNode _ _ val _ <- fromJust <$> gets (G.getNode pid)
              let newNode = GraphNode op label (P.tanh val) grad
              modify (G.setNode nid newNode)
            x -> error $ "Tanh node has " <> show (length x) <> " parents"

-- | Given a list, return a list of each element and all other elements of that
-- list
oneVsRest :: [a] -> [(a, [a])]
oneVsRest [] = []
oneVsRest as = reverse . fst $ foldr go ([], as) as
  where
    go :: a -> ([(a, [a])], [a]) -> ([(a, [a])], [a])
    go _ (xs, []) = (xs, [])
    go _ (xs, y:ys) = ((y,ys) : xs, ys <> [y])

backprop :: BPGraph -> BPGraph
backprop graph = flip execState graph $ do
  let terminalNodes = G.terminal graph
  mapM_ oneGrad terminalNodes
  mapM_ go (G.terminal graph)
  where
    oneGrad :: G.NodeId -> State BPGraph ()
    oneGrad nid = do
      GraphNode op label val _ <- fromJust <$> gets (G.getNode nid)
      let newNode = GraphNode op label val 1.0
      modify (G.setNode nid newNode)

    go :: G.NodeId -> State BPGraph ()
    go nid = do
      GraphNode op _ _ grad <- fromJust <$> gets (G.getNode nid)
      case op of
        ValueNode -> pure ()
        AddNode -> do
          parents <- gets (G.parents nid)
          forM_ parents $ \pid -> do
            GraphNode pop plabel pval pgrad <- fromJust <$> gets (G.getNode pid)
            let newNode = GraphNode pop plabel pval (pgrad + grad)
            modify (G.setNode pid newNode)
            go pid
        MulNode -> do
          parents <- gets (G.parents nid)
          parentVals <- forM parents $ \pid -> do
            GraphNode _ _ val _ <- fromJust <$> gets (G.getNode pid)
            return val
          let rests = map snd (oneVsRest parentVals)
          forM_ (zip parents rests) $ \(pid, rest) -> do
            GraphNode pop plabel pval pgrad <- fromJust <$> gets (G.getNode pid)
            let newNode = GraphNode pop plabel pval (pgrad + grad * product rest)
            modify (G.setNode pid newNode)
            go pid
        TanhNode -> do
          parents <- gets (G.parents nid)
          case parents of
            [pid] -> do
              GraphNode pop plabel pval pgrad <- fromJust <$> gets (G.getNode pid)
              let newGrad = pgrad + grad * (1 - P.tanh pval ** 2)
                  newNode = GraphNode pop plabel pval newGrad
              modify (G.setNode pid newNode)
              go pid
            x -> error $ "Tanh node has " <> show (length x) <> " parents"

--------------
-- Plotting --
--------------

type NodeMap = M.Map G.NodeId G.NodeId
type TransformState = (PlotGraph, NodeMap, NodeMap)

showNodeType :: NodeType -> String
showNodeType ValueNode = error "ValueNode is not an operation"
showNodeType AddNode = "+"
showNodeType MulNode = "*"
showNodeType TanhNode = "tanh"

-- | Convert a BPGraph to a PlotGraph which:
--   - Categorize nodes into data nodes and operation nodes
--   - Split intermediate nodes in BPGraph into an operation and a data node
toPlotGraph :: BPGraph -> PlotGraph
toPlotGraph bpGraph =
  let (plotGraph, _, _) = execState createPlotGraph (G.empty, M.empty, M.empty)
   in plotGraph
  where
    createPlotGraph :: State TransformState ()
    createPlotGraph = addDataNodes >> addOpNodes >> addEdges

    addDataNode :: G.NodeId -> GraphNode -> State TransformState ()
    addDataNode nid (GraphNode _ label val grad) = do
      (plotGraph, dataNodeMap, opNodeMap) <- get
      let node = dataNode label val grad
          (nid', plotGraph') = G.addNode node plotGraph
          dataNodeMap' = M.insert nid nid' dataNodeMap
      put (plotGraph', dataNodeMap', opNodeMap)

    addDataNodes :: State TransformState ()
    addDataNodes = mapM_ (uncurry addDataNode) (M.toList (G.nodes bpGraph))

    addOpNode :: G.NodeId -> GraphNode -> State TransformState ()
    addOpNode nid GraphNode{..} =
      case nodeType of
        ValueNode -> pure ()
        _ -> do
          (plotGraph, dataNodeMap, opNodeMap) <- get
          let node = opNode (showNodeType nodeType)
              (nid', plotGraph') = G.addNode node plotGraph
              opNodeMap' = M.insert nid nid' opNodeMap
          put (plotGraph', dataNodeMap, opNodeMap')

    addOpNodes :: State TransformState ()
    addOpNodes = mapM_ (uncurry addOpNode) (M.toList (G.nodes bpGraph))

    -- Add edge that already existed in the back propagation graph
    addPrevEdge :: G.NodeId -> G.NodeId -> State TransformState ()
    addPrevEdge nid1 nid2 = do
      (plotGraph, dataNodeMap, opNodeMap) <- get
      let nid1' = fromJust $ M.lookup nid1 dataNodeMap
          nid2' = fromJust $ M.lookup nid2 opNodeMap
          plotGraph' = G.addEdge nid1' nid2' plotGraph
      put (plotGraph', dataNodeMap, opNodeMap)

    -- Add new edge to connect the op nodes to their respective data nodes 
    addOpEdge :: G.NodeId -> G.NodeId -> State TransformState ()
    addOpEdge oldOpNode newOpNode = do
      (plotGraph, dataNodeMap, opNodeMap) <- get
      let source = newOpNode
          target = fromJust $ M.lookup oldOpNode dataNodeMap
          plotGraph' = G.addEdge source target plotGraph
      put (plotGraph', dataNodeMap, opNodeMap)

    addEdges :: State TransformState ()
    addEdges = do
      mapM_ (uncurry addPrevEdge) (G.edges bpGraph)
      (_, _, opNodeMap) <- get
      mapM_ (uncurry addOpEdge) (M.toList opNodeMap)

-- | Plot a BPGraph to a PNG file using the Graphviz library
plotGraphPng :: FilePath -> BPGraph -> IO FilePath
plotGraphPng fp = plotPng fp . graphToDot . toPlotGraph  
