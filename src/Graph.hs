{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A simple graph implementation for use as a computational graph

module Graph

  -- * Types
  ( NodeId(nodeIdToInt)
  , Graph (nodes, edges)

  -- * Constructors
  , empty

  -- * Node operations
  , insertNode
  , getNode
  , setNode
  , setNodes
  , terminalNodes
  , parentNodes
  , topoSort

  -- * Edge operations
  , insertEdge

  ) where

import Control.Monad.Trans.State
import GHC.Generics (Generic)
import qualified Data.Set as S
import qualified Data.Map as M

-- | The unique identifier attached to each node in the graph
newtype NodeId = NodeId { nodeIdToInt :: Int }
  deriving (Show, Eq, Ord, Generic)

-- | An edge in the graph (from, to)
type Edge = (NodeId, NodeId)

-- | The main graph type. Each node in the graph consists of an identifier
-- (NodeId) and a payload (which the Graph type parameterizes over).
data Graph a =
  Graph
    { nodes :: M.Map NodeId a
    , edges :: [Edge]
    }
  deriving (Eq, Generic)

-------------------
-- Constructors -- 
-------------------

-- | Create an empty graph
empty :: Graph a
empty =
  Graph
    { nodes = M.empty
    , edges = []
    }

---------------------
-- Node operations --
---------------------

-- | Insert a node into the graph
insertNode :: a -> Graph a -> (NodeId, Graph a)
insertNode node Graph{..} = (nid, graph)
  where 
    nid = NodeId (length nodes)
    graph =
      Graph
        { nodes = M.insert nid node nodes
        , edges = edges
        }

-- | Return the node associated with a NodeId. Note that although this is a
-- partial function, it's safe because:
--
--   1. The user can only get a NodeId through insertNode
--
--   2. There's no way to update NodeIds in the graph API
--
--   3. There's no way to delete NodeIds in the graph API
getNode :: NodeId -> Graph a -> a
getNode nid graph = (nodes graph) M.! nid

-- | Set the payload of a node in the graph
setNode :: NodeId -> a -> Graph a -> Graph a
setNode nid node Graph{..} =
  Graph
    { nodes = M.adjust (const node) nid nodes
    , edges = edges
    }

-- | Set the payloads for a list of nodes
setNodes :: Graph a -> [(NodeId, a)]-> Graph a
setNodes = foldr (uncurry setNode)

-- | Return a list of terminal nodes (nodes with no outgoing edges)
terminalNodes :: Graph a -> [NodeId]
terminalNodes Graph{..} =
  let targets = map fst edges
   in filter (`notElem` targets) (M.keys nodes)

-- | Return a list of parent nodes (nodes with an edge to the given node)
parentNodes :: NodeId -> Graph a -> [NodeId]
parentNodes nid Graph{..} = map fst (filter ((== nid) . snd) edges)

-- | Return a topologically sorted list of the nodes in the graph starting from
-- a particular node.
-- FIXME: This might break if the graph is not a DAG
topoSort :: Graph a -> NodeId -> [NodeId]
topoSort graph = snd . flip execState (S.empty, []). go
  where
    go :: NodeId -> State (S.Set NodeId, [NodeId]) ()
    go node = do
      (visited, _) <- get
      if S.member node visited
      then pure ()
      else do
        modify (\(v,s) -> (S.insert node v, s))
        mapM_ go (parentNodes node graph)
        modify (\(v,s) -> (v, node : s))

---------------------
-- Edge operations --
---------------------

-- | Insert an edge into the graph. Self-edges are not allowed, and will not be
-- added to the graph.
insertEdge :: Eq a => NodeId -> NodeId -> Graph a -> Graph a 
insertEdge node1 node2 graph
  | node1 == node2 = graph
  | otherwise = graph { edges = (node1, node2) : edges graph }
