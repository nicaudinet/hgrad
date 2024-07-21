{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph
  -- Types
  ( NodeId
  , Graph (nodes, edges)
  -- Constructors
  , empty
  -- Node operations
  , addNode
  , getNode
  , setNode
  , setNodes
  , terminal
  , parents
  -- Edge operations
  , addEdge
  , getEdges
  -- Convenience
  , idToInt
  ) where

import qualified Data.Map as M

newtype NodeId = NodeId Int
  deriving (Show, Eq, Ord)

type Edge = (NodeId, NodeId)

data Graph a =
  Graph
    { nodes :: M.Map NodeId a
    , edges :: [Edge]
    }
  deriving Eq

empty :: Graph a
empty =
  Graph
    { nodes = M.empty
    , edges = []
    }

---------------------
-- Node operations --
---------------------

idToInt :: NodeId -> Int
idToInt (NodeId x) = x

addNode :: a -> Graph a -> (NodeId, Graph a)
addNode node Graph{..} = (nid, graph)
  where 
    nid = NodeId (length nodes)
    graph =
      Graph
        { nodes = M.insert nid node nodes
        , edges = edges
        }

-- | Return the node associated with a NodeId
-- Although this is a partial function, it's safe because:
--   1. The user can only get a NodeId through addNode
--   2. There's no way to update NodeIds in the graph API
--   3. There's no way to delete NodeIds in the graph API
getNode :: NodeId -> Graph a -> a
getNode nid Graph{..} = nodes M.! nid

setNode :: NodeId -> a -> Graph a -> Graph a
setNode nid node Graph{..} =
  Graph
    { nodes = M.adjust (const node) nid nodes
    , edges = edges
    }

setNodes :: Graph a -> [(NodeId, a)]-> Graph a
setNodes = foldr (uncurry setNode)

-- | Return a list of terminal nodes (nodes with no outgoing edges)
terminal :: Graph a -> [NodeId]
terminal Graph{..} =
  let targets = map fst edges
   in filter (`notElem` targets) (M.keys nodes)

parents :: NodeId -> Graph a -> [NodeId]
parents nid Graph{..} = map fst (filter ((== nid) . snd) edges)

---------------------
-- Edge operations --
---------------------

addEdge :: Eq a => NodeId -> NodeId -> Graph a -> Graph a 
addEdge node1 node2 graph
  | node1 == node2 = graph
  | otherwise = graph { edges = (node1, node2) : edges graph }

getEdges :: NodeId -> Graph a -> ([Edge], [Edge])
getEdges nid Graph{..} = (inEdges, outEdges)
  where
    inEdges = filter (\x -> snd x == nid) edges
    outEdges = filter (\x -> fst x == nid) edges
