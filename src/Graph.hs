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
  , terminal
  , parents
  -- Edge operations
  , addEdge
  , getEdges
  ) where

import qualified Data.Map as M

newtype NodeId = NodeId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

type Edge = (NodeId, NodeId)

data Graph a =
  Graph
    { counter :: Int
    , nodes :: M.Map NodeId a
    , edges :: [Edge]
    }
  deriving Eq

empty :: Graph a
empty =
  Graph
    { counter = 0
    , nodes = M.empty
    , edges = []
    }

---------------------
-- Node operations --
---------------------

addNode :: a -> Graph a -> (NodeId, Graph a)
addNode node Graph{..} = (nid, graph)
  where 
    nid = NodeId counter
    graph =
      Graph
        { counter = counter + 1
        , nodes = M.insert nid node nodes
        , edges = edges
        }

getNode :: NodeId -> Graph a -> Maybe a
getNode nid Graph{..} = M.lookup nid nodes 

setNode :: NodeId -> a -> Graph a -> Graph a
setNode nid node Graph{..} =
  Graph
    { counter = counter
    , nodes = M.adjust (const node) nid nodes
    , edges = edges
    }

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
  | getNode node1 graph == Nothing = graph
  | getNode node2 graph == Nothing = graph
  | otherwise = graph { edges = (node1, node2) : edges graph }

getEdges :: NodeId -> Graph a -> ([Edge], [Edge])
getEdges nid Graph{..} = (inEdges, outEdges)
  where
    inEdges = filter (\x -> snd x == nid) edges
    outEdges = filter (\x -> fst x == nid) edges
