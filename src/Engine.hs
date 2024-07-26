{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | A module for constructing and executing actions over a computational graph

module Engine

  -- * Types
  ( Node
  , Label
  , NodeOp(..)
  , CGraph
  , Payload(..)

  -- * AutoGradT
  , AutoGradT
  , runAutoGradT
  , evalAutoGradT
  , execAutoGradT

  -- * AutoGradT
  , AutoGrad
  , runAutoGrad
  , runAutoGradIO
  , evalAutoGrad
  , execAutoGrad

  -- * Manipulating the computational graph
  --
  -- ** Inserting
  -- | All nodes are initialised with a gradient of 0.0. All non-value nodes are
  -- also initialized with a value of 0.0.

  , withLabel

  -- *** Primitive operation inserts
  -- | These operations are directly supported by the computational graph (are a
  -- single node)
  , value
  , randomValue
  , add
  , shift
  , sum
  , mul
  , scale
  , prod
  , pow
  , relu
  , tanh

  -- *** Composite operation inserts
  , neg
  , sub
  , recip
  , div

  -- ** Getting
  , getNodePayload
  , getNodeOp
  , getNodeLabel
  , getNodeVal
  , getNodeGrad
  , getNodes
  , getParentNodes

  -- ** Setting
  , setNodePayload
  , setNodeLabel
  , setNodeVal
  , setNodeVals
  , setNodeGrad
  , setNodeGrads
  , zeroGrad

  -- * Graph operations
  , forward
  , backprop
  )

where

import qualified Prelude as P (sum, tanh)
import Prelude hiding (sum, tanh, recip, div)

import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Functor.Identity
import qualified Data.Map as M

import qualified Graph as G

-----------
-- Types --
-----------

-- | A node in the computational graph
type Node = G.NodeId

-- | A descriptor for each node in the computational graph
type Label = String

-- | Operations that a node can represent in the computational graph
data NodeOp
  = ValueOp
  | AddOp
  | ShiftOp Double
  | MulOp
  | ScaleOp Double
  | PowOp Double
  | ReLUOp
  | TanhOp
  deriving (Eq)

-- | Payload for each node in the computational graph
data Payload =
  Payload
    { nodeType :: NodeOp
    , nodeLabel :: Maybe Label
    , nodeVal :: Double
    , nodeGrad :: Double
    }
  deriving (Eq)

-- | The computational graph
type CGraph = G.Graph Payload

---------------
-- AutoGradT --
---------------

-- | The AutoGradT monad transformer for constructing and executing actions over
-- the computational graph. AutoGradT includes:
--
--   * A state monad to store and  access to the computational graph
--
--   * A random monad for generating random weights and biases
type AutoGradT m a = StateT CGraph (RandT StdGen m) a

-- | Run an AutoGradT action, returning the result and the computational graph
runAutoGradT :: Monad m => AutoGradT m a -> StdGen -> m (a, CGraph)
runAutoGradT ma = evalRandT (runStateT ma G.empty)

-- | Evaluate an AutoGradT action, returning the result
evalAutoGradT :: Monad m => AutoGradT m a -> StdGen -> m a
evalAutoGradT ma = evalRandT (evalStateT ma G.empty)

-- | Execute an AutoGradT action, returning the computational graph
execAutoGradT :: Monad m => AutoGradT m a -> StdGen -> m CGraph
execAutoGradT ma = evalRandT (execStateT ma G.empty)

--------------
-- AutoGrad --
--------------

-- | The AutoGrad monad for constructing and executing pure actions over the
-- computational graph
type AutoGrad a = AutoGradT Identity a

-- | Run an AutoGrad action, returning the result and the computational graph
runAutoGrad :: AutoGrad a -> StdGen -> (a, CGraph)
runAutoGrad m = evalRand (runStateT m G.empty)

-- | Run an AutoGrad action, returning the result and the computational graph.
-- Also samples a StdGen from the IO monad 
runAutoGradIO :: AutoGrad a -> IO (a, CGraph)
runAutoGradIO m = runAutoGrad m <$> getStdGen

-- | Evaluate an AutoGrad action, returning the result
evalAutoGrad :: AutoGrad a -> StdGen -> a
evalAutoGrad m = evalRand (evalStateT m G.empty)

-- | Execute an AutoGrad action, returning the computational graph
execAutoGrad :: AutoGrad a -> StdGen -> CGraph
execAutoGrad m = evalRand (execStateT m G.empty)

------------------------------------------
-- Manipulating the computational graph --
------------------------------------------

-- | By construction, the computational graph can only ever be added to. One can
-- only ever insert new nodes and edges, but never remove them. This ensures
-- that if you have a NodeId, the node is guaranteed to still be in the graph.
-- One can, however, modify the payload of existing nodes (e.g. in a backprop
-- step)

---------------
-- Inserting --
---------------

-- | Insert a node into the computational graph
insertNode :: Monad m => Payload -> AutoGradT m Node
insertNode payload = do
  graph <- get
  let (nid, graph') = G.insertNode payload graph
  put graph'
  pure nid

-- | Insert an edge between two existing nodes in the computational graph
insertEdge :: Monad m => Node -> Node -> AutoGradT m ()
insertEdge nid1 nid2 = modify (G.insertEdge nid1 nid2)

-- | Add a label to a node
withLabel :: Monad m => Label -> AutoGradT m Node -> AutoGradT m Node
withLabel label action = do
  nid <- action
  setNodeLabel nid label
  pure nid

-- | Insert a node with no label, zero value and zero gradient
insertZeroNode :: Monad m => NodeOp -> AutoGradT m Node
insertZeroNode op = insertNode (Payload op Nothing 0.0 0.0)

-----------------------
-- Primitive inserts --
-----------------------

-- | Insert a value node into the computational graph
value :: Monad m => Double -> AutoGradT m Node
value val = insertNode (Payload ValueOp Nothing val 0.0)

-- | Insert a random value between -1 and 1
randomValue :: Monad m => AutoGradT m Node
randomValue = getRandomR (-1.0, 1.0) >>= value

-- | Insert a binary addition node into the computational graph
add :: Monad m => Node -> Node -> AutoGradT m Node 
add a b = do
  nid <- insertZeroNode AddOp
  insertEdge a nid
  insertEdge b nid
  pure nid

-- | Insert a shift node into the computational graph. Shift nodes take a single
-- input and add a constant value to it
shift :: Monad m => Double -> Node -> AutoGradT m Node 
shift c a = do
  nid <- insertZeroNode (ShiftOp c)
  insertEdge a nid
  pure nid

-- | Insert an addition node into the computational graph
sum :: Monad m => [Node] -> AutoGradT m Node 
sum inputs = do
  nid <- insertZeroNode AddOp
  mapM_ (flip insertEdge nid) inputs
  pure nid

-- | Insert a binary multiplication node into the computational graph
mul :: Monad m => Node -> Node -> AutoGradT m Node
mul a b = do
  nid <- insertZeroNode MulOp
  insertEdge a nid
  insertEdge b nid
  pure nid

-- | Insert a scale node into the computational graph. Scale nodes take a single
-- input and multiply it by a constant value
scale :: Monad m => Double -> Node -> AutoGradT m Node
scale c a = do
  nid <- insertZeroNode (ScaleOp c)
  insertEdge a nid
  pure nid

-- | Insert a multiplication node into the computational graph
prod :: Monad m => [Node] -> AutoGradT m Node 
prod inputs = do
  nid <- insertZeroNode MulOp
  mapM_ (flip insertEdge nid) inputs
  pure nid

-- | Insert a power node into the computational graph. Power nodes take a single
-- input and raise it to the power of a constant exponent
pow :: Monad m => Double -> Node -> AutoGradT m Node
pow e a = do
  nid <- insertZeroNode (PowOp e)
  insertEdge a nid
  pure nid

-- | Insert a ReLU node into the computational graph
relu :: Monad m => Node -> AutoGradT m Node
relu a = do
  nid <- insertZeroNode ReLUOp
  insertEdge a nid
  pure nid

-- | Insert a tanh node into the computational graph
tanh :: Monad m => Node -> AutoGradT m Node
tanh a = do
  nid <- insertZeroNode TanhOp
  insertEdge a nid
  pure nid

-----------------------
-- Composite inserts --
-----------------------

-- | Insert a negation: mul (value -1))
neg :: Monad m => Node -> AutoGradT m Node
neg a = do
  minusOne <- value (-1.0)
  mul a minusOne

-- | Insert a subtraction: add a (neg b)
sub :: Monad m => Node -> Node -> AutoGradT m Node
sub a b = do
  negB <- neg b
  add a negB

-- | Insert a reciprocal: pow (-1)
recip :: Monad m => Node -> AutoGradT m Node
recip = pow (-1.0)

-- | Insert a division: mul a (recip b)
div :: Monad m => Node -> Node -> AutoGradT m Node
div a b = do
  recipB <- recip b
  mul a recipB

-------------
-- Getting --
-------------

-- | Get the payload of a node
getNodePayload :: Monad m => Node -> AutoGradT m Payload
getNodePayload nid = gets (G.getNode nid)

-- | Get a node's operation
getNodeOp :: Monad m => Node -> AutoGradT m NodeOp
getNodeOp = fmap nodeType . getNodePayload

-- | Get a node's label
getNodeLabel :: Monad m => Node -> AutoGradT m (Maybe Label)
getNodeLabel = fmap nodeLabel . getNodePayload

-- | Get a node's value
getNodeVal :: Monad m => Node -> AutoGradT m Double
getNodeVal = fmap nodeVal . getNodePayload

-- | Get a node's gradient
getNodeGrad :: Monad m => Node -> AutoGradT m Double
getNodeGrad = fmap nodeGrad . getNodePayload

-- | Get all nodes in the computational graph
getNodes :: Monad m => AutoGradT m [Node]
getNodes = gets (M.keys . G.nodes)

-- | Get all parent nodes of a node in the computational graph
getParentNodes :: Monad m => Node -> AutoGradT m [Node]
getParentNodes nid = gets (G.parentNodes nid)

-------------
-- Setting --
-------------

-- | Set a node's payload
setNodePayload :: Monad m => Node -> Payload -> AutoGradT m ()
setNodePayload nid payload = modify (G.setNode nid payload)

-- | Set a node's label
setNodeLabel :: Monad m => Node -> Label -> AutoGradT m ()
setNodeLabel nid label = do
  payload <- getNodePayload nid
  setNodePayload nid (payload { nodeLabel = Just label })

-- | Set a node's value
setNodeVal :: Monad m => Node -> Double -> AutoGradT m ()
setNodeVal nid val = do
  payload <- getNodePayload nid
  setNodePayload nid (payload { nodeVal = val })

-- | Set the values for a list of nodes
setNodeVals :: Monad m => [(Node, Double)] -> AutoGradT m ()
setNodeVals = mapM_ (uncurry setNodeVal)

-- | Set a node's gradient
setNodeGrad :: Monad m => Node -> Double -> AutoGradT m ()
setNodeGrad nid grad = do
  payload <- getNodePayload nid
  setNodePayload nid (payload { nodeGrad = grad })

-- | Set the gradients for a list of nodes
setNodeGrads :: Monad m => [(Node, Double)] -> AutoGradT m ()
setNodeGrads = mapM_ (uncurry setNodeGrad)

-- | Set all gradients in the computational graph to zero
zeroGrad :: Monad m => AutoGradT m ()
zeroGrad = do
  nodes <- getNodes
  setNodeGrads (zip nodes (repeat 0.0))

----------------------
-- Graph operations --
----------------------

-- | Perform a forward pass in the computational graph
forward :: Monad m => AutoGradT m ()
forward = gets G.terminalNodes >>= mapM_ forwardNode
  where
    -- | Perform a forward pass on a single node
    forwardNode :: Monad m => Node -> AutoGradT m ()
    forwardNode nid = do
      getNodeOp nid >>= \case
        ValueOp -> pure ()
        AddOp -> do
          parents <- getParentNodes nid
          mapM_ forwardNode parents
          vals <- mapM getNodeVal parents
          setNodeVal nid (P.sum vals)
        ShiftOp c -> do
          getParentNodes nid >>= \case
            [pid] -> do
              forwardNode pid
              val <- getNodeVal pid
              setNodeVal nid (val + c)
            x -> error $ "ShiftOp node has " <> show (length x) <> " parents"
        MulOp -> do
          parents <- getParentNodes nid
          mapM_ forwardNode parents
          vals <- mapM getNodeVal parents
          setNodeVal nid (product vals)
        ScaleOp c -> do
          getParentNodes nid >>= \case
            [pid] -> do
              forwardNode pid
              val <- getNodeVal pid
              setNodeVal nid (val * c)
            x -> error $ "ScaleOp node has " <> show (length x) <> " parents"
        PowOp e -> do
          getParentNodes nid >>= \case
            [pid] -> do
              forwardNode pid
              val <- getNodeVal pid
              setNodeVal nid (val ** e)
            x -> error $ "PowOp node has " <> show (length x) <> " parents"
        ReLUOp -> do
          getParentNodes nid >>= \case
            [pid] -> do
              forwardNode pid
              val <- getNodeVal pid
              setNodeVal nid (max val 0)
            x -> error $ "ReLU node has " <> show (length x) <> " parents"
        TanhOp -> do
          getParentNodes nid >>= \case
            [pid] -> do
              forwardNode pid
              val <- getNodeVal pid
              setNodeVal nid (P.tanh val)
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

-- | Perform a backpropagation pass starting from a particular node in the
-- computational graph
backprop :: Monad m => Node -> AutoGradT m ()
backprop node = do
  zeroGrad
  setNodeGrad node 1.0
  graph <- get
  mapM_ backpropNode (G.topoSort graph node)
  where
    -- | Perform a backpropagation pass on a single node
    backpropNode :: Monad m => Node -> AutoGradT m ()
    backpropNode nid = do
      grad <- getNodeGrad nid
      getNodeOp nid >>= \case
        ValueOp -> pure ()
        AddOp -> do
          parents <- getParentNodes nid
          forM_ parents $ \pid -> do
            pgrad <- getNodeGrad pid
            setNodeGrad pid (pgrad + grad)
        ShiftOp _ -> do
          getParentNodes nid >>= \case
            [pid] -> do
              pgrad <- getNodeGrad pid
              setNodeGrad pid (pgrad + grad)
            x -> error $ "ShiftOp node has " <> show (length x) <> " parents"
        MulOp -> do
          parents <- getParentNodes nid
          parentVals <- mapM getNodeVal parents
          let rests = map snd (oneVsRest parentVals)
          forM_ (zip parents rests) $ \(pid, rest) -> do
            pgrad <- getNodeGrad pid
            setNodeGrad pid (pgrad + grad * product rest)
        ScaleOp c -> do
          getParentNodes nid >>= \case
            [pid] -> do
              pgrad <- getNodeGrad pid
              setNodeGrad pid (pgrad + grad * c)
            x -> error $ "ScaleOp node has " <> show (length x) <> " parents"
        PowOp e -> do
          getParentNodes nid >>= \case
            [pid] -> do
              pval <- getNodeVal pid
              pgrad <- getNodeGrad pid
              setNodeGrad pid (pgrad + grad * e * (pval ** (e - 1)))
            x -> error $ "ScaleOp node has " <> show (length x) <> " parents"
        ReLUOp -> do
          getParentNodes nid >>= \case
            [pid] -> do
              pval <- getNodeVal pid
              pgrad <- getNodeGrad pid
              when (pval > 0) $ setNodeGrad pid (pgrad + grad)
            x -> error $ "ReLU node has " <> show (length x) <> " parents"
        TanhOp -> do
          getParentNodes nid >>= \case
            [pid] -> do
              pval <- getNodeVal pid
              pgrad <- getNodeGrad pid
              setNodeGrad pid (pgrad + grad * (1 - P.tanh pval ** 2))
            x -> error $ "Tanh node has " <> show (length x) <> " parents"
