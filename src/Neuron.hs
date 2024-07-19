module Neuron
  -- Runners
  ( runGraphMaker
  , execGraphMaker
  -- Neural network constructors
  , value
  , randomValue
  , neuron
  , layer
  , mlp
  )where

import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Engine as E
import qualified Graph as G

type GraphMaker a = RandT StdGen (State E.BPGraph) a

runGraphMaker :: GraphMaker a -> IO (a, E.BPGraph)
runGraphMaker m = do
  gen <- getStdGen
  pure $ runState (evalRandT m gen) G.empty

execGraphMaker :: GraphMaker a -> IO E.BPGraph
execGraphMaker m = do
  gen <- getStdGen
  pure (flip execState G.empty $ evalRandT m gen)

value :: String -> Double -> GraphMaker G.NodeId
value label val = lift (E.value label val)

-- | Generate a random value between -1 and 1
randomValue :: GraphMaker G.NodeId
randomValue = do
  w <- getRandomR (-1.0, 1.0)
  lift $ E.value "" w

neuron :: [G.NodeId] -> GraphMaker (G.NodeId, [G.NodeId])
neuron inputs = do
  weights <- replicateM (length inputs) randomValue
  bias <- randomValue
  wx <- lift $ zipWithM (E.mul "") weights inputs
  activation <- lift $ E.sumNodes "" (bias : wx)
  nid <- lift $ E.tanh "" activation
  pure (nid, bias : weights)

layer :: Int -> [G.NodeId] -> GraphMaker ([G.NodeId], [G.NodeId])
layer n inputs = do
  neurons <- replicateM n (neuron inputs)
  pure (map fst neurons, concatMap snd neurons)

mlp :: [Int] -> [G.NodeId] -> GraphMaker ([G.NodeId], [G.NodeId])
mlp layerSizes inputs = snd <$> foldM go (inputs, ([], [])) layerSizes
  where
    go
      :: ([G.NodeId], ([G.NodeId], [G.NodeId]))
      -> Int
      -> GraphMaker ([G.NodeId], ([G.NodeId], [G.NodeId]))
    go (layerInputs, (prevNeurons, prevParams)) size = do
      (newNeurons, newParams) <- layer size layerInputs
      pure (newNeurons, (prevNeurons ++ newNeurons, prevParams <> newParams))
