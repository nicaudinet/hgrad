{-# LANGUAGE RecordWildCards #-}

module Neuron
  -- Types
  ( Network(..)
  -- Runners
  , runGraphMaker
  , execGraphMaker
  -- Neural network constructors
  , value
  , randomValue
  -- Initialize network parameters 
  , neuronInit
  , layerInit
  , networkInit
  -- Call the network
  , neuronCall
  , layerCall
  , networkCall
  -- Loss functions
  , mseLoss
  ) where

import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Engine as E
import qualified Graph as G

type GraphMaker a = RandT StdGen (State E.BPGraph) a

data NeuronParams = 
  NeuronParams
    { weights :: [G.NodeId]
    , bias :: G.NodeId
    }

type LayerParams = [NeuronParams]

type NetworkParams = [LayerParams]

data Neuron = 
  Neuron
    { neuronInputs :: [G.NodeId]
    , neuronParams :: NeuronParams
    , neuronOutput :: G.NodeId
    }

data Layer =
  Layer
    { layerInputs :: [G.NodeId]
    , layerNeurons :: [Neuron]
    , layerOutputs :: [G.NodeId]
    }

data Network =
  Network
    { networkInputs :: [G.NodeId]
    , networkLayers :: [Layer]
    , networkOutputs :: [G.NodeId]
    }

---------------------------
-- Convenience functions --
---------------------------

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

neuronInit :: Int -> GraphMaker NeuronParams
neuronInit size = do
  weights <- replicateM size randomValue
  bias <- randomValue
  pure (NeuronParams weights bias)

layerInit :: Int -> Int -> GraphMaker LayerParams
layerInit inputSize layerSize = replicateM layerSize (neuronInit inputSize)

networkInit :: Int -> [Int] -> GraphMaker NetworkParams
networkInit inputSize layerSizes =
  zipWithM layerInit (inputSize:layerSizes) layerSizes

neuronCall :: [G.NodeId] -> NeuronParams -> GraphMaker Neuron
neuronCall inputs neuron = do
  wx <- lift $ zipWithM (E.mul "") (weights neuron) inputs
  activation <- lift $ E.sumNodes "" (bias neuron : wx)
  output <- lift $ E.tanh "" activation
  pure $
    Neuron
      { neuronInputs = inputs
      , neuronParams = neuron
      , neuronOutput = output
      }

layerCall :: [G.NodeId] -> LayerParams -> GraphMaker Layer
layerCall inputs layer = do
  neurons <- mapM (neuronCall inputs) layer
  pure $
    Layer
      { layerInputs = inputs
      , layerNeurons = neurons
      , layerOutputs = map neuronOutput neurons
      }

networkCall :: [G.NodeId] -> NetworkParams -> GraphMaker Network
networkCall inputs network = do
  (outputs, layers) <- foldM go (inputs, []) network
  pure $
    Network
      { networkInputs = inputs
      , networkLayers = layers
      , networkOutputs = outputs
      }
  where
    go :: ([G.NodeId], [Layer]) -> LayerParams -> GraphMaker ([G.NodeId], [Layer])
    go (layerInputs, prevLayers) params = do
      layer <- layerCall layerInputs params
      pure (layerOutputs layer, prevLayers <> [layer])

mseLoss :: [[Double]] -> [[Double]] -> NetworkParams -> GraphMaker G.NodeId
mseLoss xs ys networkParams = do
  -- For each sample, make relevant input nodes and call the network on them
  yPreds <- forM xs $ \x -> do
    inputs <- mapM (value "") x
    network <- networkCall inputs networkParams
    pure (networkOutputs network)
  -- Make an input node for each label
  yTrues <- forM ys $ \y ->
    mapM (value "") y
  -- Add nodes to compute the loss
  a <- lift $ forM (zip yTrues yPreds) $ \(yTrue, yPred) -> do
    subs <- forM (zip yTrue yPred) $ \(yt, yp) -> do
      E.sub "" yt yp
    E.sumNodes "" subs
  b <- lift $ mapM (\x -> E.mul "" x x) a
  lift $ E.sumNodes "" b
