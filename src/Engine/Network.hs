{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

-- | Module for defining a simple Multi-Layer Perceptron network

module Engine.Network

  -- * Types
  ( NeuronParams(..)
  , LayerParams
  , NetworkParams
  , Neuron(..)
  , Layer(..)
  , Network(..)

  -- * Initializing the network
  , neuronInit
  , layerInit
  , networkInit

  -- * Calling the network
  , neuronCall
  , layerCall
  , networkCall

  -- * Re-exports from Engine
  , evalAutoGradT
  , evalAutoGrad

  ) where

import Control.Monad (replicateM, zipWithM, foldM)
import qualified Engine as E

import Engine (evalAutoGradT, evalAutoGrad) -- imported for re-export

-----------
-- Types --
-----------

-- | Parameters of a single neuron 
data NeuronParams = 
  NeuronParams
    { weights :: [E.Node]
    , bias :: E.Node
    }

-- | Parameters for every neuron in a layer
type LayerParams = [NeuronParams]

-- | Parameters for every layer in a network
type NetworkParams = [LayerParams]

-- | A single neuron
data Neuron = 
  Neuron
    { neuronInputs :: [E.Node] -- ^ The input nodes
    , neuronParams :: NeuronParams -- ^ The parameters of the neuron
    , neuronOutput :: E.Node -- ^ The output node
    }

-- | A layer
data Layer =
  Layer
    { layerInputs :: [E.Node] -- ^ The input nodes (shared with each neuron)
    , layerNeurons :: [Neuron] -- ^ The neurons in the layer
    , layerOutputs :: [E.Node] -- ^ The output nodes (one for each neuron)
    }

-- | A Multi-Layer Perceptron neural network
data Network =
  Network
    { networkInputs :: [E.Node] -- ^ The input nodes
    , networkLayers :: [Layer] -- ^ The layers in the network
    , networkOutputs :: [E.Node] -- ^ The output nodes
    }

------------------------------
-- Initializing the network --
------------------------------

-- | Initialize the parameters of a neuron (the weights and the bias)
neuronInit :: Monad m => Int -> E.AutoGradT m NeuronParams
neuronInit size = do
  weights <- replicateM size E.randomValue
  bias <- E.randomValue
  pure (NeuronParams weights bias)

-- | Initialize the parameters of a layer
layerInit :: Monad m => Int -> Int -> E.AutoGradT m LayerParams
layerInit inputSize layerSize = replicateM layerSize (neuronInit inputSize)

-- | Initialize the parameters of a network
networkInit :: Monad m => Int -> [Int] -> E.AutoGradT m NetworkParams
networkInit inputSize layerSizes =
  zipWithM layerInit (inputSize:layerSizes) layerSizes

-------------------------
-- Calling the network --
-------------------------

-- | Create a neuron from it's inputs and parameters
neuronCall :: Monad m => [E.Node] -> NeuronParams -> E.AutoGradT m Neuron
neuronCall inputs neuron = do
  wx <- zipWithM E.mul (weights neuron) inputs
  activation <- E.sum (bias neuron : wx)
  output <- E.tanh activation
  pure $
    Neuron
      { neuronInputs = inputs
      , neuronParams = neuron
      , neuronOutput = output
      }

-- | Create a layer from it's inputs and parameters
layerCall :: Monad m => [E.Node] -> LayerParams -> E.AutoGradT m Layer
layerCall inputs layer = do
  neurons <- mapM (neuronCall inputs) layer
  pure $
    Layer
      { layerInputs = inputs
      , layerNeurons = neurons
      , layerOutputs = map neuronOutput neurons
      }

-- | Create a network from it's inputs and parameters
networkCall :: Monad m => [E.Node] -> NetworkParams -> E.AutoGradT m Network
networkCall inputs network = do
  (outputs, layers) <- foldM makeLayer (inputs, []) network
  pure $
    Network
      { networkInputs = inputs
      , networkLayers = layers
      , networkOutputs = outputs
      }
  where
    makeLayer
      :: Monad m
      => ([E.Node], [Layer])
      -> LayerParams
      -> E.AutoGradT m ([E.Node], [Layer])
    makeLayer (layerInputs, prevLayers) params = do
      layer <- layerCall layerInputs params
      pure (layerOutputs layer, prevLayers <> [layer])
