{-# LANGUAGE RecordWildCards #-}

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
  , getParamsNeuron
  , getParamsLayer
  , getParamsNetwork

  -- * Calling the network
  , neuronCall
  , layerCall
  , networkCall

  -- * Loss functions
  , mseLoss

  -- * Training the network
  , train

  ) where

import Control.Monad (forM, replicateM, zipWithM, foldM)
import qualified Engine as E
import qualified Graph as G

-----------
-- Types --
-----------

-- | Parameters of a single neuron 
data NeuronParams = 
  NeuronParams
    { weights :: [G.NodeId]
    , bias :: G.NodeId
    }

-- | Parameters for every neuron in a layer
type LayerParams = [NeuronParams]

-- | Parameters for every layer in a network
type NetworkParams = [LayerParams]

-- | A single neuron
data Neuron = 
  Neuron
    { neuronInputs :: [G.NodeId] -- ^ The input nodes
    , neuronParams :: NeuronParams -- ^ The parameters
    , neuronOutput :: G.NodeId -- ^ The output node
    }

-- | A layer
data Layer =
  Layer
    { layerInputs :: [G.NodeId] -- ^ The input nodes (shared with each neuron)
    , layerNeurons :: [Neuron] -- ^ The parameters
    , layerOutputs :: [G.NodeId] -- ^ The output nodes (one for each neuron)
    }

-- | A Multi-Layer Perceptron neural network
data Network =
  Network
    { networkInputs :: [G.NodeId] -- ^ The input nodes
    , networkLayers :: [Layer] -- ^ The parameters
    , networkOutputs :: [G.NodeId] -- ^ The output nodes
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
neuronCall :: Monad m => [G.NodeId] -> NeuronParams -> E.AutoGradT m Neuron
neuronCall inputs neuron = do
  wx <- zipWithM (E.mul "") (weights neuron) inputs
  activation <- E.sum "" (bias neuron : wx)
  output <- E.tanh "" activation
  pure $
    Neuron
      { neuronInputs = inputs
      , neuronParams = neuron
      , neuronOutput = output
      }

-- | Create a layer from it's inputs and parameters
layerCall :: Monad m => [G.NodeId] -> LayerParams -> E.AutoGradT m Layer
layerCall inputs layer = do
  neurons <- mapM (neuronCall inputs) layer
  pure $
    Layer
      { layerInputs = inputs
      , layerNeurons = neurons
      , layerOutputs = map neuronOutput neurons
      }

-- | Create a network from it's inputs and parameters
networkCall :: Monad m => [G.NodeId] -> NetworkParams -> E.AutoGradT m Network
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
      => ([G.NodeId], [Layer])
      -> LayerParams
      -> E.AutoGradT m ([G.NodeId], [Layer])
    makeLayer (layerInputs, prevLayers) params = do
      layer <- layerCall layerInputs params
      pure (layerOutputs layer, prevLayers <> [layer])

--------------------
-- Loss functions --
--------------------

-- | The Mean Squared Error (MSE) loss function, defined as:
--  \[
--      MSE = \frac{1}{n} \sum_{i=0}^N \sum_{j=0}^M (y_{true} - y_{pred})^2
--  \]
--  where
--
--      * \(N\) is the number of samples
--
--      * \(M\) is the number of labels
--
--      * \(y_{true}\) is the true label
--
--      * \(y_{pred}\) is the predicted label
mseLoss
  :: Monad m
  => [[Double]] -- ^ The input samples
  -> [[Double]] -- ^ The input labels
  -> NetworkParams -- ^ The network parameters
  -> E.AutoGradT m G.NodeId
mseLoss xs ys networkParams = do
  -- For each sample, make relevant input nodes and call the network on them
  yPreds <- forM xs $ \x -> do
    inputs <- mapM (E.value "") x
    network <- networkCall inputs networkParams
    pure (networkOutputs network)
  -- Make an input node for each label
  yTrues <- forM ys $ \y ->
    mapM (E.value "") y
  -- Add nodes to compute the loss
  a <- forM (zip yTrues yPreds) $ \(yTrue, yPred) -> do
    subs <- forM (zip yTrue yPred) $ \(yt, yp) -> do
      E.sub "" yt yp
    E.sum "" subs
  b <- mapM (\x -> E.mul "" x x) a
  E.sum "" b

--------------------------
-- Training the network --
--------------------------

-- | Get a list of parameter nodes for a neuron
getParamsNeuron :: NeuronParams -> [G.NodeId]
getParamsNeuron NeuronParams{..} = bias : weights

-- | Get a list of parameter nodes for a layer
getParamsLayer :: LayerParams -> [G.NodeId]
getParamsLayer = concatMap getParamsNeuron

-- | Get a list of parameter nodes for a network
getParamsNetwork :: NetworkParams -> [G.NodeId]
getParamsNetwork = concatMap getParamsLayer

-- | Increment the value of a node
nudge :: Monad m => Double -> G.NodeId -> E.AutoGradT m ()
nudge lr nid = do
  payload <- E.getNodePayload nid
  let nodeVal' = E.nodeVal payload - lr * E.nodeGrad payload
  E.setNodeVal nid nodeVal'

-- | Train the network using the MSE loss function
train
  :: Monad m
  => Double
  -- ^ The learning rate
  -> Int
  -- ^ The number of epochs
  -> [[Double]]
  -- ^ The input samples
  -> [[Double]]
  -- ^ The input labels
  -> NetworkParams
  -- ^ The network parameters
  -> E.AutoGradT m [Double]
train lr epochs xs ys networkParams = do
  loss <- mseLoss xs ys networkParams
  replicateM epochs $ do
    E.forward
    E.backprop loss
    mapM_ (nudge lr) (getParamsNetwork networkParams)
    E.getNodeVal loss
