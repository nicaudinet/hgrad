{-# LANGUAGE RecordWildCards #-}

module Engine.Network.Train
  
  -- * Loss functions
  ( Loss
  , mseLoss

  -- * Training
  , HyperParams(..)
  , train

  -- * Convenience functions
  , getNeuronParams
  , getLayerParams
  , getNetworkParams

  ) where

import Control.Monad (forM, replicateM)

import qualified Engine as E
import qualified Engine.Network as N


getNeuronParams :: N.NeuronParams -> [E.Node]
getNeuronParams N.NeuronParams{..} = bias : weights

getLayerParams :: N.LayerParams -> [E.Node]
getLayerParams = concatMap getNeuronParams

getNetworkParams :: N.NetworkParams -> [E.Node]
getNetworkParams = concatMap getLayerParams

--------------------
-- Loss functions --
--------------------

data Loss =
  Loss
    { lossOut :: E.Node
    , lossParams :: [E.Node]
    }

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
  -> N.NetworkParams -- ^ The network parameters
  -> E.AutoGradT m Loss
mseLoss xs ys networkParams = do
  -- For each sample, make relevant input nodes and call the network on them
  yPreds <- forM xs $ \x -> do
    inputs <- mapM (E.value "") x
    network <- N.networkCall inputs networkParams
    pure (N.networkOutputs network)
  -- Make an input node for each label
  yTrues <- forM ys $ \y ->
    mapM (E.value "") y
  -- Add nodes to compute the loss
  a <- forM (zip yTrues yPreds) $ \(yTrue, yPred) -> do
    subs <- forM (zip yTrue yPred) $ \(yt, yp) -> do
      E.sub "" yt yp
    E.sum "" subs
  b <- mapM (\x -> E.mul "" x x) a
  lossOutput <- E.sum "" b
  pure $
    Loss
      { lossOut = lossOutput
      , lossParams = getNetworkParams networkParams
      }


--------------
-- Training --
--------------

-- | The hyper-parameters for controlling the training of the network
data HyperParams =
  HyperParams
    { lr :: Double
    -- ^ The learning rate
    , epochs :: Int
    -- ^ The number of epochs
    }

-- | Increment the value of a node
nudge :: Monad m => Double -> E.Node -> E.AutoGradT m ()
nudge learningRate nid = do
  payload <- E.getNodePayload nid
  let nodeVal' = E.nodeVal payload - learningRate * E.nodeGrad payload
  E.setNodeVal nid nodeVal'

-- | Train the network using the MSE loss function
train
  :: Monad m
  => HyperParams -- ^ The training parameters (hyper-paramters)
  -> Loss -- ^ The loss to use for training
  -> E.AutoGradT m [Double]
train params Loss{..} = do
  replicateM (epochs params) $ do
    E.forward
    E.backprop lossOut
    mapM_ (nudge (lr params)) lossParams
    E.getNodeVal lossOut
