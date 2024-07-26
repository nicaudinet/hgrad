{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Criterion.Main

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (StdGen, newStdGen)
import Control.Monad.Trans.State (get, put)

import qualified Engine as E
import qualified Engine.Network as N
import qualified Engine.Network.Train as N
import Engine.Visualize (plotGraphSVG)

instance NFData E.Node
instance NFData E.NodeOp
instance NFData E.Payload
instance NFData E.CGraph

xs :: [[Double]]
xs =
  [ [ 2.0,  3.0, -1.0 ]
  , [ 3.0, -1.0,  0.5 ]
  , [ 0.5,  1.0,  1.0 ]
  , [ 1.0,  1.0, -1.0 ]
  ]

ys :: [[Double]]
ys =
  [ [1.0]
  , [-1.0]
  , [-1.0]
  , [1.0]
  ]

trainMLP :: Int -> [Int] -> StdGen -> [Double]
trainMLP inputs layers gen = flip E.evalAutoGrad gen $ do
  network <- N.networkInit inputs layers
  loss <- N.mseLoss xs ys network
  N.train (N.HyperParams 0.01 20) loss

benchMLP :: StdGen -> Benchmark
benchMLP gen = bgroup "MLP"
  [ bench "3 [2,2,1]" $ nf (trainMLP 3 [2,2,1]) gen
  , bench "3 [4,4,1]" $ nf (trainMLP 3 [4,4,1]) gen
  , bench "3 [8,8,1]" $ nf (trainMLP 3 [8,8,1]) gen
  ]

initGraph :: IO (E.Node, E.CGraph)
initGraph = do
  gen <- newStdGen
  flip E.evalAutoGradT gen $ do
    inputs <- mapM E.value [1..10]
    params <- N.networkInit (length inputs) [10, 10, 1]
    out <- head . N.networkOutputs <$> N.networkCall inputs params
    graph <- get
    pure (out, graph)

benchTransformations :: StdGen -> Benchmark
benchTransformations gen =
  env initGraph $ \ ~(out, graph) -> 
    bgroup "Transformations"
      [ bench "forward" $ nf benchForward graph
      , bench "backprop" $ nf (benchBackprop out) graph
      ]
  where
    benchForward graph = E.execAutoGrad (put graph >> E.forward) gen
    benchBackprop out graph = E.execAutoGrad (put graph >> E.backprop out) gen

main :: IO ()
main = do
  gen <- newStdGen
  defaultMain [ benchMLP gen, benchTransformations gen ]
