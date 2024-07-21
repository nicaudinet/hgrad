module Main where

import Prelude hiding (tanh)

import Engine (forward, backprop, plotGraphPng)
import qualified Neuron as N

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

main :: IO ()
main = do
  graph <- N.execGraphMaker $ do
    network <- N.networkInit 3 [3, 1]
    N.mseLoss xs ys network
  -- print (N.predict mlp graph (head xs), head ys)
  fp <- plotGraphPng "graph" (backprop (forward graph))
  putStrLn ("Graph written to " ++ fp)
