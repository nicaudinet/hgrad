module Main where

import Prelude hiding (tanh)

import Engine (forward, backprop, plotGraphPng)
import qualified Neuron as N

main :: IO ()
main = do
  graph <- N.execGraphMaker $ do
    i1 <- N.value "i1" 1.0
    i2 <- N.value "i2" 2.0
    N.mlp [3, 3, 1] [i1, i2]
  fp <- plotGraphPng "graph" (backprop (forward graph))
  putStrLn ("Graph written to " ++ fp)
