module Main where

import Prelude hiding (tanh)

import Engine (makeGraph, value, add, mul, tanh, forward, backprop, plotGraphPng)

main :: IO ()
main = do
  fp <- plotGraphPng "graph" (backprop . forward $ graph)
  putStrLn ("Graph written to " ++ fp)
  where
    graph = makeGraph $ do
      -- inputs
      x1 <- value "x1" 2.0
      x2 <- value "x2" 0.0
      -- weights
      w1 <- value "w1" (-3.0)
      w2 <- value "w2" 1.0
      -- bias
      b <- value "b" 6.8813735870195432
      -- activation
      x1w1 <- mul "x1w1" x1 w1
      x2w2 <- mul "x2w2" x2 w2
      xw <- add "xw" x1w1 x2w2
      n <- add "n" xw b
      -- output
      tanh "o" n
