module Main where

import Prelude hiding (tanh)

import Engine (value, add, mul, tanh, plotGraphPng)

-- main :: IO ()
-- main = do
--   let -- inputs
--       x1 = value "x1" 2.0
--       x2 = value "x2" 0.0
--       -- weights
--       w1 = value "w1" (-3.0)
--       w2 = value "w2" 1.0
--       -- bias
--       b = value "b" 6.8813735870195432
--       -- activation
--       x1w1 = mul "x1w1" x1 w1
--       x2w2 = mul "x2w2" x2 w2
--       xw = add "xw" x1w1 x2w2
--       n = add "n" xw b
--       o = tanh "o" n
--   fp <- plotGraphPng "graph" o
--   putStrLn ("Graph written to " ++ fp)


main :: IO ()
main = do
  let -- inputs
      a = value "a" 1.0
      b = add "b" a a
  fp <- plotGraphPng "graph" b
  putStrLn ("Graph written to " ++ fp)
