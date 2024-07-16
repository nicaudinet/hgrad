module Main where

import Engine (value, add, mul, eval, plotGraphPng)

main :: IO ()
main = do
  let a = value "a" 2.0
      b = value "b" (-3.0)
      c = value "c" 10.0
      e = mul "e" a b
      d = add "d" e c
      f = value "f" (-2.0)
      l = mul "L" d f
  print $ eval l
  fp <- plotGraphPng "graph" l
  putStrLn ("Graph written to " ++ fp)
