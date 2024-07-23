module Main where

import Prelude hiding (tanh)

import Control.Monad.Random (mkStdGen)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Engine as E
import qualified Engine.Network as N
import Engine.Visualize (plotGraphSVG)

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

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

main :: IO ()
main = do
  let ((network, losses), graph) = flip E.runAutoGrad (mkStdGen 0) $ do
        n <- N.networkInit 3 [4, 4, 1]
        ls <- N.train 0.05 20 xs ys n
        pure (n, ls)
  print (length (N.getParamsNetwork network))
  fp <- plotGraphSVG "graph" graph
  putStrLn ("Graph written to " ++ fp)
  mapM_ print losses
  toFile def "loss.svg" $ do
    layout_title .= "Training Loss"
    plot (line "Loss" [enumerate losses])
