module Main where

import Prelude hiding (tanh)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import Engine (plotGraphSVG)
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

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

main :: IO ()
main = do
  let ((network, losses), graph) = N.runGraphMakerPure 0 $ do
        network <- N.networkInit 3 [4, 4, 1]
        losses <- N.train 0.05 20 xs ys network
        pure (network, losses)
  print (length (N.getParamsNetwork network))
  fp <- plotGraphSVG "graph" graph
  putStrLn ("Graph written to " ++ fp)
  mapM_ print losses
  toFile def "loss.svg" $ do
    layout_title .= "Training Loss"
    plot (line "Loss" [enumerate losses])
