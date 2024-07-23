module Main where

import Prelude hiding (tanh)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (get)
import Control.Monad.Random (getStdGen)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Engine as E (evalAutoGradT)
import qualified Engine.Network as N
import qualified Engine.Network.Train as N
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
  gen <- getStdGen
  flip E.evalAutoGradT gen $ do
    network <- N.networkInit 3 [4, 4, 1]
    loss <- N.mseLoss xs ys network
    losses <- N.train (N.HyperParams 0.01 20) loss
    graph <- get
    liftIO $ do
      putStrLn $ "Total params: " <> show (length (N.getNetworkParams network))
      fp <- plotGraphSVG "graph" graph
      putStrLn ("Graph written to " ++ fp)
      mapM_ print losses
      toFile def "loss.svg" $ do
        layout_title .= "Training Loss"
        plot (line "Loss" [enumerate losses])
