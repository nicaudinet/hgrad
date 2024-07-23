{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad.Random (getStdGen)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import qualified Engine as E
import qualified Engine.Network as N
import qualified Graph as G

type AutoGradTest = E.AutoGradT IO ()

runAutoGradTest :: AutoGradTest -> IO ()
runAutoGradTest m = getStdGen >>= E.evalAutoGradT m

assertVal :: G.NodeId -> Double -> AutoGradTest
assertVal nid a = do
  bLabel <- E.getNodeLabel nid
  bVal <- E.getNodeVal nid
  liftIO $ assertEqual ("value of " <> bLabel) a bVal

assertGrad :: G.NodeId -> Double -> AutoGradTest
assertGrad nid a = do
  bLabel <- E.getNodeLabel nid
  bGrad <- E.getNodeGrad nid
  liftIO $ assertEqual ("grad of " <> bLabel) a bGrad

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ propsGraph, testsBackprop, testsNeuron ]

--------------------------
-- Graph property tests --
--------------------------

propsGraph :: TestTree
propsGraph = testGroup "Graph Properties" [ propsNode, propsEdge ]

propsNode :: TestTree
propsNode = testGroup "Node Properties"
  [ QC.testProperty "getNode after insertNode" getAdd
  , QC.testProperty "setNode after insertNode" setAdd ]
  where
    getAdd node = 
      let (nid, graph) = G.insertNode node (G.empty :: G.Graph Int)
       in G.getNode nid graph == node
    setAdd node1 node2 =
      let (nid, graph) = G.insertNode node1 (G.empty :: G.Graph Int)
       in G.getNode nid (G.setNode nid node2 graph) == node2

propsEdge :: TestTree
propsEdge = testGroup "Edge Properties"
  [ QC.testProperty "cannot insert edge from A to A" addToItself
  ]
  where
    addToItself node =
      let (nid, graph) = G.insertNode node (G.empty :: G.Graph Int)
       in G.insertEdge nid nid graph == graph

-------------------------
-- Backprop unit tests --
-------------------------

testsBackprop :: TestTree
testsBackprop = testGroup "Backprop unit Tests"
  [ testCase "b = a + a" testAddNodeTwice
  , testCase "f = (a + b) * (a * b)" test2
  , testCase "sum: e = a + b + c + d" testSum
  , testCase "prod: e = a * b * c * d" testProd
  ]
  where
    testAddNodeTwice = runAutoGradTest $ do
      a <- E.value "a" 3.0
      b <- E.add "b" a a
      E.forward
      E.backprop b
      assertVal b 6.0
      assertGrad a 2.0

    test2 = runAutoGradTest $ do
      a <- E.value "a" (-2.0)
      b <- E.value "b" 3.0
      d <- E.mul "d" a b
      e <- E.add "e" a b
      f <- E.mul "f" d e
      E.forward
      E.backprop f
      assertVal f (-6.0)
      assertGrad a (-3.0)
      assertGrad b (-8.0)

    testSum = runAutoGradTest $ do
      a <- E.value "a" 1.0
      b <- E.value "b" 2.0
      c <- E.value "c" 3.0
      d <- E.value "d" 4.0
      e <- E.sum "e" [a, b, c, d]
      E.forward
      E.backprop e
      assertVal e 10.0
      assertGrad a 1.0
      assertGrad b 1.0
      assertGrad c 1.0
      assertGrad d 1.0

    testProd = runAutoGradTest $ do
      a <- E.value "a" 1.0
      b <- E.value "b" 2.0
      c <- E.value "c" 3.0
      d <- E.value "d" 4.0
      e <- E.prod "e" [a, b, c, d]
      E.forward
      E.backprop e
      assertVal e 24.0
      assertGrad a 24.0
      assertGrad b 12.0
      assertGrad c  8.0
      assertGrad d  6.0

-----------------------
-- Neuron unit tests --
-----------------------

testsNeuron :: TestTree
testsNeuron = testGroup "Neuron unit Tests"
  [ testCase "simple neuron with two inputs" simpleNeuron ]
  where
    simpleNeuron = runAutoGradTest $ do
      i1 <- E.value "i1" 1.0
      i2 <- E.value "i2" 2.0
      params <- N.neuronInit 2
      neuron <- N.neuronOutput <$> N.neuronCall [i1, i2] params
      E.setNodeVals (zip (N.getParamsNeuron params) (repeat 1))
      E.forward
      E.backprop neuron
      let nval = tanh 4.0
      assertVal neuron nval
      assertGrad i1 (1.0 - nval ** 2)
      assertGrad i2 (1.0 - nval ** 2)
