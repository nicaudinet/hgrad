{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad.Trans.State (runState)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import qualified Engine as E
import qualified Graph as G
import qualified Neuron as N

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
  [ QC.testProperty "getNode after addNode" getAdd
  , QC.testProperty "setNode after addNode" setAdd ]
  where
    getAdd node = 
      let (nid, graph) = G.addNode node (G.empty :: G.Graph Int)
       in G.getNode nid graph == node
    setAdd node1 node2 =
      let (nid, graph) = G.addNode node1 (G.empty :: G.Graph Int)
       in G.getNode nid (G.setNode nid node2 graph) == node2

propsEdge :: TestTree
propsEdge = testGroup "Edge Properties"
  [ QC.testProperty "cannot add edge from A to A" addToItself
  ]
  where
    addToItself node =
      let (nid, graph) = G.addNode node (G.empty :: G.Graph Int)
       in G.addEdge nid nid graph == graph

-------------------------
-- Backprop unit tests --
-------------------------

testsBackprop :: TestTree
testsBackprop = testGroup "Backprop unit Tests"
  [ testCase "b = a + a" test1
  , testCase "f = (a + b) * (a * b)" test2
  , testCase "sum: e = a + b + c + d" test3
  , testCase "prod: e = a * b * c * d" test4
  ]
  where
    test1 = do
      let (nids, graph) = flip runState G.empty $ do
            a <- E.value "a" 3.0
            b <- E.add "b" a a
            pure (a, b)
      let graph' = E.backprop (E.forward graph)
      let (a, b) = nids
      assertEqual "value of b" 6.0 (E.getVal b graph')
      assertEqual "gradient a" 2.0 (E.getGrad a graph')

    test2 = do
      let (nids, graph) = flip runState G.empty $ do
            a <- E.value "a" (-2.0)
            b <- E.value "b" 3.0
            d <- E.mul "d" a b
            e <- E.add "e" a b
            f <- E.mul "f" d e
            pure (a, b, f)
      let graph' = E.backprop (E.forward graph)
      let (a, b, f) = nids
      assertEqual "value of f" (-6.0) (E.getVal f graph')
      assertEqual "gradient a" (-3.0) (E.getGrad a graph')
      assertEqual "gradient b" (-8.0) (E.getGrad b graph')

    test3 = do
      let (nids, graph) = flip runState G.empty $ do
            a <- E.value "a" 1.0
            b <- E.value "b" 2.0
            c <- E.value "c" 3.0
            d <- E.value "d" 4.0
            e <- E.sumNodes "e" [a, b, c, d]
            pure (a, b, c, d, e)
      let graph' = E.backprop (E.forward graph)
      let (a, b, c, d, e) = nids
      assertEqual "value of e" 10.0 (E.getVal e graph')
      assertEqual "gradient a"  1.0 (E.getGrad a graph')
      assertEqual "gradient b"  1.0 (E.getGrad b graph')
      assertEqual "gradient c"  1.0 (E.getGrad c graph')
      assertEqual "gradient d"  1.0 (E.getGrad d graph')

    test4 = do
      let (nids, graph) = flip runState G.empty $ do
            a <- E.value "a" 1.0
            b <- E.value "b" 2.0
            c <- E.value "c" 3.0
            d <- E.value "d" 4.0
            e <- E.prodNodes "e" [a, b, c, d]
            pure (a, b, c, d, e)
      let graph' = E.backprop (E.forward graph)
      let (a, b, c, d, e) = nids
      assertEqual "value of e" 24.0 (E.getVal e graph')
      assertEqual "gradient a" 24.0 (E.getGrad a graph')
      assertEqual "gradient b" 12.0 (E.getGrad b graph')
      assertEqual "gradient c"  8.0 (E.getGrad c graph')
      assertEqual "gradient d"  6.0 (E.getGrad d graph')

-----------------------
-- Neuron unit tests --
-----------------------

testsNeuron :: TestTree
testsNeuron = testGroup "Neuron unit Tests"
  [ testCase "simple neuron with two inputs" simpleNeuron ]
  where
    simpleNeuron = do
      ((i1, i2, neuron), graph0) <- N.runGraphMaker $ do
        params <- N.neuronInit 2
        i1 <- N.value "i1" 1.0
        i2 <- N.value "i2" 2.0
        neuron <- N.neuronCall [i1, i2] params
        pure (i1, i2, neuron)
      let params = N.getParamsNeuron (N.neuronParams neuron)
      let graph1 = E.setVals graph0 (zip params (repeat 1))
      let graph2 = E.forward graph1
      assertEqual "value of i1" 1.0 (E.getVal i1 graph2)
      assertEqual "value of i2" 2.0 (E.getVal i2 graph2)
      let neuronVal = E.getVal (N.neuronOutput neuron) graph2
      assertEqual "value of neuron" (tanh 4.0) neuronVal
      let graph3 = E.backprop graph2
      assertEqual "gradient of i1" (1.0 - neuronVal ** 2) (E.getGrad i1 graph3)
      assertEqual "gradient of i2" (1.0 - neuronVal ** 2) (E.getGrad i2 graph3)
