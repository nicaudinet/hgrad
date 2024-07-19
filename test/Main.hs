{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad.Trans.State (runState)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import qualified Engine as E
import qualified Graph as G

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ propsGraph, testsBackprop ]

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
  ]
  where
    getVal :: G.NodeId -> E.BPGraph -> Double
    getVal nid graph = E.nodeVal (G.getNode nid graph)

    getGrad :: G.NodeId -> E.BPGraph -> Double
    getGrad nid graph = E.nodeGrad (G.getNode nid graph)

    test1 = do
      let (nids, graph) = flip runState G.empty $ do
            a <- E.value "a" 3.0
            b <- E.add "b" a a
            pure (a, b)
      let graph' = E.backprop (E.forward graph)
      let (a, b) = nids
      assertEqual "value of b" 6.0 (getVal b graph')
      assertEqual "gradient a" 2.0 (getGrad a graph')

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
      assertEqual "value of f" (-6.0) (getVal f graph')
      assertEqual "gradient a" (-3.0) (getGrad a graph')
      assertEqual "gradient b" (-8.0) (getGrad b graph')
