{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import qualified Engine as E
import qualified Graph as G

instance QC.Arbitrary G.NodeId where
  arbitrary = fromInteger <$> QC.arbitrary

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
  [ QC.testProperty "getNode on empty graph" getEmpty
  , QC.testProperty "getNode after addNode" getAdd
  , QC.testProperty "setNode on empty graph" setEmpty
  , QC.testProperty "setNode after addNode" setAdd ]
  where
    getEmpty nid = G.getNode nid (G.empty :: G.Graph Int) == Nothing
    getAdd node = 
      let (nid, graph) = G.addNode node (G.empty :: G.Graph Int)
       in G.getNode nid graph == Just node
    setEmpty nid node = G.setNode nid node G.empty == (G.empty :: G.Graph Int)
    setAdd node1 node2 =
      let (nid, graph) = G.addNode node1 (G.empty :: G.Graph Int)
       in G.getNode nid (G.setNode nid node2 graph) == Just node2

propsEdge :: TestTree
propsEdge = testGroup "Edge Properties"
  [ QC.testProperty "addEdge on empty graph" addEmpty
  , QC.testProperty "addEdge with only one node in graph" addOne
  , QC.testProperty "cannot add edge from A to A" addToItself
  ]
  where
    addEmpty nid1 nid2 =
      G.addEdge nid1 nid2 G.empty == (G.empty :: G.Graph Int)
    addOne node1 nid2 =
      let (nid1, graph) = G.addNode node1 (G.empty :: G.Graph Int)
       in G.addEdge nid1 nid2 graph == graph
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
    getVal :: G.NodeId -> E.BPGraph -> Maybe Double
    getVal nid graph = fmap E.nodeVal (G.getNode nid graph)

    getGrad :: G.NodeId -> E.BPGraph -> Maybe Double
    getGrad nid graph = fmap E.nodeGrad (G.getNode nid graph)

    test1 = do
      let graph = E.backprop . E.forward . E.makeGraph $ do
            a <- E.value "a" 3.0
            E.add "b" a a
      assertEqual "value of b" (Just 6.0) (getVal 1 graph)
      assertEqual "gradient of a" (Just 2.0) (getGrad 0 graph)

    test2 = do
      let graph = E.backprop . E.forward . E.makeGraph $ do
            a <- E.value "a" (-2.0)
            b <- E.value "b" 3.0
            d <- E.mul "d" a b
            e <- E.add "e" a b
            E.mul "f" d e
      assertEqual "value of f" (Just (-6.0)) (getVal 4 graph)
      assertEqual "gradient of a" (Just (-3.0)) (getGrad 0 graph)
      assertEqual "gradient of b" (Just (-8.0)) (getGrad 1 graph)
