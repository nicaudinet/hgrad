module Main (main) where

import Control.Monad.Random (getStdGen)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import qualified Engine as E
import qualified Engine.Network as N
import qualified Engine.Network.Train as N
import qualified Graph as G

type GraphState a = StateT (G.Graph ()) IO a

runGraphState :: GraphState () -> IO ()
runGraphState = flip evalStateT G.empty

insertNode :: GraphState G.NodeId
insertNode = do
  (nid, graph) <- G.insertNode () <$> get
  put graph
  pure nid

insertEdge :: G.NodeId -> G.NodeId -> GraphState ()
insertEdge n1 n2 = modify (G.insertEdge n1 n2)

type AutoGradTest = E.AutoGradT IO ()

runAutoGradTest :: AutoGradTest -> IO ()
runAutoGradTest m = getStdGen >>= E.evalAutoGradT m

assertVal :: G.NodeId -> Double -> AutoGradTest
assertVal nid a = do
  bLabel <- fromMaybe "_" <$> E.getNodeLabel nid
  bVal <- E.getNodeVal nid
  liftIO $ assertEqual ("value of " <> bLabel) a bVal

assertValApprox :: G.NodeId -> Double -> AutoGradTest
assertValApprox nid a = do
  bLabel <- fromMaybe "_" <$> E.getNodeLabel nid
  bVal <- E.getNodeVal nid
  let cond = abs (bVal - a) < 1e-6
  let failMsg = intercalate " "
        [ "value of", bLabel
        , "should be approximately", show a
        , "but is", show bVal
        ]
  liftIO $ assertBool failMsg cond

assertGrad :: G.NodeId -> Double -> AutoGradTest
assertGrad nid a = do
  bLabel <- fromMaybe "_" <$> E.getNodeLabel nid
  bGrad <- E.getNodeGrad nid
  liftIO $ assertEqual ("grad of " <> bLabel) a bGrad

assertGradApprox :: G.NodeId -> Double -> AutoGradTest
assertGradApprox nid a = do
  bLabel <- fromMaybe "_" <$> E.getNodeLabel nid
  bGrad <- E.getNodeGrad nid
  let cond = abs (bGrad - a) < 1e-6
  let failMsg = intercalate " "
        [ "grad of", bLabel
        , "should be approximately", show a
        , "but is", show bGrad
        ]
  liftIO $ assertBool failMsg cond

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ propsGraph
  , testsGraph
  , testsBackprop
  , testsNeuron
  ]

--------------------------
-- Graph property tests --
--------------------------

propsGraph :: TestTree
propsGraph = testGroup "Graph Property Tests" [ propsNode, propsEdge ]

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

testsGraph :: TestTree
testsGraph = testGroup "Graph Unit Tests"
  [ testCase "topoSort on simple neuron" testTopo ]
  where
    -- sigma * (x1 * w1 + x2 * w2 + b)
    testTopo = runGraphState $ do
      x1 <- insertNode -- 0
      w1 <- insertNode -- 1
      x1w1 <- insertNode -- 2
      x2 <- insertNode -- 3
      w2 <- insertNode -- 4
      x2w2 <- insertNode -- 5
      x1w1x2w2 <- insertNode -- 6
      b <- insertNode -- 7
      n <- insertNode -- 8
      act <- insertNode -- 9
      insertEdge x1 x1w1
      insertEdge w1 x1w1
      insertEdge x2 x2w2
      insertEdge w2 x2w2
      insertEdge x1w1 x1w1x2w2
      insertEdge x2w2 x1w1x2w2
      insertEdge x1w1x2w2 n
      insertEdge b n
      insertEdge n act
      graph <- get
      let ts = G.topoSort graph act
      let expected = [act, n, x1w1x2w2, x1w1, x1, w1, x2w2, x2, w2, b]
      liftIO $ assertEqual "topoSort" expected ts

-------------------------
-- Backprop unit tests --
-------------------------

testsBackprop :: TestTree
testsBackprop = testGroup "Backprop Unit Tests"
  [ testCase "b = a + a" testAddNodeTwice
  , testCase "f = (a + b) * (a * b)" test2
  , testCase "sum: e = a + b + c + d" testSum
  , testCase "prod: e = a * b * c * d" testProd
  , testCase "micrograd: test_sanity_check" testSanityCheck
  , testCase "micrograd: test_more_ops" testMoreOps
  ]
  where
    testAddNodeTwice = runAutoGradTest $ do
      a <- E.withLabel "a" $ E.value 3.0
      b <- E.withLabel "b" $ E.add a a
      E.forward
      E.backprop b
      assertVal b 6.0
      assertGrad a 2.0

    test2 = runAutoGradTest $ do
      a <- E.withLabel "a" $ E.value (-2.0)
      b <- E.withLabel "b" $ E.value 3.0
      d <- E.withLabel "d" $ E.mul a b
      e <- E.withLabel "e" $ E.add a b
      f <- E.withLabel "f" $ E.mul d e
      E.forward
      E.backprop f
      assertVal f (-6.0)
      assertGrad a (-3.0)
      assertGrad b (-8.0)

    testSum = runAutoGradTest $ do
      a <- E.withLabel "a" $ E.value 1.0
      b <- E.withLabel "b" $ E.value 2.0
      c <- E.withLabel "c" $ E.value 3.0
      d <- E.withLabel "d" $ E.value 4.0
      e <- E.withLabel "e" $ E.sum [a, b, c, d]
      E.forward
      E.backprop e
      assertVal e 10.0
      assertGrad a 1.0
      assertGrad b 1.0
      assertGrad c 1.0
      assertGrad d 1.0

    testProd = runAutoGradTest $ do
      a <- E.withLabel "a" $ E.value 1.0
      b <- E.withLabel "b" $ E.value 2.0
      c <- E.withLabel "c" $ E.value 3.0
      d <- E.withLabel "d" $ E.value 4.0
      e <- E.withLabel "e" $ E.prod [a, b, c, d]
      E.forward
      E.backprop e
      assertVal e 24.0
      assertGrad a 24.0
      assertGrad b 12.0
      assertGrad c  8.0
      assertGrad d  6.0

    -- Original test from https://github.com/karpathy/micrograd/blob/master/test/test_engine.py
    -- The magic numbers here correspond to what PyTorch produced. They can be
    -- reproduced by running test_py/test_sanity_check.py
    testSanityCheck = runAutoGradTest $ do
      x  <- E.withLabel "x" $ E.value (-4.0)
      z  <- E.withLabel "z" $ do
        z1 <- E.scale 2 x
        z2 <- E.shift 2 x
        E.add z1 z2
      q  <- E.withLabel "q" $ do
        q1 <- E.relu z
        q2 <- E.mul z x
        E.add q1 q2
      h  <- E.withLabel "h" $ do
        h1 <- E.mul z z
        E.relu h1
      y  <- E.withLabel "y" $ do
        y1 <- E.mul q x
        E.sum [h, q, y1]
      E.forward
      E.backprop y
      assertVal z (-10.0)
      assertVal q 40.0
      assertVal h 100.0
      assertVal y (-20.0)
      assertGrad h 1.0
      assertGrad q (-3.0)
      assertGrad z (-8.0)
      assertGrad x 46.0

    -- Original test from https://github.com/karpathy/micrograd/blob/master/test/test_engine.py
    -- The magic numbers here correspond to what PyTorch produced. They can be
    -- reproduced by running test_py/test_more_ops.py
    testMoreOps = runAutoGradTest $ do
      a <- E.withLabel "a" $ E.value (-4.0)
      b <- E.withLabel "b" $ E.value 2.0
      c <- E.add a b
      d <- do
        d1 <- E.mul a b
        d2 <- E.pow 3 b
        E.add d1 d2
      c <- E.add c c
      c <- E.shift 1 c
      c <- E.add c c
      c <- E.shift 1 c
      c <- E.sub c a
      d <- do
        d1 <- E.scale 2 d
        d2 <- E.add b a
        d2 <- E.relu d2
        E.sum [d, d1, d2]
      d <- do
        d1 <- E.scale 3 d
        d2 <- E.sub b a
        d2 <- E.relu d2
        E.sum [d, d1, d2]
      e <- E.sub c d
      f <- E.pow 2 e
      g <- E.scale (1 / 2.0) f
      g <- do
        g1 <- E.recip f
        g1 <- E.scale 10 g1
        E.add g g1
      E.forward
      E.backprop g
      assertValApprox g 24.70408163265306
      assertGradApprox a 138.83381924198252
      assertGradApprox b 645.5772594752186

-----------------------
-- Neuron unit tests --
-----------------------

testsNeuron :: TestTree
testsNeuron = testGroup "Neuron unit Tests"
  [ testCase "simple neuron with two inputs" simpleNeuron ]
  where
    simpleNeuron = runAutoGradTest $ do
      i1 <- E.withLabel "i1" $ E.value 1.0
      i2 <- E.withLabel "i2" $ E.value 2.0
      params <- N.neuronInit 2
      neuron <- N.neuronOutput <$> N.neuronCall [i1, i2] params
      E.setNodeVals (zip (N.getNeuronParams params) (repeat 1))
      E.forward
      E.backprop neuron
      let nval = tanh 4.0
      assertVal neuron nval
      assertGrad i1 (1.0 - nval ** 2)
      assertGrad i2 (1.0 - nval ** 2)
