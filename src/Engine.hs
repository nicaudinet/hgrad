module Engine
  -- constructors
  ( value
  , add
  , mul
  , tanh
  -- evaluation
  , eval
  -- plotting
  , plotGraphPng
  )
where

import Prelude hiding (tanh)
import qualified Prelude as P (tanh)
import Graph.Dot
import Control.Monad.Trans.State


-----------
-- Types --
-----------

-- Main expression type
-- Parametrized over the values of the intermediate nodes
data ExprAlg a
  = Value Label Double Double
  | Add a [ExprAlg a]
  | Mul a [ExprAlg a]
  | Tanh a (ExprAlg a)

-- Expressions before evaluation
type Label = String
type Expr = ExprAlg Label

-- Expressions after evaluation
data EvalNode = EvalNode Label Double
type EvalExpr = ExprAlg EvalNode

-- Expressions after backpropagation
data GradNode = GradNode Label Double Double
type GradExpr = ExprAlg GradNode


------------------
-- Constructors --
------------------

value :: Label -> Double -> Expr
value label num = Value label num 0.0

add :: Label -> Expr -> Expr -> Expr
add label a b = Add label [a, b]

mul :: Label -> Expr -> Expr -> Expr
mul label a b = Mul label [a, b]

tanh :: Label -> Expr -> Expr
tanh = Tanh


----------------
-- Evaluation --
----------------

eval :: Expr -> Double
eval (Value _ val _) = val
eval (Add _ children) = sum (fmap eval children)
eval (Mul _ children) = product (fmap eval children)
eval (Tanh _ child) = P.tanh (eval child)

getValue :: EvalExpr -> Double
getValue (Value _ val _) = val
getValue (Add (EvalNode _ val) _) = val
getValue (Mul (EvalNode _ val) _) = val
getValue (Tanh (EvalNode _ val) _) = val

evalExpr :: Expr -> EvalExpr
evalExpr (Value label val grad) = Value label val grad
evalExpr (Add label children) =
  let children' = fmap evalExpr children
      evalNode = EvalNode label (sum (fmap getValue children'))
  in Add evalNode children'
evalExpr (Mul label children) =
  let children' = fmap evalExpr children
      evalNode = EvalNode label (product (fmap getValue children'))
  in Mul evalNode children'
evalExpr (Tanh label child) =
  let child' = evalExpr child
      evalNode = EvalNode label (P.tanh (getValue child'))
  in Tanh evalNode child'

oneVsRest :: [a] -> [(a, [a])]
oneVsRest [] = []
oneVsRest as = reverse . fst $ foldr go ([], as) as
  where
    go :: a -> ([(a, [a])], [a]) -> ([(a, [a])], [a])
    go _ (xs, []) = (xs, [])
    go _ (xs, y:ys) = ((y,ys) : xs, ys <> [y])

backpropExpr' :: Double -> EvalExpr -> GradExpr
backpropExpr' grad (Value label val _) = Value label val grad
backpropExpr' grad (Add evalNode children) =
  let EvalNode label val = evalNode
      gradNode = GradNode label val grad
      children' = fmap (backpropExpr' grad) children
   in Add gradNode children'
backpropExpr' grad (Mul evalNode children) =
  let EvalNode label val = evalNode
      gradNode = GradNode label val grad
      computeGrad :: (EvalExpr, [EvalExpr]) -> Double
      computeGrad (_, xs) = grad * product (fmap getValue xs)
      grads = fmap computeGrad (oneVsRest children)
      children' = zipWith backpropExpr' grads children
   in Mul gradNode children'
backpropExpr' grad (Tanh evalNode child) =
  let EvalNode label val = evalNode
      gradNode = GradNode label val grad
      child' = backpropExpr' (grad * (1 - val * val)) child
   in Tanh gradNode child'

backpropExpr :: EvalExpr -> GradExpr
backpropExpr = backpropExpr' 1.0


--------------
-- Graphing --
--------------

newtype NodeId = NodeId Int

data Node
  = OpNode Label
  | DataNode Label Double Double

data Tree = Tree NodeId Node [Tree]

getNodeId :: State Int NodeId
getNodeId = do
  nid <- get
  put (nid + 1)
  pure (NodeId nid)

toTree' :: GradExpr -> State Int Tree
--
toTree' (Value label val grad) = do
  nid <- getNodeId
  pure (Tree nid (DataNode label val grad) [])
--
toTree' (Add gradNode children) = do
  let GradNode label val grad = gradNode
  children' <- traverse toTree' children
  nid <- getNodeId
  let inner = Tree nid (OpNode "+") children'
  mid <- getNodeId
  pure (Tree mid (DataNode label val grad) [inner])
--
toTree' (Mul gradNode children) = do
  let GradNode label val grad = gradNode
  children' <- traverse toTree' children
  nid <- getNodeId
  let inner = Tree nid (OpNode "*") children'
  mid <- getNodeId
  pure (Tree mid (DataNode label val grad) [inner])
--
toTree' (Tanh gradNode child) = do
  let GradNode label val grad = gradNode
  child' <- toTree' child
  nid <- getNodeId
  let inner = Tree nid (OpNode "tanh") [child']
  mid <- getNodeId
  pure (Tree mid (DataNode label val grad) [inner])

toTree :: GradExpr -> Tree
toTree expr = evalState (toTree' expr) 0

nodeToDot :: NodeId -> Node -> DotLine
nodeToDot (NodeId nid) (OpNode l) = opNode nid l
nodeToDot (NodeId nid) (DataNode l v g) = dataNode nid l v g

edgeToDot :: NodeId -> NodeId -> DotLine
edgeToDot (NodeId i) (NodeId j) = edge i j

treeToDot :: Tree -> Dot
treeToDot (Tree nid n ts) =
  [nodeToDot nid n] <> fmap toEdge ts <> concatMap treeToDot ts
  where
    toEdge :: Tree -> DotLine
    toEdge (Tree mid _ _) = edgeToDot mid nid

plotGraphPng :: FilePath -> Expr -> IO FilePath
plotGraphPng fp expr = do
  let dotFile = treeToDot (toTree (backpropExpr (evalExpr expr)))
  plotPng fp dotFile
