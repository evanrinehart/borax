module Compile where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Syntax

import Data.IntMap as IM
import Data.Map as M

type Compile a = ReaderT (Map String Int) (State CompileData) a

data CompileData = CompileData
  { cdBreakable :: [Int]
  , cdLabelMap  :: Map String Int
  , cdSwitchTable :: Maybe [(Constant,Int)]
  , cdGenerator :: Int
  , cdGraph :: IntMap (Int,Node)
  , cdProblem :: Maybe String
  } deriving Show

data Node =
  Goto Int |
  IfGoto Expr Int Int |
  Switch Expr [(Constant,Int)] Int |
  Eval Expr Int |
  Return Expr |
  Null
    deriving Show

-- return a control node graph and the root node id
compileStatement :: Statement -> Either String (Int, IntMap (Int,Node))
compileStatement stmt = result where
  g0 = IM.fromList [(0, (0, Null))]
  cdata = CompileData [] M.empty Nothing 1 g0 Nothing
  (n, cdata') = runState (runReaderT (compile stmt 0) finalLMap) cdata
  CompileData _ finalLMap _ _ g mproblem = cdata'
  result = case mproblem of
    Nothing  -> Right (n,g)
    Just msg -> Left msg
    

-- compile a statement given the node ID of the following node
-- may add graph nodes, may add labels, may update internal switch map
-- may push or pop node ID for breaking out of current while or switch
compile :: Statement -> Int -> Compile Int

compile (AutoStatement _ _ sub) next = compile sub next
compile (ExtrnStatement _ _ sub) next = compile sub next
compile (RValueStatement line ex) next = addNode (line, Eval ex next)
compile (ReturnStatement line (Just ex)) next = addNode (line, Return ex)
compile (ReturnStatement line Nothing) next = addNode (line, Null)
compile (NullStatement line) next = return next

compile (ConditionalStatement line ex body Nothing) next = do
  n1 <- compile body next
  n <- addNode (line, IfGoto ex n1 next)
  return n

compile (ConditionalStatement line ex body1 (Just body2)) next = do
  n2 <- compile body2 next
  n1 <- compile body1 next
  addNode (line, IfGoto ex n1 n2)

compile (WhileStatement line ex body) next = do  
  here <- generate
  pushBreakable next
  n1 <- compile body here
  popBreakable
  addNodeWithId here (line, IfGoto ex n1 next)
  return here

compile (SwitchStatement line ex body) next = do
  old <- swapSwitchTable (Just [])
  pushBreakable next
  _ <- compile body next
  popBreakable
  mnew <- swapSwitchTable old
  let Just new = mnew
  addNode (line, Switch ex new next)
  
compile (GotoStatement line ex) next = do
  let NameExpr name = ex
  finalLabelMap <- ask
  let target = finalLabelMap M.! name
  addNode (line, Goto target)

compile (BreakStatement line) next = do
  n <- currentBreakTarget
  addNode (line, Goto n)

compile (LabelStatement _ name sub) next = do
  n <- compile sub next
  addLabel name n
  return n

compile (CaseStatement _ k sub) next = do
  n <- compile sub next
  addCase k n
  return n

compile (CompoundStatement _ [])     next = return next
compile (CompoundStatement _ (s:ss)) next = do
  n2 <- compile (CompoundStatement 0 ss) next
  n1 <- compile s n2
  return n1


-- CompileData helpers
pushBreakable :: Int -> Compile ()
pushBreakable n = modify (\s -> s { cdBreakable = n : cdBreakable s })

popBreakable :: Compile ()
popBreakable = modify (\s -> s { cdBreakable = tail (cdBreakable s) })

currentBreakTarget :: Compile Int
currentBreakTarget = gets (head . cdBreakable)

swapSwitchTable :: Maybe [(Constant,Int)] -> Compile (Maybe [(Constant,Int)])
swapSwitchTable sm' = do
  sm <- gets cdSwitchTable
  modify (\s -> s { cdSwitchTable = sm' })
  return sm

getSwitchTable :: Compile [(Constant,Int)]
getSwitchTable = do
  mtable <- gets cdSwitchTable
  case mtable of
    Nothing    -> assertProblem "no switch statement found" >> return []
    Just table -> return table

addLabel :: String -> Int -> Compile ()
addLabel name n = modify (\s -> s { cdLabelMap = M.insert name n (cdLabelMap s) })

addNodeWithId :: Int -> (Int,Node) -> Compile ()
addNodeWithId n p = modify (\s -> s { cdGraph = IM.insert n p (cdGraph s) })

addNode :: (Int, Node) -> Compile Int
addNode p = do
  n <- generate
  addNodeWithId n p
  return n

addCase :: Constant -> Int -> Compile ()
addCase k n = do
  table <- getSwitchTable
  modify (\s -> s { cdSwitchTable = Just ((k,n):table) })

generate :: Compile Int
generate = do
  g <- gets cdGenerator
  modify (\s -> s { cdGenerator = g + 1 })
  return g

assertProblem :: String -> Compile ()
assertProblem msg = modify (\s -> s { cdProblem = Just msg })
