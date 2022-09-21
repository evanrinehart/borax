module Compile where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Syntax

import Data.IntMap as IM
import Data.Map as M

-- A local monad to compile a function (Reader + State + Exception)
type Compile a = ReaderT (Map String Int) (StateT CompileData (Except (Int,String))) a

data CompileData = CompileData
  { cdBreakable :: [Int]
  , cdLabelMap  :: Map String Int
  , cdSwitchTable :: Maybe [(Constant,Int)]
  , cdGenerator :: Int
  , cdGraph :: CodeGraph
  } deriving Show

data OpCode =
  Goto Int |
  IfGoto Expr Int Int |
  Switch Expr [(Constant,Int)] Int |
  Eval Expr Int |
  Return Expr |
  Null
    deriving Show

data Node = Node
  { nodeSourceLine  :: Int
  , nodeInstruction :: OpCode } deriving Show

type CodeGraph = IntMap Node


compileFunction :: Statement -> Either (Int,String) (Int,CodeGraph)
compileFunction stmt = answer where
  answer = fmap (\(n,CompileData _ _ _ _ g) -> (n,g)) algorithmResults
  algorithmResults = runExcept (runStateT (runReaderT (compile stmt 0) finalLMap) blankData)
  blankGraph = IM.fromList [(0, (Node 0 Null))]
  blankData  = CompileData [] M.empty Nothing 1 blankGraph
  -- Laziness Note
  -- provides the computed table for gotos as a readable resource to the algorithm
  ~(Right (_, CompileData _ finalLMap _ _ _)) = algorithmResults

-- The main compilation loop. Traverse the statements and build a flow chart.
-- If a case statement is encountered outside a switch statement, compilation fails.
compile :: Statement -> Int -> Compile Int

compile (AutoStatement _ _ sub) next = compile sub next
compile (ExtrnStatement _ _ sub) next = compile sub next
compile (RValueStatement line ex) next = addNode line (Eval ex next)
compile (ReturnStatement line (Just ex)) next = addNode line (Return ex)
compile (ReturnStatement line Nothing) next = addNode line (Null)
compile (NullStatement line) next = return next

compile (ConditionalStatement line ex body Nothing) next = do
  n1 <- compile body next
  n <- addNode line (IfGoto ex n1 next)
  return n

compile (ConditionalStatement line ex body1 (Just body2)) next = do
  n2 <- compile body2 next
  n1 <- compile body1 next
  addNode line (IfGoto ex n1 n2)

compile (WhileStatement line ex body) next = do  
  here <- generate
  pushBreakable next
  n1 <- compile body here
  popBreakable
  addNodeWithId here (Node line (IfGoto ex n1 next))
  return here

compile (SwitchStatement line ex body) next = do
  old <- swapSwitchTable (Just [])
  pushBreakable next
  _ <- compile body next
  popBreakable
  mnew <- swapSwitchTable old
  let Just new = mnew
  addNode line (Switch ex new next)
  
compile (GotoStatement line ex) next = do
  let NameExpr name = ex
  finalLabelMap <- ask
  let target = finalLabelMap M.! name
  addNode line (Goto target)

compile (BreakStatement line) next = do
  n <- currentBreakTarget
  addNode line (Goto n)

compile (LabelStatement _ name sub) next = do
  n <- compile sub next
  addLabel name n
  return n

compile (CaseStatement line k sub) next = do
  n <- compile sub next
  addCase line k n
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

getSwitchTable :: Int -> Compile [(Constant,Int)]
getSwitchTable lineNo = do
  mtable <- gets cdSwitchTable
  case mtable of
    Nothing    -> throwError (lineNo, "No switch statement found here")
    Just table -> return table

addLabel :: String -> Int -> Compile ()
addLabel name n = modify (\s -> s { cdLabelMap = M.insert name n (cdLabelMap s) })

addNodeWithId :: Int -> Node -> Compile ()
addNodeWithId n node = modify (\s -> s { cdGraph = IM.insert n node (cdGraph s) })

addNode :: Int -> OpCode -> Compile Int
addNode line opcode = do
  n <- generate
  addNodeWithId n (Node line opcode)
  return n

addCase :: Int -> Constant -> Int -> Compile ()
addCase lineNo k n = do
  table <- getSwitchTable lineNo
  modify (\s -> s { cdSwitchTable = Just ((k,n):table) })

generate :: Compile Int
generate = do
  g <- gets cdGenerator
  modify (\s -> s { cdGenerator = g + 1 })
  return g
