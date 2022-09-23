module Compile where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer

import Syntax

import Data.List
import Data.IntMap as IM hiding (filter, map)
import Data.Map as M hiding (filter, map)

-- A local monad to compile a function (Reader + State + Exception)
type Compile a = ReaderT (Map String Int) (StateT CompileData (Except (Int,String))) a

data CompileData = CompileData
  { cdBreakable   :: [Int]
  , cdLabelMap    :: Map String Int
  , cdSwitchTable :: Maybe [(Constant,Int)]
  , cdGenerator   :: Int
  , cdGraph       :: CodeGraph Expr
  } deriving Show

data OpCode a =
  Goto Int |
  IfGoto a Int Int |
  Switch a [(Constant,Int)] Int |
  Eval a Int |
  Return a |
  Null
    deriving Show

data Node a = Node
  { nodeSourceLine  :: Int
  , nodeInstruction :: OpCode a }

type CodeGraph a = IntMap (Node a)

data FrameObjType = Argument | AutoVar | AutoVec deriving Show
data FrameObj = FrameObj
  { foName   :: String
  , foType   :: FrameObjType
  , foSize   :: Int
  , foOffset :: Int }
    deriving Show

instance Show a => Show (Node a) where
  show (Node l op) = "Node(l="++show l++", "++ showOpcode op ++ ")"
  

compileFunction :: Statement -> Either (Int,String) (Int,CodeGraph Expr)
compileFunction stmt =
  let
    -- Blank records to work with
    blankGraph = IM.fromList [(0, (Node 0 Null))]
    blankData  = CompileData [] M.empty Nothing 1 blankGraph

    -- Run the compilation of function body. gotoTable (from the future) is provided
    algorithmResults = runCompile blankData gotoTable (compile stmt 0)

    -- ! --
    Right (_, CompileData _ gotoTable _ _ _) = algorithmResults
  in
    fmap (fmap cdGraph) algorithmResults


runCompile :: CompileData -> Map String Int -> Compile a -> Either (Int,String) (a, CompileData)
runCompile s cheats action = runExcept (runStateT (runReaderT action cheats) s)


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

addNodeWithId :: Int -> Node Expr -> Compile ()
addNodeWithId n node = modify (\s -> s { cdGraph = IM.insert n node (cdGraph s) })

addNode :: Int -> OpCode Expr -> Compile Int
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


-- debug print

showGraph :: Show a => CodeGraph a -> String
showGraph gr = unlines (Prelude.map f (IM.toList gr)) where
  f (here, Node line op) = show here ++ ":\t" ++ g op where
    g (Goto n) = "Goto " ++ show n
    g (IfGoto ex n1 n2) = "IfGoto[" ++ show ex ++ "] " ++ show n1 ++ " " ++ show n2
    g (Eval ex next) = "Eval[" ++ show ex ++ "] " ++ show next
    g (Return ex) = "Return[" ++ show ex ++ "]"
    g Null = "Null"
    g (Switch ex table next) = "Switch[" ++ show ex ++ "] " ++ h table ++ " " ++ show next
  h = showSwitchTable

showOpcode :: Show a => OpCode a -> String
showOpcode = g where
  g (Goto n) = "Goto " ++ show n
  g (IfGoto ex n1 n2) = "IfGoto[" ++ show ex ++ "] " ++ show n1 ++ " " ++ show n2
  g (Eval ex next) = "Eval[" ++ show ex ++ "] " ++ show next
  g (Return ex) = "Return[" ++ show ex ++ "]"
  g Null = "Null"
  g (Switch ex table next) = "Switch[" ++ show ex ++ "] " ++ h table ++ " " ++ show next
  h = showSwitchTable

showSwitchTable :: [(Constant,Int)] -> String
showSwitchTable table = concat ["{",concat (intersperse "," (Prelude.map f table)),"}"] where
  f (k, n) = showConstant k ++ "=>" ++ show n



-- frame layout extractor
analyzeFrameLayout :: FunctionDef -> [FrameObj]
analyzeFrameLayout (FunctionDef _ _ params body) = autos ++ argos where
  bodyAutos = autoVariablesInBody body
  autoSize = sum (map measure bodyAutos)
  (_, autos) = mapAccumL f (-autoSize) bodyAutos
  argos = zipWith (\i name -> FrameObj name Argument 1 i) [1..] params
  f b (name, Nothing)   = (b+1,    FrameObj name AutoVar 1 b)
  f b (name, Just size) = (b+size, FrameObj name AutoVec size b)
  measure (_, Nothing)   = 1
  measure (_, Just size) = size

-- location of arguments and autos within the frame, relative to base
frameNamesMap :: [FrameObj] -> Map String Int
frameNamesMap = M.fromList . map (\o -> (foName o, foOffset o))

frameVectorLocations :: [FrameObj] -> [Int]
frameVectorLocations = map foOffset . filter isAutoVec

frameAutoSize :: [FrameObj] -> Int
frameAutoSize = sum . map foSize . filter (not . isArgument)

isArgument :: FrameObj -> Bool
isArgument (FrameObj { foType = Argument }) = True
isArgument (FrameObj { foType = _        }) = False

isAutoVec :: FrameObj -> Bool
isAutoVec (FrameObj { foType = AutoVec }) = True
isAutoVec (FrameObj { foType = _       }) = False

autoVariablesInBody :: Statement -> [(String, Maybe Int)]
autoVariablesInBody = execWriter . go where
  go :: Statement -> Writer [(String, Maybe Int)] ()
  go (AutoStatement _ vars stmt) = do
    tell vars
--    tell (map (\(name,msize) -> (name, case msize of Nothing -> 1; Just (ConstNumber n) -> fromIntegral n+1)) vars)
    go stmt
  go (ExtrnStatement _ _ stmt) = go stmt
  go (LabelStatement _ _ stmt) = go stmt
  go (CaseStatement _ _ stmt) = go stmt
  go (CompoundStatement _ stmts) = mapM_ go stmts
  go (ConditionalStatement _ _ stmt Nothing) = go stmt
  go (ConditionalStatement _ _ stmt1 (Just stmt2)) = do
    go stmt1
    go stmt2
  go (WhileStatement _ _ stmt) = go stmt
  go (SwitchStatement _ _ stmt) = go stmt
  go _ = return ()

