module Compile where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe

import Syntax
import PackedString

import Data.List
import Data.IntMap as IM hiding (filter, map)
import Data.Map as M hiding (filter, map)

-- compiled source file
-- if a = Int, it's ready to run
-- if a = LinkMe, global entities have unresolved initializers
data Borate a = Borate
  { borAtoms   :: [(String, a)]
  , borVectors :: [(String, Int, [a])]
  , borFuncs   :: [Func] }
      deriving Show

data LinkMe = LMJust Int | LMString String | LMVariable String
  deriving Show

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

data Func = Func
  { funName :: String
  , funCode :: CodeGraph Expr
  , funAutoSize :: Int
  , funExterns :: [String]
  , funLocals :: Map String Int
  , funSize :: Int -- func occupies this amount of memory in words
  , funVectors :: [Int] -- for i in vecs, *(bp-i) = bp-i
  , funStrings :: [String]
  , funStart :: Int
  } deriving Show


instance Show a => Show (Node a) where
  show (Node l op) = "Node(l="++show l++", "++ showOpcode op ++ ")"
  

compileFunction :: Statement -> Either (Int,String) (Int,CodeGraph Expr)
compileFunction stmt =
  let
    -- Blank records to work with
    blankGraph = IM.fromList [(0, (Node 0 Null))]
    blankData  = CompileData [] M.empty Nothing 1 blankGraph

    -- Run the compilation of function body. gotoTable (from the future) is provided
    algorithmResults = runCompile blankData gotoTable (compileStatement stmt 0)

    -- ! --
    Right (_, CompileData _ gotoTable _ _ _) = algorithmResults
  in
    fmap (fmap cdGraph) algorithmResults


runCompile :: CompileData -> Map String Int -> Compile a -> Either (Int,String) (a, CompileData)
runCompile s cheats action = runExcept (runStateT (runReaderT action cheats) s)


-- The main compilation loop. Traverse the statements and build a flow chart.
-- If a case statement is encountered outside a switch statement, compilation fails.
compileStatement :: Statement -> Int -> Compile Int
compileStatement stmt next = go stmt next where

  go (AutoStatement _ _ sub) next = go sub next
  go (ExtrnStatement _ _ sub) next = go sub next
  go (RValueStatement line ex) next = addNode line (Eval ex next)
  go (ReturnStatement line (Just ex)) next = addNode line (Return ex)
  go (ReturnStatement line Nothing) next = addNode line (Null)
  go (NullStatement line) next = return next

  go (ConditionalStatement line ex body Nothing) next = do
    n1 <- go body next
    n <- addNode line (IfGoto ex n1 next)
    return n

  go (ConditionalStatement line ex body1 (Just body2)) next = do
    n2 <- go body2 next
    n1 <- go body1 next
    addNode line (IfGoto ex n1 n2)

  go (WhileStatement line ex body) next = do  
    here <- generate
    pushBreakable next
    n1 <- go body here
    popBreakable
    addNodeWithId here (Node line (IfGoto ex n1 next))
    return here

  go (SwitchStatement line ex body) next = do
    old <- swapSwitchTable (Just [])
    pushBreakable next
    _ <- go body next
    popBreakable
    mnew <- swapSwitchTable old
    let Just new = mnew
    addNode line (Switch ex new next)
    
  go (GotoStatement line ex) next = do
    let NameExpr name = ex
    finalLabelMap <- ask
    let target = finalLabelMap M.! name
    addNode line (Goto target)

  go (BreakStatement line) next = do
    n <- currentBreakTarget
    addNode line (Goto n)

  go (LabelStatement _ name sub) next = do
    n <- go sub next
    addLabel name n
    return n

  go (CaseStatement line k sub) next = do
    n <- go sub next
    addCase line k n
    return n

  go (CompoundStatement _ [])     next = return next
  go (CompoundStatement _ (s:ss)) next = do
    n2 <- go (CompoundStatement 0 ss) next
    n1 <- go s n2
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

showGraph :: (a -> String) -> CodeGraph a -> String
showGraph sh gr = unlines (Prelude.map f (IM.toList gr)) where
  f (here, Node line op) = show here ++ ":\t" ++ g op where
    g (Goto n) = "Goto " ++ show n
    g (IfGoto ex n1 n2) = "IfGoto[" ++ sh ex ++ "] " ++ show n1 ++ " " ++ show n2
    g (Eval ex next) = "Eval[" ++ sh ex ++ "] " ++ show next
    g (Return ex) = "Return[" ++ sh ex ++ "]"
    g Null = "Null"
    g (Switch ex table next) = "Switch[" ++ sh ex ++ "] " ++ h table ++ " " ++ show next
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



-- generate frame layout. if args are x, y and auto a, b[4], c, d
-- then we configure the stack during a call like (stack grows down)
--       y
-- +1 -> x
-- bp -> oldbase
-- -1 -> d
--       c
--       bonus
--       b[3]
--       b[2]
--       b[1]
--       b[0]
-- -8 -> b     (initialize to -8 + 1)
-- -9 -> a
-- sp -> \
analyzeFrameLayout :: FunctionDef -> [FrameObj]
analyzeFrameLayout (FunctionDef _ _ params body) = autos ++ argos where
  bodyAutos = autoVariablesInBody body
  autoSize = sum (map measure bodyAutos)
  (_, autos) = mapAccumL f (-autoSize) bodyAutos
  f b (name, Nothing)   = (b+1,      FrameObj name AutoVar 1 b)
  f b (name, Just size) = (b+size+2, FrameObj name AutoVec (size+2) b)
  measure (_, Nothing)   = 1
  measure (_, Just size) = size + 2 -- 1 ptr var and 1 bonus cell at the end
  argos = zipWith (\i name -> FrameObj name Argument 1 i) [1..] params

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




compile :: Boron -> Either (Int,String) (Borate LinkMe)
compile (Boron defs) = go [] [] [] defs where
  go as vs fs [] = return (Borate as vs fs)
  go as vs fs (DefV1 _ name Nothing : more) = go (x:as) vs fs more where
    x = (name, LMJust 0)
  go as vs fs (DefV1 _ name (Just ival) : more) = go (x:as) vs fs more where
    x = (name, ivalToLinkMe ival)
  go as vs fs (DefVN _ name Nothing ivals : more) = go as (x:vs) fs more where
    x = (name, length ivals, map ivalToLinkMe ivals)
  go as vs fs (DefVN _ name (Just size) ivals : more) = go as (x:vs) fs more where
    x = (name, size, map ivalToLinkMe ivals)
  go as vs fs (DefF fdef : more) = makeFunc fdef >>= \func -> go as vs (func : fs) more

-- recover everything you need to know about a function from its syntax
makeFunc :: FunctionDef -> Either (Int,String) Func
makeFunc fdef@(FunctionDef _ funcname params body) = case compileFunction body of
  Left (line,msg)  -> Left (line,msg)
  Right (start,gr) -> do
    let extList = extrnVariablesInBody body
    let strings = stringsInCode gr
    let layout = analyzeFrameLayout fdef
    let autoSize = frameAutoSize layout
    let localMap = frameNamesMap layout
    let vecs = frameVectorLocations layout
    let nlabels = length (labelsUsedInBody body)
    return $ Func
      { funName = funcname
      , funCode = gr
      , funAutoSize = autoSize
      , funExterns = extList
      , funLocals = localMap
      , funSize = (nlabels + 1)
      , funVectors = vecs
      , funStrings = strings
      , funStart = start }


ivalToLinkMe :: IVal -> LinkMe
ivalToLinkMe (IVConst (ConstNumber n)) = LMJust n
ivalToLinkMe (IVConst (ConstChar cs)) = LMJust (packChars cs)
ivalToLinkMe (IVConst (ConstString str)) = LMString str
ivalToLinkMe (IVName name) = LMVariable name
        


extrnVariablesInBody :: Statement -> [String]
extrnVariablesInBody = execWriter . go where
  go :: Statement -> Writer [String] ()
  go (AutoStatement _ _ stmt) = go stmt
  go (ExtrnStatement _ vars stmt) = do
    tell vars
    go stmt
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

labelsUsedInBody :: Statement -> [String]
labelsUsedInBody = execWriter . go where
  go :: Statement -> Writer [String] ()
  go (AutoStatement _ _ stmt) = go stmt
  go (ExtrnStatement _ _ stmt) = go stmt
  go (LabelStatement _ name stmt) = tell [name] >> go stmt
  go (CaseStatement _ _ stmt) = go stmt
  go (CompoundStatement _ stmts) = mapM_ go stmts
  go (ConditionalStatement _ _ stmt Nothing) = go stmt
  go (ConditionalStatement _ _ stmt1 (Just stmt2)) = do
    go stmt1
    go stmt2
  go (WhileStatement _ _ stmt) = go stmt
  go (SwitchStatement _ _ stmt) = go stmt
  go _ = return ()

stringsInCode :: CodeGraph Expr -> [String]
stringsInCode gr = foldMap f gr where
  f (Node _ (IfGoto ex _ _)) = stringsInExpr ex
  f (Node _ (Switch ex _ _)) = stringsInExpr ex
  f (Node _ (Eval ex _))     = stringsInExpr ex
  f (Node _ (Return ex))     = stringsInExpr ex
  f _ = []


stringsInExpr :: Expr -> [String]
stringsInExpr = execWriter . go where
  go :: Expr -> Writer [String] ()
  go (ParenExpr ex) = go ex
  go (ConstExpr (ConstString str)) = tell [str]
  go (ConstExpr _) = return ()
  go (AssignExpr _ e1 e2) = do
    go e1
    go e2
  go (PreIncDec _ ex) = go ex
  go (PostIncDec _ ex) = go ex
  go (AmpersandExpr ex) = go ex
  go (UnaryExpr _ ex) = go ex
  go (BinaryExpr _ e1 e2) = do
    go e1
    go e2
  go (TernaryExpr e1 e2 e3) = do
    go e1
    go e2
    go e3
  go (FunctionExpr e es) = do
    go e
    mapM_ go es
  go (NameExpr _) = return ()
  go (StarExpr ex) = go ex
  go (VectorExpr e1 e2) = do
    go e1
    go e2


borateStrings :: [Borate LinkMe] -> [String]
borateStrings = nub . foldMap f where
  f :: Borate LinkMe -> [String]
  f (Borate as vs fs) = concatMap atomStrings as ++
                        concatMap vectorStrings vs ++
                        concatMap funStrings fs
  atomStrings :: (b,LinkMe) -> [String]
  atomStrings (_, LMString str) = [str]
  atomStrings (_, _) = []
  vectorStrings :: (b,c,[LinkMe]) -> [String]
  vectorStrings (_,_,links) = catMaybes (map g links)
  g :: LinkMe -> Maybe String
  g (LMString str) = Just str
  g _ = Nothing

