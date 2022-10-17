module Prepare where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe
import Data.Fix

import Syntax
import Expr
import PackedString
import Doc

import Data.List
import Data.IntMap as IM hiding (filter, map)
import Data.Map as M hiding (filter, map)

{- TODO
 - consistency check for case statements outside a switch
 - if we take care of this prior to `compile' it simplifies the process
 -}

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

-- A local monad to compile a function (Reader + State)
type Compile a = ReaderT (Map String Int) (State CompileData) a

data CompileData = CompileData
  { cdBreakable   :: [Int]
  , cdLabelMap    :: Map String Int
  , cdSwitchTable :: Maybe [(K,Int)]
  , cdGenerator   :: Int
  , cdGraph       :: CodeGraph E
  } deriving Show

data OpCode a =
  Goto Int |
  IfGoto a Int Int |
  Switch a [(K,Int)] Int |
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
  , funCode :: CodeGraph E
  , funAutoSize :: Int
  , funExterns :: [String]
  , funLocals :: Map String Int
  , funSize :: Int -- func occupies this amount of memory in words
  , funVectors :: [Int] -- for i in vecs, *(bp-i) = bp-i
  , funStrings :: [String]
  , funStart :: Int
  } deriving Show


instance Show a => Show (Node a) where
  show (Node l op) = "Node(l="++show l++", "++ "NO" ++ ")"
  

compileFunction :: Statement -> (Int, CodeGraph E)
compileFunction stmt =
  let
    -- Blank records to work with
    blankGraph = IM.fromList [(0, (Node 0 Null))]
    blankData  = CompileData [] M.empty Nothing 1 blankGraph

    -- Run the compilation of function body. gotoTable (from the future) is provided
    algorithmResults = runCompile blankData gotoTable (compileStatement stmt 0)

    -- ! --
    (_, CompileData _ gotoTable _ _ _) = algorithmResults
  in
    fmap cdGraph algorithmResults


runCompile :: CompileData -> Map String Int -> Compile a -> (a, CompileData)
runCompile s cheats action = runState (runReaderT action cheats) s


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
    let EVar name = ex
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

swapSwitchTable :: Maybe [(K,Int)] -> Compile (Maybe [(K,Int)])
swapSwitchTable sm' = do
  sm <- gets cdSwitchTable
  modify (\s -> s { cdSwitchTable = sm' })
  return sm

getSwitchTable :: Int -> Compile [(K,Int)]
getSwitchTable lineNo = do
  mtable <- gets cdSwitchTable
  case mtable of
    Nothing    -> error "getSwitchTable, no switch here"
    Just table -> return table

addLabel :: String -> Int -> Compile ()
addLabel name n = modify (\s -> s { cdLabelMap = M.insert name n (cdLabelMap s) })

addNodeWithId :: Int -> Node E -> Compile ()
addNodeWithId n node = modify (\s -> s { cdGraph = IM.insert n node (cdGraph s) })

addNode :: Int -> OpCode E -> Compile Int
addNode line opcode = do
  n <- generate
  addNodeWithId n (Node line opcode)
  return n

addCase :: Int -> K -> Int -> Compile ()
addCase lineNo k n = do
  table <- getSwitchTable lineNo
  modify (\s -> s { cdSwitchTable = Just ((k,n):table) })

generate :: Compile Int
generate = do
  g <- gets cdGenerator
  modify (\s -> s { cdGenerator = g + 1 })
  return g


-- debug print

showGraph :: (a -> Doc) -> CodeGraph a -> Doc
showGraph sh gr = joinDocs newline (map f (IM.toList gr)) where
  f (here, Node _ op) = showN here <> text ":" <> tab <> showOpcode sh op

showOpcode :: (a -> Doc) -> OpCode a -> Doc
showOpcode sh = g where
  g (Goto n)               = showForm "Goto" (showN n) []
  g (IfGoto ex n1 n2)      = showForm "IfGoto" (sh ex) [showN n1, showN n2]
  g (Eval ex next)         = showForm "Eval" (sh ex) [showN next]
  g (Return ex)            = showForm "Return" (sh ex) []
  g Null                   = text "Null"
  g (Switch ex table next) = showForm "Switch" (sh ex) [showSwitchTable table, showN next]

showForm :: String -> Doc -> [Doc] -> Doc
showForm heading body straglers =
  text heading <>
  text "[" <> body <> text "]" <>
  joinDocs (text " ") straglers

showSwitchTable :: [(K,Int)] -> Doc
showSwitchTable table = text "{" <> joinDocs (text ",") ps <> text "}" where
  ps = map f table
  f (k,n) = showK k <> text "=>" <> showN n
  



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




compile :: FileAST -> Either String (Borate LinkMe)
compile (FileAST defs) = go [] [] [] defs where
  go as vs fs [] = return (Borate as vs fs)
  go as vs fs (DefV1 _ name Nothing : more) = go (x:as) vs fs more where
    x = (name, LMJust 0)
  go as vs fs (DefV1 _ name (Just ival) : more) = go (x:as) vs fs more where
    x = (name, ivalToLinkMe ival)
  go as vs fs (DefVN _ name Nothing ivals : more) = go as (x:vs) fs more where
    x = (name, length ivals, map ivalToLinkMe ivals)
  go as vs fs (DefVN _ name (Just size) ivals : more) = go as (x:vs) fs more where
    x = (name, size, map ivalToLinkMe ivals)
  go as vs fs (DefF fdef : more) = let func = makeFunc fdef in go as vs (func : fs) more

-- recover everything you need to know about a function from its syntax
makeFunc :: FunctionDef -> Func
makeFunc fdef@(FunctionDef _ funcname params body) =
  let (start,gr) = compileFunction body
      extList = extrnVariablesInBody body
      strings = stringsInCode gr
      layout = analyzeFrameLayout fdef
      autoSize = frameAutoSize layout
      localMap = frameNamesMap layout
      vecs = frameVectorLocations layout
      nlabels = length (labelsUsedInBody body)
  in Func
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
ivalToLinkMe (IVConst (KN n)) = LMJust n
ivalToLinkMe (IVConst (KC cs)) = LMJust (packChars cs)
ivalToLinkMe (IVConst (KStr str)) = LMString str
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

stringsInCode :: CodeGraph E -> [String]
stringsInCode gr = foldMap f gr where
  f (Node _ (IfGoto ex _ _)) = stringsInExpr ex
  f (Node _ (Switch ex _ _)) = stringsInExpr ex
  f (Node _ (Eval ex _))     = stringsInExpr ex
  f (Node _ (Return ex))     = stringsInExpr ex
  f _ = []



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

