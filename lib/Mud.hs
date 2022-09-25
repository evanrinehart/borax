{-# LANGUAGE LambdaCase #-}
module Mud where

import System.Directory
import System.FilePath
import Data.List
import Data.Maybe

import Syntax
import Parser
import Compile
import Eval
import Heap
import PackedString

import Control.Monad.Writer
import Data.Map (Map, (!))
import qualified Data.Map as M
-- a MUD interface to the compiler system.

-- room, the room you're in contains images of any b source files in the directory
-- or in the subdir.

-- if you compile b file you get a bo3 file. This is an incomplete program, it will
-- contain dangling references, which you can list. It also lists functions and
-- external objects defined within.

-- if you have a bunch of bo3 files which link together correctly, they can be
-- link to get a borax file. This is a full program ready to run.

-- bo3 and borax can be dumped to actual files.


-- return the filenames of all .b files
look :: FilePath -> IO [FilePath]
look path = map (path </>) . filter (".b" `isSuffixOf`) <$> listDirectory path


quickc :: FilePath -> IO (FunctionDef, CodeGraph Expr)
quickc path = do
  src <- readFile path
  let Right (Program [DefF fdef@(FunctionDef _ _ _ body)]) = parse path src
  let Right (_, g) = compileFunction body
  return (fdef,g)


-- load and run a self-contained function (main) in a file by itself
oneShot :: FilePath -> IO ()
oneShot path = do
  src <- readFile path
  case parse path src of
    Left err -> printError err
    Right (Program (DefF fdef:_)) -> do
      let FunctionDef _ funcname params body = fdef
      putStrLn ("funcname: " ++ funcname)
      putStrLn ("params: " ++ show params)
      case makeFunc fdef of
        Left (line,msg) -> print (line,msg)
        Right func -> do
          putStrLn (showGraph showExpr (funCode func))
          print func
          runShot func


data LinkMe = LMJust Int | LMString String | LMVariable String
  deriving Show

data Borate a = Borate
  { borAtoms :: [(String, a)]
  , borVectors :: [(String, Int, [a])]
  , borFuncs :: [Func] }
      deriving Show

data GlobalType = GloAtom | GloVec | GloFun
  deriving Show
data GlobalEntity a = GlobalEntity
  { gloName :: String
  , gloSize :: Int
  , gloType :: GlobalType
  , gloInit :: [a] }
      deriving Show
type GlobalEnv a = Map String (GlobalEntity a)

ivalToLinkMe :: IVal -> LinkMe
ivalToLinkMe (IVConst (ConstNumber n)) = LMJust n
ivalToLinkMe (IVConst (ConstChar cs)) = LMJust (packChars cs)
ivalToLinkMe (IVConst (ConstString str)) = LMString str
ivalToLinkMe (IVName name) = LMVariable name
        
sourceFile :: Program -> Either (Int,String) (Borate LinkMe)
sourceFile (Program defs) = go [] [] [] defs where
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

borateRawExtrns :: Borate LinkMe -> [GlobalEntity LinkMe]
borateRawExtrns (Borate as vs fs) = go where
  go = map atomNames as ++ map vectorNames vs ++ map funNames fs
  atomNames (name, link)          = GlobalEntity name 1 GloAtom [link]
  vectorNames (name, size, links) = GlobalEntity name (size+2) GloVec links
  funNames fun                    = GlobalEntity (funName fun) (funSize fun) GloFun [minus1]
  minus1 = LMJust (-1)
  
dedupExtrns :: [GlobalEntity LinkMe] -> Either String (Map String (GlobalEntity LinkMe))
dedupExtrns pile = go M.empty pile where
  go table [] = Right table
  go table (ent@(GlobalEntity name size _ links) : more) =
    case M.insertLookupWithKey (\_ x _ -> x) name ent table of
      (Nothing, table') -> go table' more
      (Just _, _) -> Left ("duplicate external name " ++ name)

-- after no duplicates assured, form a graph to figure out initialization values
-- and initialize address of string constants while you're at it.
-- the resulting graph may contain reference cycles which is the next thing to check
initializerGraph :: Map String Int -> Map String (GlobalEntity LinkMe) -> Map (String,Int) (Either String Int)
initializerGraph strings = M.fromList . map f . concat . g . h where
  h = M.toList
  g = zipWith (\i (name, (GlobalEntity _ _ _ links)) -> map (\link -> ((name,i),link)) links) [0..]
  f ((name,i), LMJust n) = ((name,i), Right n)
  f ((name,i), LMString str) = ((name,i), Right (strings M.! str))
  f ((name,i), LMVariable name') = ((name,i), Left name')
  

-- lay out global entities in address space, based on first argument.
locateEntities :: Int -> GlobalEnv a -> (Int, Map String Int)
locateEntities base genv = foldl f (base, M.empty) (M.toList genv) where
  f (ptr,out) (name, ent) = (ptr+gloSize ent, M.insert name ptr out)


-- this blindly traverses the global env looking for a value. 
-- freezes if there is a cycle
resolveInitializer :: GlobalEnv LinkMe -> Map String Int -> Map String Int -> LinkMe -> Int
resolveInitializer genv strings mmap (LMJust n) = n
resolveInitializer genv strings mmap (LMString str) = strings M.! str
resolveInitializer genv strings mmap (LMVariable name) = case M.lookup name genv of
  Nothing -> error ("global entity missing: " ++ name)
  Just (GlobalEntity _ size ty (link:_)) -> case ty of
    GloAtom -> resolveInitializer genv strings mmap link
    GloVec  -> mmap M.! name + 1
    GloFun  -> -1

-- 0. locate global entities in memory, generate the map
         --locateEntities
-- I. disallow duplicate named entities
         --dedupExtrns
-- II. require entities referenced by functions and each other to exist
         -- (reference check)
-- III. disallow cyclic references in initializers
         -- (cyclic initializer check)
-- IV. entity initialized to a string constant initialized with address of string
         -- already in two places
-- V. entity initialized with the name of another entity, traverse until value found
         --resolveInitializer
-- finally, given all the borates, produce the initial heap, entity map, string map, funmap
--       V . IV . III . II . I . 0

data Borax = Borax
  { bxHeap :: Heap
  , bxFuncs :: Map Int Func
  , bxNames :: Map String Int
  , bxStrings :: Map String Int
  } deriving Show

link :: [Borate LinkMe] -> Either String Borax
link bors = do
  let strings = borateStrings bors
  let (base1, stringLocs, heap1) = compileStringPlacement 100 strings

  let rawExt = concatMap borateRawExtrns bors
  genvNeedsLink <- dedupExtrns rawExt
  let (_, locs) = locateEntities base1 genvNeedsLink

  -- disallow cycles in initializers
  -- disallow missing entities (dangling reference in initializer or functions)

  let funcs = concatMap borFuncs bors
  let funcByAddr = M.fromList (map (\fun -> (locs M.! funName fun, fun)) funcs)

  let entNames = M.keys locs
  let genv :: Map String (GlobalEntity Int)
      genv = fmap (\glo -> glo { gloInit = map (resolveInitializer genvNeedsLink stringLocs locs) (gloInit glo)}) genvNeedsLink

  let heap = foldl (\h name -> putGlobal (locs ! name) (genv ! name) h) heap1 entNames
  return (Borax heap funcByAddr locs stringLocs)
  

putGlobal :: Int -> GlobalEntity Int -> Heap -> Heap
putGlobal ptr (GlobalEntity _ size GloAtom [n]) h = Heap.poke ptr n h
putGlobal ptr (GlobalEntity _ size GloVec ns) h   = Heap.memcpy ptr ((ptr+1) : ns) h
putGlobal ptr (GlobalEntity _ size GloFun [-1]) h = Heap.poke ptr (-1) h




compileStringPlacement :: Int -> [String] -> (Int, Map String Int, Heap)
compileStringPlacement base strs = foldl visit (base, M.empty, Heap.empty) strs where
  visit (p,d,m) str = (p+size, d',m') where
    packed = packString (terminate str)
    size = length packed
    d' = M.insert str p d
    m' = Heap.memcpy p packed m
    
  

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

-- parse file to get all the defs
-- within the file you have many string constants which need to be merged into the global dir
--   string constants need to be packed and stored in memory, remember where
-- 



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


