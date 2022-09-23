{-# LANGUAGE LambdaCase #-}
module Eval where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Char
import Data.IntMap as IM hiding (map, lookup)
import qualified Data.IntMap as IM 
import Data.Map as M hiding (map, lookup, drop)
import qualified Data.Map as M 

import Data.Bits

import Syntax
import Compile
import Heap

type StringMap = Map String

data Func = Func
  { funCode :: CodeGraph Expr
  , funAutoSize :: Int
  , funExterns :: [String]
  , funLocals :: StringMap Int
  , funSize :: Int -- func occupies this amount of memory in words
  , funVectors :: [Int] -- for i in vecs, *(bp-i) = bp-i
  , funStart :: Int
  } deriving Show

data Machine = Machine
  { machMemory  :: Heap
  , machStackPtr :: Int
  , machBasePtr :: Int -- base + 0 is argument 1
  , machFrames :: [StringMap Int] -- name => offset
  , machFuncs :: IntMap Func      -- addr => func
  , machDrivers :: IntMap ()
  , machNames :: StringMap Int --   name => addr
  , machStrings :: StringMap Int -- string => addr
  , machOverlayIO :: IntMap Int --  addr => driver number
  } deriving Show

type Eval m a = ExceptT String (StateT Machine m) a

blankMachine = Machine
  { machMemory = Heap.empty
  , machStackPtr = 9999
  , machBasePtr = 9999
  , machFrames = [M.empty]
  , machFuncs = IM.empty
  , machDrivers = IM.empty
  , machNames = M.empty
  , machStrings = M.empty 
  , machOverlayIO = IM.empty }

bootUp :: Machine -> IO (Either String Int)
bootUp mch = flip evalStateT mch . runExceptT $ do
  mainAddr <- decodeName "main"
  r <- callFunction mainAddr []
  mem <- gets machMemory
  liftIO (print mem)
  return r


  
compile2 :: FunctionDef -> ()
compile2 fdef = ()
  

-- to run the code we need a interpreter loop that can
-- access and modify memory
-- follow a code graph
-- make a call to another function and get the result back
-- yield a value to whoever called you

-- names access various things:
--   local variable (located in the stack frame)
--   arguments      (located in the stack frame)
--   external variables (located in data space)
--   labels         (no storage, but they have an rvalue)
--   other functions (located in code space, has an rvalue so we can pass funcrefs around)

evalR :: Monad m => Expr -> Eval m Int
evalR (AssignExpr Nothing e1 e2) = do
  addr <- evalL e1
  val  <- evalR e2
  storeAt addr val -- could cause i/o now
  return val
evalR (AssignExpr (Just binop) e1 e2) = do
  addr <- evalL e1
  val1 <- loadFrom addr -- I/O?
  val2 <- evalR e2
  let val3 = arith2 binop val1 val2
  storeAt addr val3 -- I/O?
  return val3
evalR (PreIncDec incdec ex) = do
  addr <- evalL ex
  val  <- loadFrom addr -- could cause i/o now
  let val' = case incdec of PlusPlus -> val+1; MinusMinus -> val-1
  storeAt addr val' -- could cause i/o now
  return val'
evalR (PostIncDec incdec ex) = do
  addr <- evalL ex
  val  <- loadFrom addr -- could cause i/o now
  let val' = case incdec of PlusPlus -> val+1; MinusMinus -> val-1
  storeAt addr val' -- could cause i/o now
  return val
evalR (ConstExpr k) = case k of
  ConstNumber n -> return (fromIntegral n)
  ConstChar cs  -> return (packChars cs)
  ConstString str -> do
    addr <- findString str
    return addr
evalR (NameExpr name) = do
  addr <- decodeName name -- local? extrn? function? label?
  loadFrom addr -- could cause i/o now
evalR (UnaryExpr LogicNot ex) = do
  val <- evalR ex
  return (if val==0 then 1 else 0)
evalR (UnaryExpr Negative ex) = do
  val <- evalR ex
  return (negate val)
evalR (BinaryExpr binop ex1 ex2) = do
  val1 <- evalR ex1
  val2 <- evalR ex2
  return (arith2 binop val1 val2)
evalR (TernaryExpr ex1 ex2 ex3) = do
  val <- evalR ex1
  if val==0
    then evalR ex3
    else evalR ex2
evalR (StarExpr ex) = do
  addr <- evalR ex
  loadFrom addr -- could cause i/o now
evalR (AmpersandExpr ex) = do
  addr <- evalL ex
  return addr
evalR (FunctionExpr exfun exargs) = do
  funaddr <- evalR exfun
  argvals <- mapM evalR exargs
  callFunction funaddr argvals
evalR (VectorExpr ex1 ex2) = do
  base <- evalL ex1
  offset <- evalR ex2
  loadFrom (base + offset)

evalL :: Monad m => Expr -> Eval m Int
evalL (NameExpr name)    = decodeName name
evalL (StarExpr ex)      = evalR ex -- section 4.2.2
evalL (VectorExpr e1 e2) = do
  base   <- evalL e1
  offset <- evalR e2
  return (base + offset)
evalL _ = panic "illogical"

callFunction :: Monad m => Int -> [Int] -> Eval m Int
callFunction funaddr argvals = do
  fun <- findFunction funaddr
  mapM_ pushWord (reverse argvals)
  pushFrame fun argvals
  let gr = funCode fun
  let start = funStart fun
  r <- runStatement gr start
  popFrame
  pops (length argvals)
  return r

runStatement :: Monad m => CodeGraph Expr -> Int -> Eval m Int
runStatement gr i =
  let Node _ opcode = gr IM.! i in
  case opcode of
    Goto next       -> runStatement gr next
    IfGoto ex n1 n2 -> do
      val <- evalR ex
      runStatement gr (if val==0 then n2 else n1)
    Return ex       -> evalR ex
    Eval ex next    -> do
      _ <- evalR ex
      runStatement gr next
    Null            -> return 0
    Switch ex table next -> do
      val <- evalR ex
      switchLookup table val >>= \case
        Nothing -> runStatement gr next
        Just n  -> runStatement gr n

-- further compilation can skip this computation
switchLookup :: Monad m => [(Constant,Int)] -> Int -> Eval m (Maybe Int)
switchLookup [] _ = return Nothing
switchLookup ((k,n):more) y = do
  x <- evalR (ConstExpr k)
  if x==y
    then return (Just n)
    else switchLookup more y

panic :: Monad m => String -> Eval m a
panic msg = throwError msg

loadFrom :: Monad m => Int -> Eval m Int
loadFrom addr = do
  -- this needs to check for mmio before accessing heap
  heapPeek addr

storeAt :: Monad m => Int -> Int -> Eval m ()
storeAt addr val = do
  -- FIXME this needs to check for mmio before accessing heap
  heapPoke addr val
  

findString :: Monad m => String -> Eval m Int
findString str = do
  panic "findString X_X"

findFunction :: Monad m => Int -> Eval m Func
findFunction addr = gets ((IM.lookup addr) . machFuncs) >>= \case
  Nothing -> panic ("failed to find function at " ++ show addr)
  Just func -> return func 

decodeName :: Monad m => String -> Eval m Int
decodeName name = do
  gets ((M.lookup name) . head . machFrames) >>= \case
    Nothing -> gets ((M.lookup name) . machNames) >>= \case
      Nothing -> panic ("failed to decode name: " ++ name)
      Just addr -> return addr
    Just offset -> do
      base <- gets machBasePtr
      return (base + offset)

pushWord :: Monad m => Int -> Eval m ()
pushWord x = do
  stack <- gets machStackPtr
  heapPoke stack x
  modify (\mch -> mch { machStackPtr = stack - 1 })

pops :: Monad m => Int -> Eval m ()
pops n = modify (\mch -> mch { machStackPtr = machStackPtr mch + n })

pushFrame :: Monad m => Func -> [Int] -> Eval m ()
pushFrame func args = do
  let autoSize = funAutoSize func
  let nameTable = funLocals func
  -- push args in reverse order
  -- stack[0] = base (old base)
  -- base = stack
  -- stack -= autosize + 1
  -- (push a table to name tables stack)
  -- enter new func
  oldBase <- gets machBasePtr
  stack   <- gets machStackPtr
  heapPoke stack oldBase
  modify (\mch -> mch { machBasePtr = stack, machStackPtr = stack - autoSize - 1 })
  modify (\mch -> mch { machFrames = nameTable : machFrames mch })


popFrame :: Monad m => Eval m ()
popFrame = do
  bp <- gets machBasePtr
  modify (\mch -> mch { machStackPtr = bp })
  oldBase <- heapPeek bp
  modify (\mch -> mch { machBasePtr =  oldBase })
  modify (\mch -> mch { machFrames = drop 1 (machFrames mch) })

packChars :: [Char] -> Int
packChars = foldl1 (\a x -> a*8 + x) . map ord

heapPeek :: Monad m => Int -> Eval m Int
heapPeek addr = gets ((`Heap.peek` addr) . machMemory)

heapPoke :: Monad m => Int -> Int -> Eval m ()
heapPoke addr val = modify (\mch -> mch { machMemory = Heap.poke addr val (machMemory mch) })

arith2 :: BinaryOp -> Int -> Int -> Int
arith2 binop val1 val2 = case binop of
  BitOr -> val1 .|. val2
  BitAnd -> val1 .&. val2
  Equals -> if val1 == val2 then 1 else 0
  NotEquals -> if val1 /= val2 then 1 else 1
  LessThan -> if val1 < val2 then 1 else 0
  LessThanEquals -> if val1 <= val2 then 1 else 0
  GreaterThan -> if val1 > val2 then 1 else 0
  GreaterThanEquals -> if val1 >= val2 then 1 else 0
  ShiftL -> val1 `shiftL` val2
  ShiftR -> val1 `shiftR` val2
  Plus -> val1 + val2
  Minus -> val1 - val2
  Modulo -> val1 `mod` val2
  Times -> val1 * val2
  Division -> val1 `div` val2




runShot :: Func -> IO ()
runShot func = do
  let mch = blankMachine
              { machFuncs = IM.singleton 10 func
              , machNames = M.singleton "main" 10 }
  result <- bootUp mch
  case result of
    Left msg -> putStrLn msg
    Right i  -> do
      putStrLn ("exited with value " ++ show i)
