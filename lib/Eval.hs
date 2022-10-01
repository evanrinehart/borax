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
import Data.Fix

import Data.Bits

import Syntax
import Compile
import Heap
import PackedString
import Link
import System
import Expr

import Debug.Trace

data Machine m = Machine
  { machMemory  :: Heap
  , machStackPtr :: Int
  , machBasePtr :: Int -- base + 0 is argument 1
  , machFrames :: [Map String Int] -- name => offset
  , machFuncs :: IntMap Func      -- addr => func
  , machNames :: Map String Int --   name => addr
  , machStrings :: Map String Int -- string => addr
  , machService :: ServiceCalls m
  }

type Eval m a = ExceptT String (StateT (Machine m) m) a

blankMachine :: Applicative m => Machine m
blankMachine = Machine
  { machMemory = Heap.empty
  , machStackPtr = 9999
  , machBasePtr = 9999
  , machFrames = [M.empty]
  , machFuncs = IM.empty
  , machNames = M.empty
  , machStrings = M.empty
  , machService = dummySystem }

fromBorax :: ServiceCalls m -> Borax -> Machine m
fromBorax srv bx = Machine
  { machMemory   = specialObjects `Heap.merge` bxHeap bx
  , machStackPtr = 9999
  , machBasePtr  = 9999
  , machFrames   = [M.empty]
  , machFuncs    = bxFuncs bx
  , machNames    = specialObjectLocs `M.union` bxNames bx
  , machStrings  = bxStrings bx
  , machService  = srv }

bootUp :: Machine IO -> IO (Either String Int)
bootUp mch = flip evalStateT mch . runExceptT $ do
  mainAddr <- decodeName "main"
  r <- callFunction mainAddr []
  --mem <- gets machMemory
  --liftIO (print mem)
  return r


-- list of special __IO globals
-- __IO.time.query   <!>
-- __IO.time.cachev  [2]
-- __IO.output       <!>
-- __IO.input.query  <!>
-- __IO.input.cache  [1]
-- __IO.exit         <!>
specialObjects :: Heap
specialObjects = Heap.load
  [(50,  2) -- __IO.output
  ,(51,  3) -- __IO.input.query
  ,(52,  0) -- __IO.input.cache
  ,(53,  7) -- __IO.time.query
  ,(54, 55) -- __IO.time.cache
  ,(55,  0)
  ,(56,  0)
  ,(57, 13) -- __IO.exit
  ]

specialObjectLocs :: Map String Int
specialObjectLocs = M.fromList
  [("__IO.output",      50)
  ,("__IO.input.query", 51)
  ,("__IO.input.cache", 52)
  ,("__IO.time.query",  53)
  ,("__IO.time.cachev", 54)
  ,("__IO.exit",        57)
  ]




evalR :: Monad m => ExprOf Expr -> Eval m Int
evalR (ExAssign (Fix e1) (Fix e2)) = do
  addr <- evalL e1
  val  <- evalR e2
  storeAt addr val -- could cause i/o now
  return val
evalR (ExAssignOp binop (Fix e1) (Fix e2)) = do
  addr <- evalL e1
  val1 <- loadFrom addr -- I/O?
  val2 <- evalR e2
  let val3 = arith2 binop val1 val2
  storeAt addr val3 -- I/O?
  return val3
evalR (ExPreInc (Fix ex)) = do
  addr <- evalL ex
  val  <- loadFrom addr -- could cause i/o now
  let val' = val + 1
  storeAt addr val' -- could cause i/o now
  return val'
evalR (ExPreDec (Fix ex)) = do
  addr <- evalL ex
  val  <- loadFrom addr -- could cause i/o now
  let val' = val - 1
  storeAt addr val' -- could cause i/o now
  return val'
evalR (ExPostInc (Fix ex)) = do
  addr <- evalL ex
  val  <- loadFrom addr -- could cause i/o now
  let val' = val + 1
  storeAt addr val' -- could cause i/o now
  return val
evalR (ExPostDec (Fix ex)) = do
  addr <- evalL ex
  val  <- loadFrom addr -- could cause i/o now
  let val' = val - 1
  storeAt addr val' -- could cause i/o now
  return val

evalR (ExConst k) = do
  f <- getStringFinder
  return (evalConstant f k)
evalR (ExName name) = do
  addr <- decodeName name -- local? extrn? function? label?
  loadFrom addr -- could cause i/o now
evalR (ExUnary LogicNot (Fix ex)) = do
  val <- evalR ex
  return (if val==0 then 1 else 0)
evalR (ExUnary Negative (Fix ex)) = do
  val <- evalR ex
  return (negate val)
evalR (ExUnary BitComplement (Fix ex)) = do
  val <- evalR ex
  return (complement val)
evalR (ExBinary binop (Fix ex1) (Fix ex2)) = do
  val1 <- evalR ex1
  val2 <- evalR ex2
  return (arith2 binop val1 val2)
evalR (ExTernary (Fix ex1) (Fix ex2) (Fix ex3)) = do
  val <- evalR ex1
  if val==0
    then evalR ex3
    else evalR ex2
evalR (ExStar (Fix ex)) = do
  addr <- evalR ex
  loadFrom addr -- could cause i/o now
evalR (ExAmp (Fix ex)) = do
  addr <- evalL ex
  return addr
evalR (ExFunc (Fix exfun) exargs) = do
  funaddr <- evalL exfun
  argvals <- mapM (evalR . unFix) exargs
  callFunction funaddr argvals
evalR (ExVector (Fix ex1) (Fix ex2)) = do
  base   <- evalR ex1
  offset <- evalR ex2
  loadFrom (base + offset)


-- evaluate expression in lvalue context, i.e. compute the address of it
evalL :: Monad m => ExprOf Expr -> Eval m Int
evalL (ExName name)    = decodeName name
evalL (ExStar (Fix ex))      = evalR ex -- section 4.2.2
evalL (ExVector (Fix e1) (Fix e2)) = do
  base   <- evalR e1
  offset <- evalR e2
  return (base + offset)
evalL _ = panic "illogical"

callFunction :: Monad m => Int -> [Int] -> Eval m Int
callFunction funaddr argvals = do
  fun <- findFunction funaddr
  mapM_ pushWord (reverse argvals)
  pushFrame fun argvals
  let gr    = funCode fun
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
    IfGoto (Fix ex) n1 n2 -> do
      val <- evalR ex
      runStatement gr (if val==0 then n2 else n1)
    Return (Fix ex)       -> evalR ex
    Eval (Fix ex) next    -> do
      _ <- evalR ex
      runStatement gr next
    Null            -> return 0
    Switch (Fix ex) table next -> do
      val <- evalR ex
      switchLookup table val >>= \case
        Nothing -> runStatement gr next
        Just n  -> runStatement gr n

-- further compilation can skip this computation
switchLookup :: Monad m => [(Constant,Int)] -> Int -> Eval m (Maybe Int)
switchLookup [] _ = return Nothing
switchLookup ((k,n):more) y = do
  x <- evalR (ExConst k)
  if x == y
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
  
evalConstant :: (String -> Int) -> Constant -> Int
evalConstant strings (ConstI n)   = fromIntegral n
evalConstant strings (ConstC cs)  = packChars cs
evalConstant strings (ConstS str) = strings str

getStringFinder :: Monad m => Eval m (String -> Int)
getStringFinder = do
  map <- gets machStrings
  return (map M.!)

findString :: Monad m => String -> Eval m Int
findString str = gets ((M.! str) . machStrings)

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
  let vectorHeads = funVectors func
  -- push args in reverse order
  -- stack[0] = base (old base)
  -- base = stack
  -- stack -= autosize + 1
  -- (push a table to name tables stack)
  -- (initialize auto vector head)
  -- enter new func
  oldBase <- gets machBasePtr
  stack   <- gets machStackPtr
  heapPoke stack oldBase
  modify (\mch -> mch { machBasePtr = stack, machStackPtr = stack - autoSize - 1 })
  modify (\mch -> mch { machFrames = nameTable : machFrames mch })
  base <- gets machBasePtr
  forM_ vectorHeads $ \offset -> do
    heapPoke (base + offset) (base + offset + 1)


popFrame :: Monad m => Eval m ()
popFrame = do
  bp <- gets machBasePtr
  modify (\mch -> mch { machStackPtr = bp })
  oldBase <- heapPeek bp
  modify (\mch -> mch { machBasePtr =  oldBase })
  modify (\mch -> mch { machFrames = drop 1 (machFrames mch) })

heapPeek :: Monad m => Int -> Eval m Int
heapPeek addr = gets ((`Heap.peek` addr) . machMemory)

heapPoke :: Monad m => Int -> Int -> Eval m ()
heapPoke 2 val = do
  -- output val, no other effect
  action <- gets (service_putchar . machService)
  _ <- lift (lift (action val))
  return ()
heapPoke 3 val = do
  -- request input, place in __IO.input.cache
  action <- gets (service_getchar . machService)
  c <- lift (lift action)
  dst <- decodeName "__IO.input.cache"
  heapPoke dst c
heapPoke 7 val = do
  -- request latest time, place in __IO.time.cachev[0] and [1]
  action <- gets (service_time . machService)
  (t0,t1) <- lift (lift action)
  dst <- decodeName "__IO.time.cachev"
  heapPoke (dst+1) t0
  heapPoke (dst+2) t1
heapPoke 13 val = do
  -- trigger program termination, this is intended to halt the VM
  action <- gets (service_exit . machService)
  _ <- lift (lift action)
  return ()
heapPoke addr val = modify (\mch -> mch { machMemory = Heap.poke addr val (machMemory mch) })

arith2 :: BinaryOp -> Int -> Int -> Int
arith2 binop val1 val2 = case binop of
  BitOr -> val1 .|. val2
  BitAnd -> val1 .&. val2
  Equals -> if val1 == val2 then 1 else 0
  NotEquals -> if val1 /= val2 then 1 else 0
  LessThan -> if val1 < val2 then 1 else 0
  LessThanEquals -> if val1 <= val2 then 1 else 0
  GreaterThan -> if val1 > val2 then 1 else 0
  GreaterThanEquals -> if val1 >= val2 then 1 else 0
  ShiftL -> val1 `shiftL` val2
  ShiftR -> val1 `rawShiftR` val2
  Plus -> val1 + val2
  Minus -> val1 - val2
  Modulo -> val1 `mod` val2
  Times -> val1 * val2
  Division -> val1 `div` val2


{-The binary operators << and >> are left and right shift respec-
tively. The left rvalue operand is taken as a bit pattern. The
right operand is taken as an integer shift count.  The result is
the bit pattern shifted by the shift count. Vacated bits are
filled with zeros. The result is undefined if the shift count is
negative or larger than an object length. -}
rawShiftR :: Int -> Int -> Int
rawShiftR i s = fromIntegral (let w = fromIntegral i :: Word in w `shiftR` s)


