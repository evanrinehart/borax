{-# LANGUAGE BangPatterns #-}
module Baby where

import qualified Expr

import Control.Monad.State
import qualified Data.IntMap as IM; import Data.IntMap (IntMap)
import qualified Data.Map as M; import Data.Map (Map)
import Data.List (union, sortBy, minimum, intersperse)
import Data.Maybe
import Data.Char

import Data.Ord

import Debug.Trace

import Data.Void
import Text.Megaparsec hiding (State, label)
import Text.Megaparsec.Char
type Parser = Parsec Void String

data K =
  KN Int |
  KC Char |
  KCS [Char] |
  KStr String
    deriving (Eq,Show)

data E =
  EK K | --
  EVar String | --
  ECond E E E | --
  ECall E [E] | --
  EUn  Unop  E | 
  EBin Binop E E |
  EAssign E E |
  EAssignOp Binop E E |
  EPFix String E |
  EAmp E |
  EStar E |
  EVect E E
    deriving Show

data Unop =
  Minus | Bang | Tilde deriving Show
data Binop = 
  BAdd | BSub | BMul | BDiv | BMod |
  BShr | BShl | BAnd | BOr  | BXor |
  BLt  | BLte | BGt  | BGte | BEq  | BNeq
    deriving Show

data R =
  RAX | RBX | RCX | RDX |
  RDI | RSI | RBP | RSP |
  R8  | R9  | R10 | R11 |
  R12 | R13 | R14 | R15
    deriving (Eq,Ord,Show)

data Loc = LReg R | LExtra Int | LVar String | LMem MemForm
  deriving (Eq,Show)

data Operand = OK K | OL Loc | OStar R
  deriving (Eq,Show)

data MemForm = MF1 K | MF2 R | MF3 R K
  deriving (Eq,Show)

data CallConfig = C1 R Loc | C2 Loc
  deriving (Eq,Show)

type Comparison = String

data IRIns =
  IRCond Comparison [IRIns] [IRIns] |
  StabCmp (Maybe Loc) R Operand |
  Stab0 Loc Operand |
  Stab1 Loc String Operand |
  Stab2 Loc R String Operand |
  StabCall Loc Int Operand [CallConfig] |
  StabVarAddr Loc String
    deriving (Show)

type IRGen a = State [Loc] a

allTheTemps = rs ++ extras where
  rs     = map LReg   [R15,R14,R13,R12]
  extras = map LExtra [1..]

previousTemp :: Loc -> Loc
previousTemp (LExtra 1) = LReg R12
previousTemp (LExtra n) = LExtra (n-1)
previousTemp (LReg R12) = LReg R13
previousTemp (LReg R13) = LReg R14
previousTemp (LReg R14) = LReg R15

forget :: Int -> IRGen ()
forget n = replicateM_ n $ do
  current <- gets head 
  modify (previousTemp current:)

temp :: IRGen Loc
temp = state (\(l:ls) -> (l, ls))

baby :: String -> IO ()
baby str = do
  let code = evalState (ir RAX (parseE str)) allTheTemps
  putStrLn (ppIR code)

ir :: R -> E -> IRGen [IRIns]
ir dst (EK k)   = return [LReg dst `Stab0` OK k]
ir dst (EVar x) = return [LReg dst `Stab0` OL (LVar x)]
ir dst (EAmp (EVar x)) = return [LReg dst `StabVarAddr` x]
ir dst (EAmp (EStar e1)) = ir dst e1
ir dst (EAmp (EVect e1 e2)) = ir dst (EAmp (EStar (EBin BAdd e1 e2)))
ir dst (EAmp _) = error "ir: sorry, no address"

ir dst (EUn unop e1) = do
  as <- ir dst e1
  return $ as ++ [Stab1 (LReg dst) (unopKeyword unop) (OL (LReg dst))]

ir dst (ECond e1 e2 e3) = do
  (comparison, code) <- ircmp e1
  bs <- ir dst e2
  cs <- ir dst e3
  return (code ++ [IRCond comparison bs cs])

ir dst (ECall e es) = do
  let (firstSixE, moreE) = splitAt 6 es

  -- in general you need to compute the function to call
  (simpleHead, fun, headCode) <- case e of
    EVar name -> return (True, LVar name, [])
    _ -> do
      (loc, code) <- uir e
      return (False, loc, code)

  -- compute arguments 7+ and hold them in temporaries
  moreStuff <- mapM uir moreE
  let stackArgsCode = concatMap snd moreStuff
  let callConfigs2  = map (C2 . fst) moreStuff

  -- compute arguments 1 through 6 out of order to avoid clobbering
  let outOfOrders = sequenceArgs firstSixE
  argData <- forM outOfOrders $ \ae -> case aeDst ae of
    Nothing -> do
      (loc, code) <- uir (aeEx ae)
      return $ ArgData {adHome = aeHome ae, adLoc = loc, adCode = code}
    Just dr -> do
      code <- ir dr (aeEx ae)
      return $ ArgData {adHome = aeHome ae, adLoc = LReg dr, adCode = code}
  let regArgsCode = concatMap adCode argData
  let strays = filter (not . isHome) argData
  let callConfigs1 = map (\ArgData{adHome=r,adLoc=l} -> C1 r l) strays

  -- 0 or 1 temp for fun head
  -- 1 temp for each argument after 6
  -- 1 temp for each stray
  -- no longer needed
  forget (if simpleHead then 0 else 1 + length moreE + length strays)
  let callConfigs  = callConfigs1 ++ callConfigs2
  (return . concat) $
    [ headCode
    , stackArgsCode
    , regArgsCode
    , [StabCall (LReg dst) (length es) (OL fun) callConfigs ]]

ir dst (EBin BDiv e1 e2) = irdiv BDiv dst e1 e2
ir dst (EBin BMod e1 e2) = irdiv BMod dst e1 e2
ir dst (EBin binop e1 (EK k)) = do
  as <- ir dst e1
  return $ as ++ [Stab2 (LReg dst) dst (binopKeyword binop) (OK k)]
ir dst (EBin binop e1 e2) = do
  (t, bs) <- uir e2
  as <- ir dst e1
  forget 1
  return $ bs ++ as ++ [Stab2 (LReg dst) dst (binopKeyword binop) (OL t)]

ir dst (EAssign (EVar x) (EK k)) = do
  return [Stab0 (LReg dst) (OK k), Stab0 (LVar x) (OL (LReg dst))]
ir dst (EAssign (EVar x) e2) = do
  bs <- ir dst e2
  return (bs ++ [Stab0 (LVar x) (OL (LReg dst))])
ir dst (EAssign e1 (EK k)) = do
  bs <- irlv dst e1
  return (bs ++ [Stab0 (LMem (MF2 dst)) (OK k), Stab0 (LReg dst) (OK k)])
ir dst (EAssign e1 e2) = do
  (t, bs) <- uir e2
  as <- irlv dst e1
  forget 1
  return (bs ++ as ++ [Stab0 (LMem (MF2 dst)) (OL t), Stab0 (LReg dst) (OL t)])

ir dst (EAssignOp binop e1 e2) = error "eassignop"

ir dst (EPFix "--_" e1) = irpre dst "--" e1
ir dst (EPFix "_--" e1) = irpost dst "--" e1
ir dst (EPFix "++_" e1) = irpre dst "++" e1
ir dst (EPFix "_++" e1) = irpost dst "++" e1


ir dst (EStar (EK k)) = do
  return [Stab0 (LReg dst) (OL (LMem (MF1 k)))]
ir dst (EStar (EBin BAdd e1 (EK k))) = do
  as <- ir dst e1
  return $ as ++ [Stab0 (LReg dst) (OL (LMem (MF3 dst k)))]
ir dst (EStar (EBin BAdd (EK k) e2)) = ir dst (EStar (EBin BAdd e2 (EK k)))
ir dst (EStar e1) = do
  as <- ir dst e1
  return (as ++ [Stab0 (LReg dst) (OStar dst)])
ir dst (EVect e1 e2) = ir dst (EStar (EBin BAdd e1 e2))

uir :: E -> IRGen (Loc, [IRIns])
uir e = do
  loc <- temp
  case loc of
    LReg dst -> do
      outs <- ir dst e
      return (loc, outs)
    LExtra i -> do
      outs <- ir R11 e
      return (loc, outs ++ [loc `Stab0` OL (LReg R11)])

-- generate code to do a comparison in context of a conditional
-- i.e. we don't need the actual result, only flags
ircmp :: E -> IRGen (Comparison, [IRIns])
ircmp (EBin BLt e1 e2) = do
  (loc,as) <- uir e2
  bs <- ir RAX e1
  forget 1
  return ("nlt", as ++ bs ++ [StabCmp Nothing RAX (OL loc)])
ircmp other = do
  code <- ir RAX other
  return ("nz", code ++ [StabCmp Nothing RAX (OK (KN 0))])

irdiv :: Binop -> R -> E -> E -> IRGen [IRIns]
irdiv op dst e1 e2 = do 
  (loc, bs) <- uir e2
  as <- ir RAX e1
  forget 1
  return (bs ++ as ++ [(LReg dst `Stab2` RAX) (binopKeyword op) (OL loc)])

irpre dst plusOrMinus e1 = do
  as <- irlv dst e1
  return $ as ++
    [Stab1 (LMem (MF2 dst)) (pfixKeyword plusOrMinus) (OStar dst)
    ,Stab0 (LReg dst) (OStar dst)]
irpost dst plusOrMinus e1 = do
  as <- irlv dst e1
  return $ as ++
    [Stab0 (LReg R11) (OStar dst)
    ,Stab1 (LMem (MF2 dst)) (pfixKeyword plusOrMinus) (OStar dst)
    ,Stab0 (LReg dst) (OL (LReg R11))]

pfixKeyword "++" = "increment"
pfixKeyword "--" = "decrement"
  

-- compute the lvalue of an lvalue expression into a register.
irlv :: R -> E -> IRGen [IRIns]
irlv dst (EVar x) = return [StabVarAddr (LReg dst) x]
irlv dst (EStar e1) = ir dst e1
irlv dst (EVect e1 e2) = ir dst (EBin BAdd e1 e2)
irlv dst _ = error "sorry, no lvalue"

{-


codegenExpr :: Int -> E -> (AsmBuilder, Int)
codegenExpr g expr =
  let ir  = stackIR HintRAX expr
      sd' = execState (mapM_ cmdToCode ir) (dummySD { sdLabelGen = g })
  in (sdBuilder sd', sdLabelGen sd')

codegen :: [Cmd] -> AsmBuilder
codegen ir =
  let sd' = execState (mapM_ cmdToCode ir) (dummySD { sdLabelGen = 1 })
  in sdBuilder sd'

runStackGen :: State StackData a -> StackData -> AsmBuilder
runStackGen act sd = sdBuilder (execState act sd)

useHint :: Hint -> State StackData Location
useHint NoHint = popReserve
useHint hint   = return (hintToLocation hint)

cmdToCode :: Cmd -> State StackData ()
cmdToCode (Cmd hint ins) = case ins of
  PushA at -> do
    dst <- useHint hint
    pushActive (traceShow ("cmdToCode PushA", "hint=", hint, "ins=", ins) dst)
    utter (asm (MOV (locationOperand dst) (atomOperand at)))

  -- WRONG --
  Quotient maybeArg -> do
    error "quotient codegen?"

  Wrangle ixs -> do
    error "wrangle: bring values in reserve into proper locations"

  Call2 maybeName -> do

    case maybeName of
      Just name -> do
        utter (asm (CALL (SYM name)))
      Nothing   -> do
        fun <- consume
        utter (asm (CALL (functionOperand fun)))

    case hint of
      HintRAX -> do
        pushActive (InRegister RAX)
      _ -> do
        dst <- useHint hint
        pushActive dst
        utterMove dst (InRegister RAX)

  Cond body1 body2 -> do
    label1 <- genLabel
    label2 <- genLabel
    scrute <- consume
    utter (asm (CMP (locationOperand scrute) (IMM 0)))
    utter (asm (JZ label1))
    mapM_ cmdToCode body1
    -- wherever the result is, it needs to match next block
    loc1 <- peekActive
    utter (asm (JMP label2))
    utter (label label1)
    mapM_ cmdToCode body2
    loc2 <- peekActive
    when (loc1 /= loc2) $ do
      adjustActiveTo loc1
    utter (label label2)

popReserve :: State StackData Location
popReserve = do
  stuff <- gets sdReserve
  let x:xs = stuff
  modify (\sd -> sd { sdReserve = xs })
  return x

pushReserve :: Location -> State StackData ()
pushReserve loc = do
  xs <- gets sdReserve
  modify (\sd -> sd { sdReserve = loc : xs })

pushActive :: Location -> State StackData ()
pushActive loc = do
  xs <- gets sdActive
  modify (\sd -> sd { sdActive = loc : xs })

popActive :: State StackData Location
popActive = do
  stuff <- gets sdActive
  let x:xs = stuff
  modify (\sd -> sd { sdActive = xs })
  return x

peekActive :: State StackData Location
peekActive = do
  stuff <- gets sdActive
  let x:xs = stuff
  return x

genLabel :: State StackData String
genLabel = do
  g <- gets sdLabelGen
  modify (\sd -> sd { sdLabelGen = g + 1 })
  return (".l" ++ show g)

consume :: State StackData Location
consume = do
  loc <- popActive
  pushReserve loc
  return loc

consumeFrom :: Location -> State StackData ()
consumeFrom r1 = do
  r2 <- consume
  if r1 /= r2
    then utter (asm (MOV (locationOperand r1) (locationOperand r2)))
    else return ()

-- the operand to access a location
-- register   - itself
-- extra i    - stack extrabase + 8 * i
-- local x    - stack localsbase + 8 * i
-- global x   - operand is a symbol
locationOperand :: Location -> A
locationOperand (InRegister r) = r
locationOperand (InExtra i) = Brack2 RBP (IMM (negate (8 * i)))

-- operand for an atom, which is what I called and should rename,
-- something which can be compiled directly into the instruction
-- instead of computed separately and then referenced by register
-- number - itself
-- variable - right now, assumes a global
-- variable - but it could be a local / argument
atomOperand :: Atom -> A
atomOperand (AN z) = IMM z
atomOperand (AV x) = Brack (SYM x)

utter :: AsmBuilder -> State StackData ()
utter b' = do
  b <- gets sdBuilder
  modify (\sd -> sd { sdBuilder = b <> b' })

adjustActiveTo :: Location -> State StackData ()
adjustActiveTo dst = do
  src <- popActive
  pushActive dst
  utterMove dst src

utterMove :: Location -> Location -> State StackData ()
utterMove dst src = do
  utter (asm (MOV (locationOperand dst) (locationOperand src)))
-}


ppE (EK k) = showK k
ppE (EVar x) = x
ppE (ECond e1 e2 e3) = ppE e1 ++ " ? " ++ ppE e2 ++ " : " ++ ppE e3
ppE (ECall e es) = "Call("++ppE e++")["++concat (intersperse "," (map ppE es))++"]"


ppIR :: [IRIns] -> String
ppIR ins = unlines $ concatMap g ins where
  g :: IRIns -> [String]
  g (IRCond c body1 body2) = 
    let spc = replicate 6 ' '
        ls1 = concatMap g body1 :: [String]
        ls2 = concatMap g body2 :: [String]
        h1 = "if " ++ c ++ " { "
        h2 = "else " ++ replicate (length c - 2) ' ' ++ " { "
    in map id (f h1 ls1) ++
       map id (f h2 ls2)
  g (Stab0 d opd)      = [concat [showLoc d, " ← ", showOpd opd]]
  g (Stab1 d code opd) = [concat [showLoc d, " ← ", code, " ", showOpd opd]]
  g (Stab2 d r code opd) =
    [concat [showLoc d, " ← ", showR r, " ", code, " ", showOpd opd]]
  g (StabCall d n fun configs) =
    [concat [showLoc d, " ← call", show n, " ", showOpd fun, showConfigs configs]]
  g (StabVarAddr d name) = [concat [showLoc d, " ← ", "&", name]]
  g (StabCmp md r opd) =
    [concat [maybe "_" showLoc md, " ← cmp ", showR r, " ", showOpd opd]]
  f :: String -> [String] -> [String]
  f heading [l]    = [heading ++ l ++ " }"]
  f heading (l:ls) =
    let end = last ls in
    let mid = init ls in
    let spc = replicate (length heading) ' ' in
    [heading ++ l] ++ map (spc ++) mid ++ [spc ++ end ++ " }"]

showK :: K -> String
showK (KN z)     = show z
showK (KC c)     = Expr.showWideChar [c]
showK (KCS cs)   = Expr.showWideChar cs
showK (KStr str) = show str

showOpd :: Operand -> String
showOpd (OL l) = showLoc l
showOpd (OK k) = showK k
showOpd (OStar r) = "*" ++ showR r

showLoc :: Loc -> String
showLoc (LReg r) = showR r
showLoc (LVar x) = x
showLoc (LExtra i) = "extra" ++ show i
showLoc (LMem (MF2 r)) = "*" ++ showR r
showLoc (LMem (MF1 k)) = "*" ++ showK k
showLoc (LMem (MF3 r k)) = "*(" ++ showR r ++ " + " ++ showK k ++ ")"

showR :: R -> String
showR r = map toLower (show r)

showConfigs :: [CallConfig] -> String
showConfigs [] = ""
showConfigs cs = "(" ++ concat (intersperse "," (map showConfig cs)) ++ ")" 

showConfig :: CallConfig -> String
showConfig (C1 r l) = showR r ++ " ← " ++ showLoc l
showConfig (C2 l) = "push(" ++ showLoc l ++ ")"


{-

ppAsm :: AsmBuilder -> String
ppAsm (AsmBuilder f) = unlines $ map g (f []) where
  g :: Either String Asm -> String
  g (Left l) = l
  g (Right mne) = case mne of
    CMP x y -> "cmp " ++ showA x ++ ", " ++ showA y
    MOV x y -> "mov " ++ showA x ++ ", " ++ showA y
    JMP l -> "jmp " ++ l
    JZ l -> "jz " ++ l
    CALL x -> "call " ++ showA x

showA :: A -> String
showA a = f a where
  f RAX = "rax"
  f RBX = "rbx"
  f RCX = "rcx"
  f RDX = "rdx"
  f RDI = "rdi"
  f RSI = "rsi"
  f RBP = "rbp"
  f RSP = "rsp"
  f R8  = "r8"
  f R9  = "r9"
  f R10 = "r10"
  f R11 = "r11"
  f R12 = "r12"
  f R13 = "r13"
  f R14 = "r14"
  f R15 = "r15"
  f (IMM z) = show z
  f (SYM name) = name
  f (Brack a) = "[" ++ showA a ++ "]"
  f (Brack2 a b) = "[" ++ showA a ++ " + " ++ showA b ++ "]"
-}




parseE :: String -> E
parseE str = case runParser (space >> anyExpr) "unknown" str of
  Left eb -> error (show eb)
  Right e -> e




unopKeyword :: Unop -> String
unopKeyword Minus = "negate"
unopKeyword Tilde = "complement"
unopKeyword Bang  = "not"

binopKeyword :: Binop -> String
binopKeyword = f where
  f BAdd = "+"
  f BSub = "-"
  f BMul = "*"
  f BDiv = "/"
  f BMod = "%"
  f BShr = ">>"
  f BShl = "<<"
  f BAnd = "&"
  f BOr  = "|"
  f BXor = "^"
  f BLt  = "<"
  f BLte = "<="
  f BGt  = ">"
  f BGte = ">="
  f BEq  = "=="
  f BNeq = "!="


-- argument expression resequencing

data Clobbers =
  ClobbersEverything |
  ClobbersRDX |
  ClobbersNothing
    deriving (Eq,Ord,Show)

data ArgEx = ArgEx
  { aeIx   :: Int
  , aeEx   :: E
  , aeHome :: R -- nominal destination for this arg
  , aeDst  :: Maybe R -- destination override, home or nothing
  }

data ArgData = ArgData
  { adHome :: R
  , adLoc  :: Loc
  , adCode :: [IRIns] }

sequenceArgs :: [E] -> [ArgEx]
sequenceArgs ps = 
  let nps   = zipWith (,) [1..] ps
      ces   = sequenceCEs nps
      crdxs = sequenceCRDXs nps
      cns   = sequenceNeutrals nps
  in ces ++ crdxs ++ cns

isHome :: ArgData -> Bool
isHome ArgData{ adHome = r1, adLoc = LReg r2 } = r1 == r2
isHome _                                       = False

clobbersWhat :: E -> Clobbers
clobbersWhat e = f e where
  f (ECall _ _) = ClobbersEverything
  f (EBin BDiv e1 e2) = minimum [f e1, f e2, ClobbersRDX]
  f (EBin BMod e1 e2) = minimum [f e1, f e2, ClobbersRDX]
  f (ECond e1 e2 e3) = minimum [f e1, f e2, f e3]
  f _ = ClobbersNothing

doesClobberEverything :: E -> Bool
doesClobberEverything e = case clobbersWhat e of
  ClobbersEverything -> True
  _ -> False

doesClobberRDX :: E -> Bool
doesClobberRDX e = case clobbersWhat e of
  ClobbersRDX -> True
  _ -> False

doesn'tClobberAnything :: E -> Bool
doesn'tClobberAnything e = case clobbersWhat e of
  ClobbersNothing -> True
  _ -> False

sequenceCEs :: [(Int,E)] -> [ArgEx]
sequenceCEs es =
  let es' = filter (doesClobberEverything . snd) es
      eraserLen = length es' - 1
      es'1 = take eraserLen es'
      es'2 = drop eraserLen es'
      f (i,e) = ArgEx i e (callreg i) Nothing
      g (i,e) = ArgEx i e (callreg i) (Just (callreg i))
  in map f es'1 ++ map g es'2

sequenceCRDXs :: [(Int,E)] -> [ArgEx]
sequenceCRDXs es = 
  let es' = filter (doesClobberRDX . snd) es
      g (i,e) = ArgEx i e (callreg i) (Just (callreg i))
      weight ArgEx{ aeIx=3 } = 5
      weight _               = 4
  in sortBy (comparing weight) (map g es')

sequenceNeutrals :: [(Int,E)] -> [ArgEx]
sequenceNeutrals es =
  let es'     = filter (doesn'tClobberAnything . snd) es
      g (i,e) = ArgEx i e (callreg i) (Just (callreg i))
  in map g es'

callreg :: Int -> R
callreg n = f n where
  f 1 = RDI
  f 2 = RSI
  f 3 = RDX
  f 4 = RCX
  f 5 = R8
  f 6 = R9
  f _ = error ("callreg " ++ show n)

{-
data Asm =
  CMP A A |
  MOV A A |
  JMP String |
  JZ String |
  IDIV A |
  CALL A
    deriving Show

data A =
  RAX | RBX | RCX | RDX |
  RDI | RSI | RBP | RSP |
  R8  | R9  | R10 | R11 |
  R12 | R13 | R14 | R15 |
  IMM Int | SYM String | Brack A | Brack2 A A
    deriving (Eq,Ord,Show)

data Location =
  InRegister A |
  InExtra Int |
  OnStack Int |
  Global String
    deriving (Eq,Ord,Show)

newtype AsmBuilder = AsmBuilder { buildAsm :: [Either String Asm] -> [Either String Asm] }

instance Show AsmBuilder where
  show (AsmBuilder f) = show (f [])

asm :: Asm -> AsmBuilder
asm a = AsmBuilder (\rest -> Right a : rest)

label :: String -> AsmBuilder
label l = AsmBuilder (\rest -> Left l : rest)

instance Semigroup AsmBuilder where
  AsmBuilder g <> AsmBuilder f = AsmBuilder (g . f)

instance Monoid AsmBuilder where
  mempty = AsmBuilder id
-}

-- expression parsers, they are factored to remove left recursion
anyExpr :: Parser E
anyExpr = assignChain

assignChain :: Parser E
assignChain = chainr1 ternaries assignmentOp

ternaries :: Parser E
ternaries = do
  a <- bitOrChain 
  questionMaybe <- optional (char '?')
  remspace
  case questionMaybe of
    Nothing -> return a
    Just _ -> do
      b <- anyExpr
      char ':'
      remspace
      c <- anyExpr
      return (ECond a b c)

bitOrChain :: Parser E
bitOrChain = chainl1 bitAndChain op where
  op = wrapbin BOr <$ (char '|' >> remspace)

bitAndChain :: Parser E
bitAndChain = chainl1 equalityChain op where
  op = wrapbin BAnd <$ (char '&' >> remspace)

equalityChain :: Parser E
equalityChain = chainl1 relationalChain op where
  op =
    wrapbin BEq  <$ (string "==" >> remspace) <|>
    wrapbin BNeq <$ (string "!=" >> remspace)

relationalChain :: Parser E
relationalChain = chainl1 shiftChain op where
  op =
    wrapbin BLte <$ (string "<=" >> remspace) <|>
    wrapbin BLt  <$ (   char '<' >> remspace) <|>
    wrapbin BGte <$ (string ">=" >> remspace) <|>
    wrapbin BGt  <$ (   char '>' >> remspace)

shiftChain :: Parser E
shiftChain = chainl1 additiveChain shiftOp

additiveChain :: Parser E
additiveChain = chainl1 multChain additiveOp

multChain :: Parser E
multChain = chainl1 unaryExpr multOp

unaryExpr :: Parser E
unaryExpr = do
  wrappers <- reverse <$> many prefixOp
  body <- primaryWithPostOp
  return (foldl (\e f -> f e) body wrappers)

primaryWithPostOp :: Parser E
primaryWithPostOp = do
  body <- primaryExpr
  wrappers <- many postfixOp
  return (foldl (\e f -> f e) body wrappers)

primaryExpr :: Parser E
primaryExpr = do
  h <- nameExpr <|> constExpr <|> parenExpr
  suffixes <- many (primaryEtc1 <|> primaryEtc2)
  return (foldl (.) id suffixes h)

nameExpr :: Parser E
nameExpr = do
  x <- validName
  remspace
  return (EVar x)

constExpr :: Parser E
constExpr = do
  k <- constant
  return (EK k)

parenExpr :: Parser E
parenExpr = do
  char '('
  remspace
  body <- anyExpr
  char ')'
  remspace
  return body

primaryEtc1 :: Parser (E -> E)
primaryEtc1 = do
  char '['
  remspace
  body <- anyExpr
  char ']'
  remspace
  return (\h -> EVect h body)

primaryEtc2 :: Parser (E -> E)
primaryEtc2 = do
  char '('
  remspace
  args <- anyExpr `sepBy` (char ',' >> remspace)
  char ')'
  remspace
  return (\h -> ECall h args)

-- operator parsers

prefixOp :: Parser (E -> E)
prefixOp =
  wrapunary Bang <$ (char '!' >> remspace) <|>
  wrapunary Tilde <$ (char '~' >> remspace) <|>
  starE <$ (char '*' >> remspace) <|>
  ampE  <$ (char '&' >> remspace) <|>
  plusPlusE   <$ (string "++" >> remspace) <|>
  minusMinusE <$ (string "--" >> remspace) <|>
  wrapunary Minus <$ (char '-' >> remspace)

postfixOp :: Parser (E -> E)
postfixOp =
  ePlusPlus   <$ (string "++" >> remspace) <|>
  eMinusMinus <$ (string "--" >> remspace)

ePlusPlus e   = EPFix "_++" e
eMinusMinus e = EPFix "_--" e
plusPlusE e   = EPFix "++_" e
minusMinusE e = EPFix "--_" e
ampE e        = EAmp e
starE e       = EStar e
wrapunary op e = EUn op e

multOp :: Parser (E -> E -> E)
multOp =
  wrapbin BMod <$ (char '%' >> remspace) <|>
  wrapbin BMul <$ (char '*' >> remspace) <|>
  wrapbin BDiv <$ (char '/' >> remspace)

additiveOp :: Parser (E -> E -> E)
additiveOp = 
  wrapbin BAdd  <$ (char '+' >> remspace) <|>
  wrapbin BSub <$ (char '-' >> remspace)

shiftOp :: Parser (E -> E -> E)
shiftOp =
  wrapbin BShl <$ (string "<<" >> remspace) <|>
  wrapbin BShr <$ (string ">>" >> remspace)

wrapbin :: Binop -> E -> E -> E
wrapbin op e1 e2 = EBin op e1 e2

assignmentOp :: Parser (E -> E -> E)
assignmentOp = do
  char '='
  mop <- optional binaryOp
  remspace
  case mop of
    Nothing -> return (\e1 e2 -> EAssign e1 e2)
    Just op -> return (\e1 e2 -> EAssignOp op e1 e2)

chainl1 :: Parser term -> Parser (term -> term -> term) -> Parser term
chainl1 term op = do
  t0 <- term
  loop t0 where
    loop accum = do
      combineMaybe <- optional op
      case combineMaybe of
        Nothing -> return accum
        Just combine -> do
          t <- term
          loop (accum `combine` t)

chainr1 :: Parser term -> Parser (term -> term -> term) -> Parser term
chainr1 term op = do
  t0 <- term
  builder <- loop
  return (builder t0) where
    loop = do
      combineMaybe <- optional op
      case combineMaybe of
        Nothing -> return id
        Just combine -> do
          t <- term
          builder <- loop
          return (\pre -> pre `combine` builder t)

remspace :: Parser ()
remspace = do
  space
  many $ do
    string "/*"
    anySingle `skipManyTill` (string "*/")
    space
  return ()

remspace1 :: Parser ()
remspace1 = do
  spaceChar
  remspace

binaryOp :: Parser Binop
binaryOp =
  BOr  <$ char '|' <|>
  BAnd <$ char '&' <|>
  BEq  <$ string "==" <|>
  BNeq <$ string "!=" <|>
  BLt  <$ char '<' <|>
  BLte <$ string "<=" <|>
  BGt  <$ char '>' <|>
  BGte <$ string ">=" <|>
  BShl <$ string "<<" <|>
  BShr <$ string ">>" <|>
  BAdd <$ char '+' <|>
  BSub <$ char '-' <|>
  BMod <$ char '%' <|>
  BMul <$ char '*' <|>
  BDiv <$ char '/'

constant :: Parser K
constant =
  KN <$> numericConstant <|>
  KStr <$> stringConstant <|>
  charConstant

numericConstant :: Parser Int
numericConstant = do
  ds <- some digitChar
  remspace
  case ds of
    "0"        -> return 0
    ('0':more) -> return (funnyOctal more)
    _          -> return (read ds)

funnyOctal :: String -> Int
funnyOctal = foldl1 (\a o -> a*8 + o) . map digitToInt

stringConstant :: Parser String
stringConstant = do
  char '"'
  str <- escapeSequence `manyTill` char '"'
  remspace
  return str

charConstant :: Parser K
charConstant = do
  char '\''
  cs <- escapeSequence `manyTill` char '\''
  remspace
  case cs of
    [c] -> pure (KC c)
    _   -> pure (KCS cs)

escapeSequence :: Parser Char
escapeSequence = do
  c <- printChar
  case c of
    '*' -> do
      c2 <- printChar
      case c2 of
        '0' -> return '\0'
        'e' -> return '\EOT'
        '(' -> return '{'
        ')' -> return '}'
        't' -> return '\t'
        '*' -> return '*'
        '\'' -> return '\''
        '"' -> return '"'
        'n' -> return '\n'
        _ -> fail "unknown escape sequence"
    '\'' -> fail "empty char constant"
    _ -> return c  

validName :: Parser String
validName = do
  c <- letterChar <|> char '_' <|> char '.'
  cs <- many (alphaNumChar <|> char '_' <|> char '.')
  remspace
  case c:cs of
    "auto"   -> fail "auto is a reserved word"
    "extrn"  -> fail "extrn is a reserved word"
    "case"   -> fail "case is a reserved word"
    "if"     -> fail "if is a reserved word"
    "while"  -> fail "while is a reserved word"
    "switch" -> fail "switch is a reserved word"
    "goto"   -> fail "goto is a reserved word"
    "break"  -> fail "break is a reserved word"
    "return" -> fail "return is a reserved word"
    _ -> return (c : cs)

