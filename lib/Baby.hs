{-# LANGUAGE BangPatterns #-}
module Baby where

import Control.Monad.State
import qualified Data.IntMap as IM; import Data.IntMap (IntMap)
import qualified Data.Map as M; import Data.Map (Map)

import Debug.Trace

import Data.Void
import Text.Megaparsec hiding (State, label)
import Text.Megaparsec.Char
type Parser = Parsec Void String

data E =
  ENum Int |
  EVar String |
  ECond E E E |
  ECall E E E
    deriving Show

data Asm =
  CMP A A |
  MOV A A |
  JMP String |
  JZ String |
  CALL A
    deriving Show

data A =
  RAX | RBX | RCX | RDX |
  RDI | RSI | RBP | RSP |
  R8 | R9 | R10 | R11 |
  R12 | R13 | R14 | R15 |
  IMM Int | SYM String | Brack A | Brack2 A A
    deriving (Eq,Ord,Show)

data Location =
  InRegister A |
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

functionOperand :: Location -> A
functionOperand (InRegister r) = r
functionOperand (Global name) = SYM name
functionOperand (OnStack i) = Brack2 RBP (IMM (negate (8 * i)))

-- stack based IR

data Hint = NoHint | HintRDI | HintRSI | HintRAX deriving Show
data Atom = AN Int | AV String deriving Show
data Cmd = Cmd Hint Ins deriving Show
data Ins =
  PushA Atom |
  Call2 (Maybe String) |
  Cond [Cmd] [Cmd]
    deriving Show

stackIR :: E -> [Cmd]
stackIR (ENum z) = [Cmd NoHint (PushA (AN z))]
stackIR (EVar x) = [Cmd NoHint (PushA (AV x))]
stackIR (ECall (EVar f) e1 e2) =
  stackIRHint HintRDI e1 ++
  stackIRHint HintRSI e2 ++
  [Cmd HintRAX (Call2 (Just f))]
stackIR (ECall e1 e2 e3) =
  stackIRHint HintRDI e2 ++
  stackIRHint HintRSI e3 ++
  stackIR e1 ++
  [Cmd HintRAX (Call2 Nothing)]
stackIR (ECond e1 e2 e3) = -- the two branches might leave the result in two locations
  stackIR e1 ++
  [Cmd NoHint (Cond (stackIR e2) (stackIR e3))]

stackIRHint hint e = case stackIR e of
  (Cmd NoHint ins : rest) -> Cmd hint ins : rest
  other                   -> other

data StackData = StackData
  { sdBuilder  :: AsmBuilder
  , sdLabelGen :: Int
  , sdReserve  :: [Location]
  , sdActive   :: [Location] }

dummySD :: StackData
dummySD = StackData
  { sdBuilder  = mempty
  , sdLabelGen = 1
  , sdReserve  = map InRegister [R15,R14,R13,R12] ++ map OnStack [1..]
  , sdActive   = [] }

codegenExpr :: Int -> E -> (AsmBuilder, Int)
codegenExpr g expr =
  let ir  = stackIR expr
      sd' = execState (stackGens ir) (dummySD { sdLabelGen = g })
  in (sdBuilder sd', sdLabelGen sd')

codegen :: [Cmd] -> AsmBuilder
codegen ir =
  let sd' = execState (stackGens ir) (dummySD { sdLabelGen = 1 })
  in sdBuilder sd'

runStackGen :: State StackData a -> StackData -> AsmBuilder
runStackGen act sd = sdBuilder (execState act sd)

stackGens :: [Cmd] -> State StackData ()
stackGens cmds = do
  let (sx, x) = unsnoc cmds
  mapM (stackGen False) sx
  stackGen True x

stackGen :: Bool -> Cmd -> State StackData ()
stackGen final (Cmd _ ins) = case ins of
  PushA at -> do
    dst <- if final then pure (InRegister RAX) else popReserve
    pushActive dst
    utter (asm (MOV (locationOperand dst) (atomOperand at)))

  Call2 maybeName -> do
    arg2 <- consume
    arg1 <- consume
    utter (asm (MOV RDI (locationOperand arg1)))
    utter (asm (MOV RSI (locationOperand arg2)))

    case maybeName of
      Just name -> do
        utter (asm (CALL (SYM name)))
      Nothing   -> do
        fun  <- consume
        utter (asm (CALL (functionOperand fun)))

    if final
      then do
        pushActive (InRegister RAX)
      else do
        dst <- popReserve
        pushActive dst
        utterMove dst (InRegister RAX)

  Cond body1 body2 -> do
    label1 <- genLabel
    label2 <- genLabel
    scrute <- consume
    utter (asm (CMP (locationOperand scrute) (IMM 0)))
    utter (asm (JZ label1))
    let (sx, x) = unsnoc body1
    stackGen False `mapM_` sx
    stackGen final x -- wherever the result is, it needs to match next block
    loc1 <- peekActive
    utter (asm (JMP label2))
    utter (label label1)
    let (sx, x) = unsnoc body2
    stackGen False `mapM_` sx
    stackGen final x
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

locationOperand :: Location -> A
locationOperand (InRegister r) = r
locationOperand (OnStack i) = Brack2 RBP (IMM (negate (8 * i)))

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

unsnoc :: [a] -> ([a], a)
unsnoc [] = ([], error "unsnoc []")
unsnoc [x] = ([], x)
unsnoc more = (init more, last more)



ppE (ENum z) = show z
ppE (EVar x) = x
ppE (ECond e1 e2 e3) = ppE e1 ++ " ? " ++ ppE e2 ++ " : " ++ ppE e3
ppE (ECall e1 e2 e3) = paren e1 (ppE e1) ++ "(" ++ ppE e2 ++ ", " ++ ppE e3 ++ ")" where
  paren (ECond _ _ _) body = "(" ++ body ++ ")"
  paren _             body = body

ppIR :: [Cmd] -> String
ppIR cmds = unlines $ concatMap g cmds where
  g (Cmd hint (Cond ir1 ir2)) = 
    let spc = replicate 6 ' '
        ls1 = concatMap g ir1
        ls2 = concatMap g ir2
    in map (spc ++) (f "if _ { " ls1) ++
       map (spc ++) (f "else { " ls2)
  g (Cmd hint ins) = [showHint hint ++ showIns ins] where
    showIns (PushA at) = "push " ++ showAtom at
    showIns (Call2 Nothing) = "call2"
    showIns (Call2 (Just name)) = "call2 " ++ name
  f :: String -> [String] -> [String]
  f heading [l]    = [heading ++ l ++ " }"]
  f heading (l:ls) =
    let end = last ls in
    let mid = init ls in
    let spc = replicate 7 ' ' in
    [heading ++ l] ++ map (spc ++) mid ++ [spc ++ end ++ " }"]

showHint NoHint  = "      "
showHint HintRDI = "(rdi) "
showHint HintRSI = "(rsi) "
showHint HintRAX = "(rax) "

showAtom (AN z) = show z
showAtom (AV x) = x


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




{-

example e
  b ? f(1, 2) : 11

example stack IR
     push b
     if _ { (rdi) push 1
            (rsi) push 2
            (rax) call2 f }
     else {       push 11 }

example asm
  mov r15 [b]
  cmp r15 0
  jz .l1
  mov r15 1
  mov r14 2
  mov rdi r15
  mov rsi r14
  call f
  jmp .l2
  .l1
  mov rax 11
  .l2

-}





parseE :: String -> E
parseE str = case runParser (space >> anyExprParser) "unknown" str of
  Left eb -> error (show eb)
  Right e -> e

anyExprParser :: Parser E
anyExprParser = condParser

primaryParser :: Parser E
primaryParser = do
  h <- varParser <|> numParser <|> parensParser
  suffixes <- many primaryEtc
  return (foldl (.) id suffixes h)

primaryEtc :: Parser (E -> E)
primaryEtc = do
  char '(' >> space
  x <- anyExprParser
  char ',' >> space
  y <- anyExprParser
  char ')' >> space
  return (\e -> ECall e x y)

condParser :: Parser E
condParser = do
  e1 <- primaryParser
  mquestion <- optional (char '?')
  space
  case mquestion of
    Nothing -> return e1
    Just _  -> do
      e2 <- anyExprParser
      char ':' >> space
      e3 <- anyExprParser
      return (ECond e1 e2 e3)

numParser :: Parser E
numParser = do
  mneg <- optional (char '-')
  cs <- some digitChar
  space
  let sign = case mneg of Nothing -> id; Just _ -> negate
  return (ENum (sign (read cs)))

varParser :: Parser E
varParser = do
  c  <- letterChar
  cs <- many alphaNumChar
  space
  return (EVar (c:cs))

parensParser :: Parser E
parensParser = do
  char '(' >> space
  e <- anyExprParser
  char ')' >> space
  return e
