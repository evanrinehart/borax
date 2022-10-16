{-# LANGUAGE BangPatterns #-}
module Baby where


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
import Text.Megaparsec.Char hiding (newline)

import qualified Expr
import qualified Asm
import Asm (R(..), showR, CodeLine(..), showCodeLines)
import Doc

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
  Stab2R Loc Operand String R |
  Stab2K Loc Operand String K |
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

ir dst (EBin BDiv e1 (EK k)) = irdivk BDiv dst e1 (OK k)
ir dst (EBin BMod e1 (EK k)) = irdivk BMod dst e1 (OK k)
ir dst (EBin BDiv e1 (EVar x)) = irdivk BDiv dst e1 (OL (LVar x))
ir dst (EBin BMod e1 (EVar x)) = irdivk BMod dst e1 (OL (LVar x))
ir dst (EBin BDiv e1 e2) = irdiv BDiv dst e1 e2
ir dst (EBin BMod e1 e2) = irdiv BMod dst e1 e2
ir dst (EBin binop e1 (EK k)) = do
  as <- ir dst e1
  return $ as ++ [Stab2 (LReg dst) dst (binopKeyword binop) (OK k)]
ir dst (EBin BAdd (EK k) e2) = ir dst (EBin BAdd e2 (EK k))
ir dst (EBin BMul (EK k) e2) = ir dst (EBin BMul e2 (EK k))
ir dst (EBin BAnd (EK k) e2) = ir dst (EBin BAnd e2 (EK k))
ir dst (EBin BOr  (EK k) e2) = ir dst (EBin BOr  e2 (EK k))
ir dst (EBin BXor (EK k) e2) = ir dst (EBin BXor e2 (EK k))
ir dst (EBin BNeq (EK k) e2) = ir dst (EBin BNeq e2 (EK k))
ir dst (EBin BEq  (EK k) e2) = ir dst (EBin BEq  e2 (EK k))
ir dst (EBin BLt  (EK k) e2) = ir dst (EBin BGt  e2 (EK k))
ir dst (EBin BGt  (EK k) e2) = ir dst (EBin BLt  e2 (EK k))
ir dst (EBin BLte (EK k) e2) = ir dst (EBin BGte e2 (EK k))
ir dst (EBin BGte (EK k) e2) = ir dst (EBin BLte e2 (EK k))
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

ir dst (EAssignOp BDiv (EVar x) e2) = ireqdivx BDiv dst x e2
ir dst (EAssignOp BMod (EVar x) e2) = ireqdivx BMod dst x e2
ir dst (EAssignOp BDiv e1 (EK k)) = ireqdivk BDiv dst e1 (OK k)
ir dst (EAssignOp BMod e1 (EK k)) = ireqdivk BMod dst e1 (OK k)
ir dst (EAssignOp BDiv e1 e2) = ireqdiv BDiv dst e1 e2
ir dst (EAssignOp BMod e1 e2) = ireqdiv BMod dst e1 e2
ir dst (EAssignOp binop (EVar x) (EK k)) = do
  return [Stab2K (LVar x) (OL (LVar x)) (binopKeyword binop) k]
ir dst (EAssignOp binop (EVar x) e2) = do
    bs <- ir dst e2
    return $ bs ++ [Stab2R (LVar x) (OL (LVar x)) (binopKeyword binop) dst]
ir dst (EAssignOp binop e1 e2) = do
  (t, bs) <- uir e2
  as <- irlv dst e1
  -- r11 <- t
  -- *d <- *d op r11
  --  d <- t
  forget 1
  return $
    bs ++
    as ++
    [Stab0 (LReg R11) (OL t)] ++
    [Stab2R (LMem (MF2 dst)) (OStar dst) (binopKeyword binop) R11] ++
    [Stab0 (LReg dst) (OL (LReg R11))]
  

ir dst (EPFix "--_" e1) = irpre  dst "--" e1
ir dst (EPFix "_--" e1) = irpost dst "--" e1
ir dst (EPFix "++_" e1) = irpre  dst "++" e1
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

getSimpleOperand :: E -> Maybe Operand
getSimpleOperand (EK k)   = Just (OK k)
getSimpleOperand (EVar x) = Just (OL (LVar x))
getSimpleOperand _        = Nothing

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
  return $ bs ++ as ++ [Stab2 (LReg dst) RAX (binopKeyword op) (OL loc)]

irdivk :: Binop -> R -> E -> Operand -> IRGen [IRIns]
irdivk op dst e1 denom = do 
  as <- ir RAX e1
  return $ as ++ [Stab2 (LReg dst) RAX (binopKeyword op) denom]

ireqdiv :: Binop -> R -> E -> E -> IRGen [IRIns]
ireqdiv binop dst e1 e2 = do
  (t, bs) <- uir e2
  as <- irlv dst e1
  forget 1
  return $
    bs ++
    as ++
    [Stab2 (LMem (MF2 dst)) RAX (binopKeyword binop) (OL t)] ++
    [Stab0 (LReg dst) (OL t)]

ireqdivk :: Binop -> R -> E -> Operand -> IRGen [IRIns]
ireqdivk binop dst e1 denom = do
  as <- irlv dst e1
  return $
    as ++
    [Stab0 (LReg RAX) (OL (LReg dst)) ] ++
    [Stab2 (LMem (MF2 dst)) RAX (binopKeyword binop) denom] ++
    [Stab0 (LReg dst) denom]

ireqdivx :: Binop -> R -> String -> E -> IRGen [IRIns]
ireqdivx binop dst x e2 = case getSimpleOperand e2 of
  Just denom -> return $
    [Stab0 (LReg RAX) (OL (LVar x))] ++
    [Stab2 (LVar x) RAX (binopKeyword binop) denom] ++
    [Stab0 (LReg dst) denom]
  Nothing -> do
    (t, as) <- uir e2
    forget 1
    return $
      as ++
      [Stab0 (LReg RAX) (OL (LVar x))] ++
      [Stab2 (LVar x) RAX (binopKeyword binop) (OL t)] ++
      [Stab0 (LReg dst) (OL t)]

irpre dst plusOrMinus (EVar x) = do
  return $
    [Stab1 (LVar x) (pfixKeyword plusOrMinus) (OL (LVar x))
    ,Stab0 (LReg dst) (OL (LVar x))]
irpre dst plusOrMinus e1 = do
  as <- irlv dst e1
  return $ as ++
    [Stab1 (LMem (MF2 dst)) (pfixKeyword plusOrMinus) (OStar dst)
    ,Stab0 (LReg dst) (OStar dst)]
irpost dst plusOrMinus (EVar x) = do
  return $
    [Stab0 (LReg dst) (OL (LVar x))
    ,Stab1 (LVar x) (pfixKeyword plusOrMinus) (OL (LVar x))]
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



baby :: String -> IO ()
baby str = do
  let ircode = evalState (ir RAX (parseE str)) allTheTemps
  let asm = toAsm ircode
  putStrLn (flatten (showCodeLines asm))

-- | Generate asm from ir

toAsm :: [IRIns] -> [CodeLine]
toAsm ins = f =<< ins where
  f :: IRIns -> [CodeLine]
  f (StabCmp mdst r opd)                = []
  f (Stab0 dst opd)                     = []
  f (Stab1 dst code opd)                = []
  f (Stab2 dst r code opd)              = []
  f (Stab2R dst opd code r)             = []
  f (Stab2K dst opd code k)             = []
  f (StabCall dst n fun args)           = []
  f (StabVarAddr dst x)                 = []
  f (IRCond comparison body1 body2)     = []

{-
-- location becomes
-- r -> r
-- extra i -> extra base - 8i
-- var x -> depends, global, local, etc
-- memforms -> formulaic translation
toOperand1 :: Loc -> Asm.Operand
toOperand1 = f where
  f (LReg r)   = Asm.OG r
  f (LExtra i) = error "where is extra base"
  f (LVar x)   = error "depends"
  f (LMem _)   = error "translate memform"
  
data Loc = LReg R | LExtra Int | LVar String | LMem MemForm

toOperand2 :: Operand -> Asm.Operand
toOperand2
-}

-- | Pretty printers

ppE :: E -> Doc
ppE (EK k) = showK k
ppE (EVar x) = text x
ppE (ECond e1 e2 e3) = ppE e1 <> text " ? " <> ppE e2 <> text " : " <> ppE e3
ppE (ECall e es) = text "Call(" <> ppE e <> text ")[" <> showCommas (map ppE es) <> text "]"


ppIR :: [IRIns] -> Doc
ppIR ins = joinDocs newline (g =<< ins) where
  g :: IRIns -> [Doc]
  g (IRCond c body1 body2) = 
    let spc = text (replicate 6 ' ')
        ls1 = g =<< body1 :: [Doc]
        ls2 = g =<< body2 :: [Doc]
        h1 = "if " ++ c ++ " { "
        h2 = "else " ++ replicate (length c - 2) ' ' ++ " { "
    in f h1 ls1 ++ f h2 ls2
  g (Stab0 d opd)      = [joinDocs arrow [showLoc d, showOpd opd]]
  g (Stab1 d code opd) = [joinDocs arrow [showLoc d, text code <> space <> showOpd opd]]
  g (Stab2 d r code opd) =
    [showLoc d <> arrow <> showR r <> space <> text code <> space <> showOpd opd]
  g (Stab2R d opd code r) =
    [showLoc d <> arrow <> showOpd opd <> space <> text code <> space <> showR r]
  g (Stab2K d opd code k) =
    [showLoc d <> arrow <> showOpd opd <> space <> text code <> space <> showK k]
  g (StabCall d n fun configs) =
    [showLoc d <> arrow <> text "call" <> showN n <> space <> showOpd fun <> showConfigs configs]
  g (StabVarAddr d name) = [showLoc d <> arrow <> amp <> text name]
  g (StabCmp md r opd) =
    [maybe under showLoc md <> arrow <> text "cmp" <> space <> showR r <> space <> showOpd opd]
  f :: String -> [Doc] -> [Doc]
  f heading [l]    = [text heading <> l <> text " }"]
  f heading (l:ls) =
    let end = last ls in
    let mid = init ls in
    let spc = text (replicate (length heading) ' ') in
    [text heading <> l] ++ map (spc <>) mid ++ [spc <> end <> text " }"]
  arrow = text " ← "
  space = text " "
  under = text "_"
  amp = text "&"


showK :: K -> Doc
showK (KN z)     = text (show z)
showK (KC c)     = text (Expr.showWideChar [c])
showK (KCS cs)   = text (Expr.showWideChar cs)
showK (KStr str) = text (show str)

showOpd :: Operand -> Doc
showOpd (OL l) = showLoc l
showOpd (OK k) = showK k
showOpd (OStar r) = text "*" <> showR r

showLoc :: Loc -> Doc
showLoc (LReg r) = showR r
showLoc (LVar x) = text x
showLoc (LExtra i) = text "extra" <> showN i
showLoc (LMem (MF2 r)) = text "*" <> showR r
showLoc (LMem (MF1 k)) = text "*" <> showK k
showLoc (LMem (MF3 r k)) = text "*(" <> showR r <> text " + " <> showK k <> text ")"

showConfigs :: [CallConfig] -> Doc
showConfigs [] = nil
showConfigs cs = text "(" <> showCommas (map showConfig cs) <> text ")" 

showConfig :: CallConfig -> Doc
showConfig (C1 r l) = showR r <> text " ← " <> showLoc l
showConfig (C2 l) = text "push(" <> showLoc l <> text ")"







-- | Argument expression resequencing

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


-- | Expression parser

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
    wrapbin BNeq <$ (string "!=" >> remspace) <|>
    wrapbin BEq  <$ try (string "==" >> notFollowedBy (char '=') >> remspace)

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

-- | Operator parsers

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
  wrapbin BAdd <$ (char '+' >> remspace) <|>
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
  BNeq <$ string "!=" <|>
  snowflakeEq <|>
  BShl <$ string "<<" <|>
  BShr <$ string ">>" <|>
  BLte <$ string "<=" <|>
  BGte <$ string ">=" <|>
  BLt  <$ char '<' <|>
  BGt  <$ char '>' <|>
  BOr  <$ char '|' <|>
  BAnd <$ char '&' <|>
  BAdd <$ char '+' <|>
  BSub <$ char '-' <|>
  BMod <$ char '%' <|>
  BMul <$ char '*' <|>
  BDiv <$ char '/'

snowflakeEq :: Parser Binop
snowflakeEq = do
  string "=="
  notFollowedBy (char '=')
  return BEq

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

