{-# LANGUAGE DeriveFoldable, DeriveFunctor, RankNTypes #-}
module Expr where

import Control.Monad.State
import Control.Monad.Writer
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Fix
import Data.Functor.Classes
import Data.Semigroup
import Data.List (intersperse)
import Data.List.NonEmpty ( NonEmpty( (:|) ) )

-- tool to break up an expression into numbered components

data Constant =
  ConstI Int |
  ConstS String |
  ConstC [Char]
    deriving Show

data ExprOf a =
  -- RValue
  ExConst Constant |
  ExAmp a |
  ExUnary UnaryOp a |
  ExBinary BinaryOp a a |
  ExAssign a a |
  ExAssignOp BinaryOp a a |
  ExPreInc a |
  ExPreDec a |
  ExPostInc a |
  ExPostDec a |
  ExFunc a [a] |
  ExTernary a a a |
  -- LValue
  ExName String |
  ExStar a |
  ExVector a a
    deriving (Functor, Foldable, Show)


instance Show1 ExprOf where
  liftShowsPrec sp sl lvl ex = f ex where
    f (ExConst k) = showsUnaryWith showsPrec "ExConst" lvl k
    f (ExAmp e)   = showsUnaryWith sp "ExAmp" lvl e
    f (ExUnary unop e) = showsBinaryWith showsPrec sp "ExUnary" lvl unop e
    f (ExBinary binop e1 e2) = showParen (lvl > 10) $
        showString "ExBinary" . showChar ' ' .
        showsPrec 11 binop . showChar ' ' .
        sp 11 e1 . showChar ' ' .
        sp 11 e2
    f (ExAssign e1 e2) = showsBinaryWith sp sp "ExAssign" lvl e1 e2
    f (ExAssignOp binop e1 e2) = showParen (lvl > 10) $
        showString "ExAssignOp" . showChar ' ' .
        showsPrec 11 binop . showChar ' ' .
        sp 11 e1 . showChar ' ' .
        sp 11 e2
    f (ExPreInc e)  = showsUnaryWith sp "ExPreInc" lvl e
    f (ExPreDec e)  = showsUnaryWith sp "ExPreDec" lvl e
    f (ExPostInc e) = showsUnaryWith sp "ExPostInc" lvl e
    f (ExPostDec e) = showsUnaryWith sp "ExPostDec" lvl e
    f (ExFunc e1 es) = showParen (lvl > 10) $
        showString "ExFunc" . showChar ' ' .
        sp 11 e1 . showChar ' ' .
        sl es
    f (ExTernary e1 e2 e3) = showParen (lvl > 10) $
        showString "ExTernary" . showChar ' ' .
        sp 11 e1 . showChar ' ' .
        sp 11 e2 . showChar ' ' .
        sp 11 e3 . showChar ' '
  -- LValue
    f (ExName str) = showsUnaryWith (\_ -> showString . show) "ExName" lvl str
    f (ExStar e)   = showsUnaryWith sp "ExStar" lvl e
    f (ExVector e1 e2) = showsBinaryWith sp sp "ExVector" lvl e1 e2

type Expr = Fix ExprOf

data UnaryOp =
  Negative |
  LogicNot |
  BitComplement deriving (Show)

data BinaryOp =
  Plus | Minus | Modulo | Times | Division |
  LessThan | LessThanEquals | GreaterThan | GreaterThanEquals | Equals | NotEquals |
  BitXor | BitOr | BitAnd | ShiftL | ShiftR | LogicAnd | LogicOr
    deriving (Show)

enumerate :: Expr -> IntMap (ExprOf Int)
enumerate (Fix ex) = IM.fromList (snd $ execState (go ex) (0,[])) where
  go :: ExprOf Expr -> En (ExprOf Int) Int
  -- RValue
  go (ExConst k)      = dance0 (ExConst k)
  go (ExAssign e1 e2) = dance2 ExAssign e1 e2
  go (ExAssignOp op e1 e2) = dance2 (ExAssignOp op) e1 e2
  go (ExPreInc  e)    = dance1 ExPreInc e
  go (ExPreDec  e)    = dance1 ExPreDec e
  go (ExPostInc e)    = dance1 ExPostInc e
  go (ExPostDec e)    = dance1 ExPostDec e
  go (ExAmp e)        = dance1 ExAmp e
  go (ExUnary unop e) = dance1 (ExUnary unop) e
  go (ExBinary binop e1 e2) = dance2 (ExBinary binop) e1 e2
  go (ExTernary (Fix e1) (Fix e2) (Fix e3)) = do
    i  <- gen
    j1 <- go e1
    j2 <- go e2
    j3 <- go e3
    out i (ExTernary j1 j2 j3)
    return i
  go (ExFunc (Fix e) fes) = do
    i  <- gen
    j  <- go e
    js <- mapM (go . unFix) fes
    out i (ExFunc j js)
    return i
  -- LValue
  go (ExName name)    = dance0 (ExName name)
  go (ExStar e)       = dance1 ExStar e
  go (ExVector e1 e2) = dance2 ExVector e1 e2
  -- helpers
  dance0 x = do
    i <- gen
    out i x
    return i
  dance1 wrap (Fix e) = do
    i <- gen
    j <- go e
    out i (wrap j)
    return i
  dance2 wrap (Fix e1) (Fix e2) = do
    i  <- gen
    j1 <- go e1
    j2 <- go e2
    out i (wrap j1 j2)
    return i

type En t a = State (Int,[(Int,t)]) a

gen :: En t Int
gen = state $ \(g,xs) -> (g, (g+1,xs))

out :: Int -> t -> En t ()
out i x = modify $ \(g,xs) -> (g, ((i,x):xs))



foldMapExpr :: Semigroup m => (forall a . ExprOf a -> m) -> Expr -> m
foldMapExpr h (Fix ex) = f ex where
  f e@(ExConst _)                       = h e
  f e@(ExAssign (Fix e1) (Fix e2))      = h e <> f e1 <> f e2
  f e@(ExAssignOp op (Fix e1) (Fix e2)) = h e <> f e1 <> f e2
  f e@(ExPreInc  (Fix e1)) = h e <> f e1
  f e@(ExPreDec  (Fix e1)) = h e <> f e1
  f e@(ExPostInc (Fix e1)) = h e <> f e1
  f e@(ExPostDec (Fix e1)) = h e <> f e1
  f e@(ExAmp (Fix e1))     = h e <> f e1
  f e@(ExUnary _ (Fix e1)) = h e <> f e1
  f e@(ExBinary binop (Fix e1) (Fix e2))     = h e <> f e1 <> f e2
  f e@(ExTernary (Fix e1) (Fix e2) (Fix e3)) = h e <> f e1 <> f e2 <> f e3
  f e@(ExFunc (Fix e1) [])                   = h e <> f e1
  f e@(ExFunc (Fix e1) (Fix x:xs)) = h e <> f e1 <> sconcat (f x :| map (f . unFix) xs)
  f e@(ExName _)                   = h e
  f e@(ExStar (Fix e1))            = h e <> f e1
  f e@(ExVector (Fix e1) (Fix e2)) = h e <> f e1 <> f e2



showConstant :: Constant -> String
showConstant (ConstI n) = show n
showConstant (ConstC cs) = showWideChar cs
showConstant (ConstS str) = show str

encodeC :: Char -> String
encodeC '\'' = "*'"
encodeC '"' = "*\""
encodeC '\EOT' = "*e"
encodeC '\t' = "*t"
encodeC '\n' = "*n"
encodeC '\0' = "*0"
encodeC '*' = "**"
encodeC c = [c]

showWideChar :: String -> String
showWideChar cs = "'" ++ concatMap encodeC cs ++ "'"

showBinaryOp :: BinaryOp -> String
showBinaryOp o = case o of
  BitOr -> "|"
  BitAnd -> "&"
  Equals -> "=="
  NotEquals -> "!="
  LessThan -> "<"
  LessThanEquals -> "<="
  GreaterThan -> ">"
  GreaterThanEquals -> ">="
  ShiftL -> "<<"
  ShiftR -> ">>"
  Plus -> "+"
  Minus -> "-"
  Modulo -> "%"
  Times -> "*"
  Division -> "/"

showExpr :: Expr -> String
showExpr = execWriter . go . unFix where
  go :: ExprOf Expr -> Writer String ()
  go (ExConst k) = do
    tell (showConstant k)
  go (ExAssign (Fix ex1) (Fix ex2)) = do
    go ex1
    tell " = "
    go ex2
  go (ExAssignOp op (Fix ex1) (Fix ex2)) = do
    go ex1
    tell " ="
    tell (showBinaryOp op)
    tell " "
    go ex2
  go (ExName name) = do
    tell name
  go (ExAmp (Fix ex)) = do
    tell "&"
    go ex
  go (ExStar (Fix ex)) = do
    tell "*"
    go ex
  go (ExUnary unop (Fix ex)) = do
    tell (case unop of Negative -> "-"; LogicNot -> "!"; BitComplement -> "~")
    go ex
  go (ExBinary op (Fix ex1) (Fix ex2)) = do
    go ex1
    tell (showBinaryOp op)
    go ex2
  go (ExPreInc (Fix ex)) = tell "++" >> go ex
  go (ExPreDec (Fix ex)) = tell "--" >> go ex
  go (ExPostInc (Fix ex)) = go ex >> tell "++"
  go (ExPostDec (Fix ex)) = go ex >> tell "--"
  go (ExTernary (Fix ex1) (Fix ex2) (Fix ex3)) = do
    go ex1
    tell "?"
    go ex2
    tell ":"
    go ex3
  go (ExFunc (Fix exfun) exargs) = do
    go exfun
    tell "("
    sequence (intersperse (tell ",") (map (go . unFix) exargs))
    tell ")"
  go (ExVector (Fix ex1) (Fix ex2)) = do
    go ex1
    tell "["
    go ex2
    tell "]"
