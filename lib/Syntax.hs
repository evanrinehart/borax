module Syntax where

import Control.Monad
import Control.Monad.Writer
import Data.List

import Expr

data Boron = Boron [Definition] deriving (Show)

type Name = String
--data Constant = ConstNumber Int | ConstChar [Char] | ConstString String deriving (Show)
data IVal = IVConst Constant | IVName Name deriving (Show)
data Definition =
  DefV1 Int Name (Maybe IVal) |
  DefVN Int Name (Maybe Int) [IVal] |
  DefF FunctionDef
    deriving (Show)

data FunctionDef = FunctionDef Int Name [Name] Statement
  deriving (Show)

data Statement =
  AutoStatement Int [(Name, Maybe Int)] Statement |
  ExtrnStatement Int [Name] Statement |
  LabelStatement Int Name Statement |
  CaseStatement Int Constant Statement |
  CompoundStatement Int [Statement] |
  ConditionalStatement Int Expr Statement (Maybe Statement) |
  WhileStatement Int Expr Statement |
  SwitchStatement Int Expr Statement |
  GotoStatement Int Expr |
  ReturnStatement Int (Maybe Expr) |
  RValueStatement Int Expr |
  BreakStatement Int |
  NullStatement Int
    deriving (Show)


{-
data OldExpr =
  -- RValue
  ParenExpr OldExpr |
  ConstExpr Constant |
  AssignExpr (Maybe BinaryOp) OldExpr OldExpr |
  PreIncDec IncDec OldExpr |
  PostIncDec IncDec OldExpr |
  AmpersandExpr OldExpr |
  UnaryExpr UnaryOp OldExpr |
  BinaryExpr BinaryOp OldExpr OldExpr |
  TernaryExpr OldExpr OldExpr OldExpr |
  FunctionExpr OldExpr [OldExpr] |
  -- LValue
  NameExpr Name |
  StarExpr OldExpr |
  VectorExpr OldExpr OldExpr 
    deriving (Show)
-}

{-
data IncDec = PlusPlus | MinusMinus deriving (Show)
data UnaryOp = Negative | LogicNot | BitComplement deriving (Show)
data BinaryOp =
  BitOr |
  BitAnd |
  Equals |
  NotEquals |
  LessThan |
  LessThanEquals | 
  GreaterThan |
  GreaterThanEquals | 
  ShiftL |
  ShiftR |
  Plus |
  Minus |
  Modulo |
  Times |
  Division
    deriving (Show)
-}


{-
showExpr :: Expr -> String
showExpr = execWriter . go where
  go :: Expr -> Writer String ()
  go (ParenExpr ex) = do
    tell "("
    go ex
    tell ")"
  go (ConstExpr k) = do
    tell (showConstant k)
  go (AssignExpr Nothing ex1 ex2) = do
    go ex1
    tell " = "
    go ex2
  go (AssignExpr (Just op) ex1 ex2) = do
    go ex1
    tell " ="
    tell (showBinaryOp op)
    tell " "
    go ex2
  go (NameExpr name) = do
    tell name
  go (AmpersandExpr ex) = do
    tell "&"
    go ex
  go (StarExpr ex) = do
    tell "*"
    go ex
  go (UnaryExpr unop ex) = do
    tell (case unop of Negative -> "-"; LogicNot -> "!"; BitComplement -> "~")
    go ex
  go (BinaryExpr op ex1 ex2) = do
    go ex1
    tell (showBinaryOp op)
    go ex2
  go (PreIncDec incdec ex) = do
    tell (case incdec of PlusPlus -> "++"; MinusMinus -> "--")
    go ex
  go (PostIncDec incdec ex) = do
    go ex
    tell (case incdec of PlusPlus -> "++"; MinusMinus -> "--")
  go (TernaryExpr ex1 ex2 ex3) = do
    go ex1
    tell "?"
    go ex2
    tell ":"
    go ex3
  go (FunctionExpr exfun exargs) = do
    go exfun
    tell "("
    sequence (intersperse (tell ",") (map go exargs))
    tell ")"
  go (VectorExpr ex1 ex2) = do
    go ex1
    tell "["
    go ex2
    tell "]"
-}
