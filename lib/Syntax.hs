module Syntax where

data Program = Program [Definition] deriving (Show)

type Name = String
data Constant = ConstNumber Integer | ConstChar [Char] | ConstString String deriving (Show)
data IVal = IVConst Constant | IVName Name deriving (Show)
data Definition =
  AtomicDef Name [IVal] |
  VectorDef Name (Maybe Constant) [IVal] |
  FunctionDef Name [Name] Statement
    deriving (Show)

data Statement =
  AutoStatement [(Name, Maybe Constant)] Statement |
  ExtrnStatement [Name] Statement |
  LabelStatement Name Statement |
  CaseStatement Constant Statement |
  CompoundStatement [Statement] |
  ConditionalStatement Expr Statement (Maybe Statement) |
  WhileStatement Expr Statement |
  SwitchStatement Expr Statement |
  GotoStatement Expr |
  ReturnStatement (Maybe Expr) |
  RValueStatement Expr |
  NullStatement
    deriving (Show)

data Expr =
  -- RValue
  ParenExpr Expr |
  ConstExpr Constant |
  AssignExpr Assignment Expr Expr |
  PreIncDec IncDec Expr |
  PostIncDec IncDec Expr |
  AmpersandExpr Expr |
  UnaryExpr UnaryOp Expr |
  BinaryExpr BinaryOp Expr Expr |
  TernaryExpr Expr Expr Expr |
  FunctionExpr Expr [Expr] |
  -- LValue
  NameExpr Name |
  StarExpr Expr |
  VectorExpr Expr Expr 
    deriving (Show)

data IncDec = PlusPlus | MinusMinus deriving (Show)
data Assignment = Assignment (Maybe BinaryOp) deriving (Show)
data UnaryOp = Negative | LogicNot deriving (Show)
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
