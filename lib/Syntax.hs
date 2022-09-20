module Syntax where

data Program = Program [Definition] deriving (Show)

type Name = String
data Constant = ConstNumber Integer | ConstChar [Char] | ConstString String deriving (Show)
data IVal = IVConst Constant | IVName Name deriving (Show)
data Definition =
  AtomicDef Int Name [IVal] |
  VectorDef Int Name (Maybe Constant) [IVal] |
  FunctionDef Int Name [Name] Statement
    deriving (Show)

data Statement =
  AutoStatement Int [(Name, Maybe Constant)] Statement |
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

data Expr =
  -- RValue
  ParenExpr Expr |
  ConstExpr Constant |
  AssignExpr (Maybe BinaryOp) Expr Expr |
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
