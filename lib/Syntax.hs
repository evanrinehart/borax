module Syntax where

import Control.Monad
import Control.Monad.Writer
import Data.List

import Expr

data Boron = Boron [Definition] deriving (Show)

type Name = String

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



