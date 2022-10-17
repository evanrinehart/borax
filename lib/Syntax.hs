module Syntax where

import Control.Monad
import Control.Monad.Writer
import Data.List

import Expr

data FileAST = FileAST [Definition] deriving (Show)

type Name = String

data IVal = IVConst K | IVName Name deriving (Show)
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
  CaseStatement Int K Statement |
  CompoundStatement Int [Statement] |
  ConditionalStatement Int E Statement (Maybe Statement) |
  WhileStatement Int E Statement |
  SwitchStatement Int E Statement |
  GotoStatement Int E |
  ReturnStatement Int (Maybe E) |
  RValueStatement Int E |
  BreakStatement Int |
  NullStatement Int
    deriving (Show)



