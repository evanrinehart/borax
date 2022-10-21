module Analyzer where

import Data.Map as M (Map, insert, empty)
--import Data.Foldable 

import Syntax


{-
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
-}

data NameInfo =
  ExternalStorage Int |
  ExternalVector Int |
  ExternalFunc Int |
  ExternalUnknown |
  LocalParameter String |
  LocalStorage Int |
  LocalLabel Int Int
    deriving Show

data Problem =
  VariableNotInScope Int String |
  MultipleLabels Int Int String |
  BadFunctionCall Int String Int
    deriving Show

topLevelNames :: FileAST -> Map Name NameInfo
topLevelNames (FileAST defs) = foldl f M.empty defs where
  f m (DefV1 _ name _) = M.insert name (ExternalStorage 1) m
  f m (DefVN _ name Nothing ivals) = M.insert name (ExternalVector (length ivals)) m
  f m (DefVN _ name (Just s) ivals) = M.insert name (ExternalVector (max (s+1) (length ivals))) m
  f m (DefF (FunctionDef _ name params _)) = M.insert name (ExternalFunc (length params)) m

namesInFunction :: Map Name NameInfo -> FunctionDef -> Map Name NameInfo
namesInFunction global (FunctionDef _ name params body) = foldl f global params where
  f m x = M.insert x (LocalParameter x) m

