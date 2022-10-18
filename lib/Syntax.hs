module Syntax where

import Prelude hiding (showList)
import Control.Monad
import Control.Monad.Writer
import Data.List

import Doc
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


showIVal (IVConst k)   = showK k
showIVal (IVName name) = text name

showIVals ivals = showList (map showIVal ivals)

showList :: [Doc] -> Doc
showList []     = text "[]"
showList [x]    = text "[" <> x <> text "]"
showList (x:xs) = text "[" <> x <> suffixes xs <> text "]" where
  suffixes []     = nil
  suffixes (x:xs) = newline <> text "," <> x <> suffixes xs

showSmallList :: [Doc] -> Doc
showSmallList ds     = text "[" <> joinDocs (text ",") ds <> text "]"

indent :: Doc -> Doc
indent d = tab <> f d where
  f (d1 :<> d2) = f d1 :<> f d2
  f Newline     = Newline <> tab
  f other       = other

showParens :: Doc -> Doc
showParens d = text "(" <> d <> text ")"

showStatement :: Statement -> Doc
showStatement = f where
  space = text " "
  f (AutoStatement l decls next) =
    let body = newline <> indent (showParens (f next))
    in text "AutoStatement" <> space <> showN l <> space <> text (show decls) <> body
  f (ExtrnStatement l names next) =
    let body = newline <> indent (showParens (f next))
    in text "ExtrnStatement" <> space <> showN l <> space <> text (show names) <> body
  f (LabelStatement l name next) =
    let body = newline <> indent (showParens (f next))
    in text "LabelStatement" <> space <> showN l <> space <> text (show name) <> body
  f (CaseStatement l k next) =
    let body = newline <> indent (showParens (f next))
    in text "CaseStatement" <> space <> showN l <> space <> showK k <> body
  f (CompoundStatement l stmts) =
    let body = indent (showList (map f stmts))
    in text "CompoundStatement" <> space <> showN l <> newline <> body
  f (ConditionalStatement l e stmt1 mstmt) =
    let body1 = newline <> indent (showParens (f stmt1))
        body2 = case mstmt of Just stmt -> newline <> indent (showParens (f stmt)); Nothing -> nil
    in joinDocs space [text "ConditionalStatement", showN l, showParens (showE e)] <> body1 <> body2
  f (WhileStatement l e next) =
    let body = newline <> indent (showParens (f next))
    in text "WhileStatement" <> space <> showN l <> space <> showParens (showE e) <> space <> body
  f (SwitchStatement l e next) =
    let body = newline <> indent (showParens (f next))
    in text "SwitchStatement" <> space <> showN l <> space <> showParens (showE e) <> space <> body
  f (GotoStatement l e) =
    text "GotoStatement" <> space <> showN l <> space <> showParens (showE e)
  f (ReturnStatement l mexpr) = case mexpr of
    Just e  -> text "ReturnStatement" <> space <> showN l <> space <> showParens (showE e)
    Nothing -> text "ReturnStatement" <> space <> showN l
  f (RValueStatement l e) =
    text "RValueStatement" <> space <> showN l <> space <> showParens (showE e)
  f (BreakStatement l) = text "BreakStatement" <> space <> showN l
  f (NullStatement l) = text "NullStatement" <> space <> showN l

showFunDef :: FunctionDef -> Doc
showFunDef (FunctionDef l name params body) = a <> b where
  a = joinDocs (text " ")
        [text "FunctionDef",showN l, text (show name), showSmallList (map (text.show) params)]
  b = newline <> indent (showParens (showStatement body))


showDef (DefV1 l name mival)     = case mival of
  Nothing -> joinDocs (text " ") [text "Global1", showN l, text (show name)]
  Just v  -> joinDocs (text " ") [text "Global1", showN l, text (show name), showIVal v]
showDef (DefVN l name msize ivals) = case msize of
  Nothing -> joinDocs (text " ") [text "GlobalV", showN l, text "_", text (show name), showIVals ivals]
  Just s  -> joinDocs (text " ") [text "GlobalV", showN l, showN s, text (show name), showIVals ivals]
showDef (DefF fundef)              = showFunDef fundef

showFileAST :: FileAST -> Doc
showFileAST (FileAST defs) = text "FileAST" <> newline <> indent (showList (map showDef defs))
