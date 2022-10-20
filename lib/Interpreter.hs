module Interpreter where

import Control.Monad.State
import Data.Map as M

import Syntax
import Expr

data ExStatement =
  ExConditional E ExStatement ExStatement |
  ExSwitch E ExStatement ExStatement |
  ExEval E ExStatement |
  ExGoto E |
  ExReturn |
  ExReturnE E |
  ExNull
    deriving Show

data EntryMapperData = MkEntryMapperData
  { emdGenerator :: Int
  , emdMap       :: Map String ExStatement }

emdBlank = MkEntryMapperData 0 M.empty

type EntryMapper = State EntryMapperData

tellEntry :: Name -> ExStatement -> EntryMapper ()
tellEntry name s = modify (\(MkEntryMapperData g m) -> MkEntryMapperData g (M.insert name s m))

generate :: EntryMapper Int
generate = state (\(MkEntryMapperData g m) -> (g, MkEntryMapperData (g+1) m))

genSym :: EntryMapper String
genSym = do
  n <- generate
  return ("$" ++ show n)

entryMap :: Statement -> Map Name ExStatement
entryMap s = emdMap (execState (genSym >>= \br -> f s ExNull br >>= tellEntry "") emdBlank) where
  f (AutoStatement l vars next)  rest br = f next rest br
  f (ExtrnStatement l vars next) rest br = f next rest br
  f (LabelStatement l name next) rest br = do
    code <- f next rest br
    tellEntry name code
    pure code
  f (CaseStatement l k next) rest br = f next rest br
  f (ConditionalStatement l e s1 Nothing) rest br = do
    code <- f s1 rest br
    pure (ExConditional e code rest)
  f (ConditionalStatement l e s1 (Just s2)) rest br = do
    code1 <- f s1 rest br
    code2 <- f s2 rest br
    pure (ExConditional e code1 code2)
  f (WhileStatement l e body) rest br = do
    -- while(e) body ==> loop: if(e){ body goto loop; }
    loop' <- genSym
    br'   <- genSym
    innerCode <- f (CompoundStatement 0 ([body, GotoStatement 0 (EVar loop')])) rest br'
    let code = ExConditional e innerCode rest
    tellEntry loop' code
    tellEntry br' rest
    pure code
  f (SwitchStatement l e body) rest br = do
    br' <- genSym
    code <- f body rest br'
    tellEntry br' rest
    pure (ExSwitch e code rest)
  f (NullStatement l) rest br = pure rest
  f (RValueStatement l e) rest br = pure (ExEval e rest)
  f (GotoStatement l e) _ br = pure (ExGoto e)
  f (ReturnStatement l me) _ br = case me of
      Just e  -> pure (ExReturnE e)
      Nothing -> pure ExReturn
  f (BreakStatement l) rest br = pure $ ExGoto (EVar br)
  f (CompoundStatement l [])     rest br = pure rest
  f (CompoundStatement l (t:tt)) rest br = do
    rest' <- f (CompoundStatement l tt) rest br
    f t rest' br




