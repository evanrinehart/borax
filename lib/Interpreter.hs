module Interpreter where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import Syntax
import Expr

data ExStatement =
  ExConditional E ExStatement ExStatement |
  ExSwitch E (Map K ExStatement) ExStatement |
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
entryMap s = emdMap (execState (genSym >>= \br -> f s ExNull br >>= \(code,_) -> tellEntry "" code) emdBlank) where
  f (AutoStatement l vars next)  rest br = f next rest br
  f (ExtrnStatement l vars next) rest br = f next rest br
  f (LabelStatement l name next) rest br = do
    (code,table) <- f next rest br
    tellEntry name code
    pure (code,table)
  f (CaseStatement l k next) rest br = do
    (code,table) <- f next rest br
    pure (code, M.insert k code table)
  f (ConditionalStatement l e s1 Nothing) rest br = do
    (code,table) <- f s1 rest br
    pure (ExConditional e code rest, table)
  f (ConditionalStatement l e s1 (Just s2)) rest br = do
    (code1,table1) <- f s1 rest br
    (code2,table2) <- f s2 rest br
    pure (ExConditional e code1 code2, table1 <> table2)
  f (WhileStatement l e body) rest br = do
    -- while(e) body ==> loop: if(e){ body goto loop; }
    loop' <- genSym
    br'   <- genSym
    (innerCode,table) <- f (CompoundStatement 0 [body, GotoStatement 0 (EVar loop')]) rest br'
    let code = ExConditional e innerCode rest
    tellEntry loop' code
    tellEntry br' rest
    pure (code,table)
  f (SwitchStatement l e body) rest br = do
    br' <- genSym
    (_,table) <- f body rest br'
    tellEntry br' rest
    pure (ExSwitch e table rest, M.empty)
  f (NullStatement l) rest br = pure (rest, M.empty)
  f (RValueStatement l e) rest br = pure (ExEval e rest, M.empty)
  f (GotoStatement l e) _ br = pure (ExGoto e, M.empty)
  f (ReturnStatement l me) _ br = case me of
      Just e  -> pure (ExReturnE e, M.empty)
      Nothing -> pure (ExReturn, M.empty)
  f (BreakStatement l) rest br = pure (ExGoto (EVar br), M.empty)
  f (CompoundStatement l [])     rest br = pure (rest, M.empty)
  f (CompoundStatement l (t:tt)) rest br = do
    (rest',table) <- f (CompoundStatement l tt) rest br
    (rest'',table') <- f t rest' br
    pure (rest'', table <> table')




stringsInFile :: FileAST -> Set String
stringsInFile (FileAST defs) = Prelude.foldl f S.empty defs where
  f acc (DefV1 _ _ (Just iv)) = g acc iv
  f acc (DefV1 _ _ _)         = acc
  f acc (DefVN _ _ _ ivs)     = Prelude.foldl g acc ivs
  f acc (DefF (FunctionDef _ _ _ body)) = stringsInStatement acc body
  g acc (IVConst (KStr str)) = S.insert str acc
  g acc _ = acc

stringsInStatement :: Set String -> Statement -> Set String
stringsInStatement = foldlStatement
  (\acc _ _ -> acc)
  (\acc _ _ -> acc)
  (\acc _ _ -> acc)
  (\acc _ k -> case k of KStr str -> S.insert str acc; _ -> acc)
  (\acc _ -> acc)
  (\acc _ e -> stringsInExpr2 acc e)
  (\acc _ e -> stringsInExpr2 acc e)
  (\acc _ e -> stringsInExpr2 acc e)
  (\acc _ e -> stringsInExpr2 acc e)
  (\acc _ me -> case me of Just e -> stringsInExpr2 acc e; _ -> acc)
  (\acc _ e -> stringsInExpr2 acc e)
  (\acc _ -> acc)
  (\acc _ -> acc)

stringsInExpr2 :: Set String -> E -> Set String
stringsInExpr2 = foldlE
  (\acc k -> case k of KStr str -> S.insert str acc; _ -> acc)
  (\acc _ -> acc)
  (\acc -> acc)
  (\acc -> acc)
  (\acc _ -> acc)
  (\acc _ -> acc)
  (\acc -> acc)
  (\acc _ -> acc)
  (\acc _ -> acc)
  (\acc -> acc)
  (\acc -> acc)
  (\acc -> acc)

foldlStatement ::
  (a -> Int -> [(Name, Maybe Int)] -> a) -> --auto
  (a -> Int -> [Name] -> a) -> -- extrn
  (a -> Int -> Name -> a) -> -- label
  (a -> Int -> K -> a) -> -- case
  (a -> Int -> a) -> -- compound
  (a -> Int -> E -> a) ->  -- conditional
  (a -> Int -> E -> a) -> -- while
  (a -> Int -> E -> a) -> -- switch
  (a -> Int -> E -> a) -> -- goto
  (a -> Int -> Maybe E -> a) -> -- return
  (a -> Int -> E -> a) ->  -- rvalue
  (a -> Int -> a) -> -- break
  (a -> Int -> a) -> -- null
  a -> Statement -> a
foldlStatement fauto fextrn flabel fcase fcomp fcond fwhile fswitch fgoto freturn frvalue fbreak fnull = f where
  f acc (AutoStatement l vars next)  = f (fauto acc l vars) next
  f acc (ExtrnStatement l vars next) = f (fextrn acc l vars) next
  f acc (LabelStatement l name next) = f (flabel acc l name) next
  f acc (CaseStatement l k next)     = f (fcase acc l k) next
  f acc (CompoundStatement l stmts)  = Prelude.foldl f (fcomp acc l) stmts
  f acc (ConditionalStatement l e s1 Nothing)   = f (fcond acc l e) s1
  f acc (ConditionalStatement l e s1 (Just s2)) = f (f (fcond acc l e) s1) s2
  f acc (WhileStatement l e body)    = f (fwhile acc l e) body
  f acc (SwitchStatement l e body)   = f (fswitch acc l e) body
  f acc (RValueStatement l e)        = frvalue acc l e
  f acc (GotoStatement l e)          = fgoto acc l e
  f acc (NullStatement l)            = fnull acc l
  f acc (ReturnStatement l me)       = freturn acc l me
  f acc (BreakStatement l)           = fbreak acc l

