module Expr where

import Doc

data K =
  KN Int |
  KC [Char] |
  KStr String
    deriving (Eq,Show)

data E =
  EK K | --
  EVar String | --
  ECond E E E | --
  ECall E [E] | --
  EUn  Unop  E |
  EBin Binop E E |
  EAssign E E |
  EAssignOp Binop E E |
  EPFix String E |
  EAmp E |
  EStar E |
  EVect E E
    deriving Show

data Unop = Minus | Bang | Tilde
    deriving Show

data Binop =
  BAdd | BSub | BMul | BDiv | BMod |
  BShr | BShl | BAnd | BOr  | BXor |
  BLt  | BLte | BGt  | BGte | BEq  | BNeq
    deriving Show


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

binopKeyword :: Binop -> String
binopKeyword = f where
  f BAdd = "+"
  f BSub = "-"
  f BMul = "*"
  f BDiv = "/"
  f BMod = "%"
  f BShr = ">>"
  f BShl = "<<"
  f BAnd = "&"
  f BOr  = "|"
  f BXor = "^"
  f BLt  = "<"
  f BLte = "<="
  f BGt  = ">"
  f BGte = ">="
  f BEq  = "=="
  f BNeq = "!="

showBinop = text . binopKeyword

showUnop = text . f where
  f Minus = "-"
  f Tilde = "~"
  f Bang  = "!"

showK :: K -> Doc
showK (KN z)     = text (show z)
showK (KC cs)    = text (showWideChar cs)
showK (KStr str) = text (show str)

cataE ::
  (K -> a) ->
  (String -> a) ->
  (a -> a -> a -> a) ->
  (a -> [a] -> a) ->
  (Unop -> a -> a) ->
  (Binop -> a -> a -> a) ->
  (a -> a -> a) ->
  (Binop -> a -> a -> a) ->
  (String -> a -> a) ->
  (a -> a) ->
  (a -> a) ->
  (a -> a -> a) ->
  E -> a
cataE fk fv fcond fcall funop fbinop fass fassop fpfix famp fstar fvect = f where
  f (EK k)                  = fk k
  f (EVar x)                = fv x
  f (ECond e1 e2 e3)        = fcond (f e1) (f e2) (f e3)
  f (ECall efun eargs)      = fcall (f efun) (map f eargs)
  f (EUn unop e1)           = funop unop (f e1)
  f (EBin binop e1 e2)      = fbinop binop (f e1) (f e2)
  f (EAssign e1 e2)         = fass (f e1) (f e2)
  f (EAssignOp binop e1 e2) = fassop binop (f e1) (f e2)
  f (EPFix code e1)         = fpfix code (f e1)
  f (EAmp e1)               = famp (f e1)
  f (EStar e1)              = fstar (f e1)
  f (EVect e1 e2)           = fvect (f e1) (f e2)

foldlE ::
  (a -> K -> a) ->
  (a -> String -> a) ->
  (a -> a) ->
  (a -> a) ->
  (a -> Unop -> a) ->
  (a -> Binop -> a) ->
  (a -> a) ->
  (a -> Binop -> a) ->
  (a -> String -> a) ->
  (a -> a) ->
  (a -> a) ->
  (a -> a) ->
  a -> E -> a
foldlE fk fv fcond fcall funop fbinop fass fassop fpfix famp fstar fvect = f where
  f acc (EK k)                  = fk acc k
  f acc (EVar x)                = fv acc x
  f acc (ECond e1 e2 e3)        = f (f (f (fcond acc) e1) e2) e3
  f acc (ECall efun eargs)      = foldl f (f (fcall acc) efun) eargs
  f acc (EUn unop e1)           = f (funop acc unop) e1
  f acc (EBin binop e1 e2)      = f (f (fbinop acc binop) e1) e2
  f acc (EAssign e1 e2)         = f (f (fass acc) e1) e2
  f acc (EAssignOp binop e1 e2) = f (f (fassop acc binop) e1) e2
  f acc (EPFix code e1)         = f (fpfix acc code) e1
  f acc (EAmp e1)               = f (famp acc) e1
  f acc (EStar e1)              = f (fstar acc) e1
  f acc (EVect e1 e2)           = f (f (fvect acc) e1) e2


showE :: E -> Doc
showE =
  cataE
    showK
    text
    (\a b c -> a <> text " ? " <> b <> text " : " <> c)
    (\fun args -> fun <> text "(" <> joinDocs (text ",") args <> text ")")
    (\unop d -> showUnop unop <> d)
    (\binop d1 d2 -> d1 <> showBinop binop <> d2)
    (\d1 d2 -> d1 <> text " = " <> d2)
    (\binop d1 d2 -> d1 <> text " =" <> showBinop binop <> text " " <> d2)
    (\code d -> showPfix code d)
    (\d -> text "&" <> d)
    (\d -> text "*" <> d)
    (\d1 d2 -> d1 <> text "[" <> d2 <> text "]")

showPfix :: String -> Doc -> Doc
showPfix "_++" d = d <> text "++"
showPfix "_--" d = d <> text "--"
showPfix "--_" d = text "--" <> d
showPfix "++_" d = text "++" <> d

stringsInExpr :: E -> [String]
stringsInExpr =
  cataE
    (\k -> case k of KStr str -> [str]; _ -> [])
    (const [])
    (\s1 s2 s3 -> s1 <> s2 <> s3)
    (\s1 s2 -> s1 <> mconcat s2)
    (\_ s1 -> s1)
    (\_ s1 s2 -> s1 <> s2)
    (\s1 s2 -> s1 <> s2)
    (\_ s1 s2 -> s1 <> s2)
    (\_ s1 -> s1)
    (\s1 -> s1)
    (\s1 -> s1)
    (\s1 s2 -> s1 <> s2)


