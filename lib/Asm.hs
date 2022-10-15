module Asm where

import Data.Char (toLower)

-- | Lines of code in an asm file
data CodeLine =
  Directive String |
  Blank |
  Label String |
  Code Asm |
  Data String [D]

type Asm = AsmF Operand

-- | Skeleton for assembly language code.
data AsmF a =
  MOV a a |
  LEA a a |
  JMP a |
  JCC String a |
  SETCC String R8bit |
  TEST a |
  CMP a a |
  CQO |
  PUSH a |
  POP a |
  CALL a |
  RET |

  ADD a a |
  SUB a a |
  IMUL a a |
  IDIV a a |
  INC a |
  DEC a |
  SHL a |
  SHR a |
  NOT a |
  AND a a |
  OR a a |
  XOR a a |
  NEG a
  

data Operand = OpK K | OpR R | OpY String | OpM MemForm

data K = KN Int | KC Char

data R =
  RAX | RBX | RCX | RDX |
  RDI | RSI | RBP | RSP |
  R8  | R9  | R10 | R11 |
  R12 | R13 | R14 | R15
    deriving Show

data R8bit = AL | BL | CL | DL
  deriving Show

-- | This form is used for memory access operands
data MemForm =
  M00 Int |
  M01 R Int Int |
  M10 String Int |
  M11 String R Int Int

data D = DN Int | DC String


-- | Pretty print functions for output [CodeLine]

showCodeLines :: [CodeLine] -> Doc
showCodeLines = f where
  f (Blank : more)         = newline <> f more
  f (Label str : more)     = text str <> text ":" <> newline <> f more
  f (Code asm : more)      = tab <> showAsmF showOperand asm <> newline <> f more
  f (Data ty dats : more)  = tab <> showData ty dats <> newline <> f more
  f (Directive str : more) = tab <> text str <> newline <> f more
  f []                     = nil

showAsmF :: (a -> Doc) -> AsmF a -> Doc
showAsmF sh = f where
  g = showAsm sh
  f (MOV x y) = g "mov" [x, y]
  f (LEA x y) = g "lea" [x, y]
  f (JMP x)   = g "jmp" [x]
  f (JCC cc x) = g ("j" ++ cc) [x]
  f (SETCC cc x) = showAsm showR8bit ("set" ++ cc) [x]
  f (CMP x y) = g "cmp" [x, y]
  f (CALL x)  = g "call" [x]

showAsm :: (a -> Doc) -> String -> [a] -> Doc
showAsm sh mnemonic os = showIndent mnemonic (showCommas (map sh os))

showData :: String -> [D] -> Doc
showData ty dats = showIndent ty (showCommas (map showD dats))

showOperand :: Operand -> Doc
showOperand = f where
  f (OpK k) = showK k
  f (OpR r) = showR r
  f (OpY s) = text s
  f (OpM m) = showMemForm m

showK :: K -> Doc
showK (KN n) = text (show n)
showK (KC c) = text (show c)

showR :: R -> Doc
showR = text . map toLower . show

showR8bit :: R8bit -> Doc
showR8bit = text . map toLower . show

showMemForm :: MemForm -> Doc
showMemForm m = text "[" <> showSum (f m) <> text "]" where
  f (M00 n)          = [showN n]
  f (M01 r s n)      = [showScale r s, showN n]
  f (M10 name n)     = [text name, showN n]
  f (M11 name r s n) = [text name, showScale r s, showN n]

showN :: Int -> Doc
showN = text . show

showSum :: [Doc] -> Doc
showSum = joinDocs (text " + ")

showScale :: R -> Int -> Doc
showScale r s = showR r <> text "*" <> showN s

showD :: D -> Doc
showD (DN n)  = text (show n)
showD (DC cs) = text "'" <> text cs <> text "'"

showCommas :: [Doc] -> Doc
showCommas = joinDocs (text ", ")

showIndent :: String -> Doc -> Doc
showIndent header body =
  let alignSpace = replicate (max 1 (8 - length header)) ' '
  in text header <> text alignSpace <> body

joinDocs :: Doc -> [Doc] -> Doc
joinDocs sep []     = nil
joinDocs sep [d]    = d
joinDocs sep (d:ds) = d <> sep <> joinDocs sep ds





-- | document type for asm output

data Doc =
  Nil |
  Newline |
  Tab |
  Text String |
  Doc :<> Doc

flatten :: Doc -> String
flatten d = f d "" where
  f Nil rest         = rest
  f Newline rest     = '\n' : rest
  f Tab rest         = '\t' : rest
  f (Text str) rest  = str ++ rest
  f (d1 :<> d2) rest = f d1 (f d2 rest)

nil :: Doc
nil = Nil

newline :: Doc
newline = Newline

tab :: Doc
tab = Tab

text :: String -> Doc
text = Text

instance Semigroup Doc where
  (<>) = (:<>)

instance Monoid Doc where
  mempty = nil
