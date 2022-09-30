module Asm where

import Data.List
import Data.Char

data Reg =
  RAX | RBX | RCX | RDX |
  RDI | RSI | RBP | RSP |
  R8  | R9  | R10 | R11 |
  R12 | R13 | R14 | R15
    deriving Show

data RawOperand =
  RawR Reg |
  RawI Int |
  RawG String |
  RawS Int |
  RawF |
  RawV1 Reg Int |
  RawV2 Reg Reg Int
  deriving Show

-- finalish form :: ThreeCode RawOperand
-- before that   :: ThreeCode (Either Int RawOperand)

showRawOperand :: RawOperand -> String
showRawOperand (RawR reg)        = showReg reg
showRawOperand (RawI i)          = show i
showRawOperand (RawG name)       = name
showRawOperand RawF              = "(flags)"
showRawOperand (RawS off)        = "s[" ++ show off ++ "]"
showRawOperand (RawV1 r off)     = concat [showReg r, "[", show off, "]"]
showRawOperand (RawV2 r1 r2 off) =
  concat [showReg r1, "[", showReg r1, "*8 + ", show (off * 8), "]"]

showRawOperandOr :: Either Int RawOperand -> String
showRawOperandOr (Left  n)   = "v" ++ show n
showRawOperandOr (Right raw) = showRawOperand raw

data MemHat a = Star a | Amp a | Bare a
  deriving Show

showMemHat :: (a -> String) -> MemHat a -> String
showMemHat sh (Star x) = '*' : sh x
showMemHat sh (Amp x)  = '&' : sh x
showMemHat sh (Bare x) =       sh x

showReg :: Reg -> String
showReg = map toLower . show


data Ins a =
  Neutral a |
  Plus a a |
  Minus a a |
  Times a a |
  Divide a a |
  Compare a a |
  Negate a |
  SignExtension a |
  JumpLess String |
  JumpEqual String |
  JumpNotEqual String |
  Jump String |
  Call a [a] |
  Increment a |
  Decrement a |
  Push a |
  Pop a |
  Return
    deriving Show

data StabLeft a =
  SL0 (Ins a) |
  SL1 a (Ins a) |
  SL2 a a (Ins a)
    deriving Show

type VarOperand = Either Int RawOperand

type Asm1 = StabLeft (MemHat RawOperand)
type Asm2 = StabLeft (MemHat VarOperand)

showAsm1 = showSL (showMemHat showRawOperand)
showAsm2 = showSL (showMemHat showRawOperandOr)

data Annot a = Annot
  { annLabel   :: String
  , annBody    :: a
  , annComment :: String }
      deriving Show

showIns :: (a -> String) -> Ins a -> String
showIns sh (Neutral x) = sh x
showIns sh (Plus x y) = sh x ++ " + " ++ sh y
showIns sh (Minus x y) = sh x ++ " - " ++ sh y
showIns sh (Times x y) = sh x ++ " * " ++ sh y
showIns sh (Divide x y) = sh x ++ " div " ++ sh y
showIns sh (Compare x y) = sh x ++ " cmp " ++ sh y
showIns sh (Negate x) = "neg " ++ sh x
showIns sh (JumpLess label) = "jl " ++ label
showIns sh (JumpEqual label) = "je " ++ label
showIns sh (JumpNotEqual label) = "jne " ++ label
showIns sh (Jump label) = "jmp " ++ label
showIns sh (Call x xs) = "call " ++ sh x ++ "(" ++ guts ++ ")" where
  guts = concat (intersperse ", " (map sh xs))
showIns sh (Increment x) = sh x ++ "++"
showIns sh (Decrement x) = sh x ++ "--"
showIns sh (Push x) = "push " ++ sh x
showIns sh (Pop x) = "pop " ++ sh x
showIns sh Return = "return"

showSL :: (a -> String) -> StabLeft a -> String
showSL sh (SL0 op) =
  let pad0 = replicate (8 + length " <- ") ' '
  in pad0 ++ showIns sh op
showSL sh (SL1 dst op) =
  let part0 = sh dst
      pad0 = replicate (max 0 (8 - length part0)) ' '
  in part0 ++ pad0 ++ " <- " ++ showIns sh op
showSL sh (SL2 dst1 dst2 op) =
  let part0 = sh dst1 ++ ", " ++ sh dst2
      pad0 = replicate (max 0 (8 - length part0)) ' '
  in part0 ++ pad0 ++ " <- " ++ showIns sh op

showAnnot :: (a -> String) -> Annot a -> String
showAnnot sh (Annot label body comment) =
  let part0 = case label of {"" -> ""; _ -> label ++ ":"}
      pad0 = replicate (max 0 (10 - length part0)) ' '
      part01 = part0 ++ pad0 ++ sh body
      pad1 = replicate (max 0 (45 - length part01)) ' '
  in case comment of
    "" -> part01
    _  -> part01 ++ pad1 ++ "; " ++ comment

c1 :: Annot Asm1
c1 = Annot ".loop" (SL1 (Bare (RawR RAX)) (Neutral (Bare (RawI 5)))) "nothing to see"
c11 = Annot "" (SL1 (Bare (RawR RAX)) (Neutral (Bare (RawI 5)))) ""


c2 :: Annot Asm2
c2 = Annot ".loop" (SL1 (Bare (Right (RawR RAX))) (Plus (Bare (Left 3)) (Bare (Right (RawI 5))))) "nothing to see"


dumpAsm1 :: [Annot Asm1] -> String
dumpAsm1 = unlines . map (showAnnot showAsm1)

dumpAsm2 :: [Annot Asm2] -> String
dumpAsm2 = unlines . map (showAnnot showAsm2)


default (Int)
c18a = Annot ".18" (SL1 (Bare (RawR RDI)) (Neutral $ Bare (RawS 1))) "fmt"
c18b = Annot "" (SL1 (Bare (RawR RSI)) (Neutral $ Bare (RawS 2))) "i"
c18c = Annot "" (SL1 (Bare (RawR RAX)) (Call (Bare $ RawG "char") [Bare (RawR RDI), Bare (RawR RSI)])) ""
c18d = Annot "" (SL0 (Increment (Bare (RawS 2)))) "i++"
c18e = Annot "" (SL1 (Bare (RawS 3)) (Neutral (Bare (RawR RAX)))) "c <- rax"
c18f = Annot "" (SL1 (Bare RawF) (Compare (Bare (RawR RAX)) (Bare (RawI (ord '%'))))) ""
c18g = Annot "" (SL0 (JumpNotEqual ".21")) ""
{-
.18: mov rdi, [fmt]
     mov rsi, [i] ; remember to increment i
     call char    ; remember to move what's currently in rax into [c]
     inc [i]
     mov [c], rax
     cmp rax, '%'
     jne 21
-}
{-
.18:      rdi      <- s[1]                   ; fmt
          rsi      <- s[2]                   ; i
          rax      <- call char(rdi, rsi)
                      s[2]++                 ; i++
          s[3]     <- rax                    ; c <- rax
          (flags)  <- rax cmp 37
                      jne .21
-}
