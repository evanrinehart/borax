module Compiler where

import Control.Monad.State

import Asm
import Expr

type Compiler    = State CompileData
data CompileData = CompileData

-- compiler context
--   where are the local variables (stack) and passed params (register/stack)
--   where are the string constants (symbols)
--   which non-clobberable registers were used for temporaries
--   
--
-- context when compiling an isolated expression to asm
--   where (register? stack?) is the value of subexpression N right now
--   which registers are available to use for temporaries
--   generator of local labels for conditionals


-- 13: Eval[ e ] 12 ; statement to evaluate e for side effects then continue to node 12
-- 12: Return[ e ]  ; statement to evaluate e and return the value immediately
compileExpr :: Expr -> Compiler [Annot Asm2]
compileExpr fullExpr = return []

-- compile expression so that its rvalue will exist somewhere, return operand
-- also make sure any side effects happen. In this part, treat comparisons as
-- normal operations that we want the result of. Test for branch is different
compileR n dst (ExPreInc i) = do
  o <- compileL i dst =<< getExpr i
  codeOut (SL0 (Increment o))
  return o
compileR n _ (ExBinary Quotient i1 i2) = divisionDance n RAX i1 i2
compileR n _ (ExBinary Modulo i1 i2)   = divisionDance n RDX i1 i2
compileR n dst (ExBinary binop i1 i2) = do
  tmp <- newTemp
  o1  <- compileR i1 dst =<< getExpr i1
  o2  <- compileR i2 tmp =<< getExpr i2
  ensure i1 dst
  let mkIns = binaryOpInstruction binop
  codeOut (SL1 dst ( dst o2))


-- worst case addition strategy, uses a temporary
algorithm2 dreg (ExBinary Plus i1 i2) = do
  subcomputation dreg =<< getExpr i1
  tmp <- getTemp
  subcomputation tmp =<< getExpr i2
  ensure i1 dreg
  codeOut (SL1 dreg (ADD dreg tmp))

algorithm2 dst (ExAmp i) = do
  -- we find out the expression looks like &x where x lives at s[2]
  codeOut (SL1 dst (Neutral (Amp (RawS 2))))
  -- we find out it's like &e1[e2]
  subcomputation dst e1
  tmp <- getTemp
  subcomputation dst e2
  ensure i1 dst
  codeOut (SL1 dst (Neutral (Amp (RawV dst tmp 0)))) -- dst <- &dst[tmp]
  -- which becomes later: LEA dst, [dst + tmp]
  -- we find out it's like &e1[2]
  subcomputation dst e1
  codeOut (SL1 dst (Neutral (Amp (RawV dst 16)))) -- dst <- &dst[16]
  -- which becomes later: LEA dst, [dst + 16]
  -- we find out it's like &2[e1]
  -- instead of creating a temp, compute the address of e1[2] instead
  -- we find out it's like &*ptr
  o <- operandForVariable "ptr"
  codeOut (SL1 dst (Neutral (Bare o))) -- dst <- ptr
  -- which becomes MOV dst, [ptr] if it's global
  --               MOV dst, [rbp - 8] or so if it's on the stack
  -- we find out it's like &x
  o <- operandForVarialbe "x"
  codeOut (SL1 dst (Neutral (Amp o))) -- dst <- &x
  -- which becomes MOV dst, x         (global x)
  --               LEA dst, [rbp - 8] (local x)

-- side effecting expressions, e1 = e2
algorithm2 dst (ExAssign i1 i2) = do
  -- we find out it's like x = 2
  o <- operandForVariable "x"
  codeOut (SL1 (Bare o)   (Neutral (Bare (RawI 2)))) -- x <- 2
  codeOut (SL1 (Bare dst) (Neutral (Bare (RawI 2)))) -- dst <- 2
  -- which becomes MOV [x], 2
  -- which becomes MOV dst, 2
  -- worst case scenario
  -- we can only assume it's e1 = e2
  r2 <- subcomputation dst =<< getExpr i2
  o1 <- storageComputation =<< getExpr i1 -- may have side effects
  codeOut (SL1 o1 (Neutral (Bare r2)))
  return r2


algorithm3 n dst (ExConstant k) = return ()
algorithm3 n dst (ExName name) = return ()
algorithm3 n dst (ExStar (ExConstant k)) = return ()
algorithm3 n dst (ExAmp i) =
  e <- rebuild n
  

computeTo dst n = do
  e <- getExpr n
  case e of
    ExConst _ -> return () -- constants can be consumed from nowhere any time
    ExName _  -> return () -- location of variables can be accessed any time
    ExAmp i -> do
      e' <- getExpr i
      case e' of
        ExName _ -> return () -- address are like constants, consumed any time
        ExStar i -> do
          -- address of star of e is formally e
          computeHint dst i
          o <- consume i
          utterMove dst o
        ExVector i1 i2 -> do
          computeHint dst i1
          computeHintNot dst i2
          o1 <- consume i1
          o2 <- consume i2
          utterMove dst (Amp (RawV1 o1 o2))   -- o2 is an integer OR
          utterMove dst (Amp (RawV2 o1 o2 0)) -- o2 is a register
    ExStar i -> do
      e' <- getExpr i
      case e' of
        ExConst _ -> return () -- contents of constant can be accessed any time
        _ -> do
          -- last resort implementation of star.
          -- normally the starred operand appears in the usage site directly
          computeHint dst i
          o <- consume i
          utterMove dst (Star o)
    ExBinaryOp Quotient i1 i2 -> computeDivisionTo dst i1 RAX RDX
    ExBinaryOp Modulo   i1 i2 -> computeDivisionTo dst i1 RDX RAX
    ExBinaryOp binop i1 i2 -> do
      computeHint    dst i1
      computeHintNot dst i2
      o1 <- consume i1
      o2 <- consume i2
      utterMove        dst o1 -- skipped if dst=src
      utterBinop binop dst o2
    ExAssign i1 i2 -> do
      -- value is value of i2
      -- side effect is value stored to i1
      
  
computeDivisionTo dst n rWanted rUnwanted = do
  computeToNot RAX i2
  acquire RAX
  computeTo    RAX i1
  acquire RDX
  utterCQO
  o2 <- getOperand i2
  utterIDiv o2
  forget rUnwanted
  utterMove dst rWanted
  
  

-- ampersand. The subexpression must be a variable, vector, or star
-- the variable must become a non-register location operand
-- the vector becomes a vector location operand
-- ampersand star cancels out, compile the inner expression instead
compileR n dst (ExAmp i) = do
  -- if i is a star e,   compile e instead, otherwise
  -- if i is a variable, get operand (s[2], globalsym)
  -- if i is a vector,   get operand (r1[r2 + 8])
  codeOut (SL1 dst (Amp o))
  remember n dst

data AddrExpr = AddrExpr
  { aeBase     :: Operand
  , aeRegister :: Operand
  , aeScale    :: Int
  , aeOffset   :: Int }

data Operand = OdZ Int | OdR Reg | OdS Int | OdY String | OdV 

-- star can only be used on a register or constant number operand
compile n dst (ExStar i) = do
  dst' <- requireRegister dst
  o    <- compile i (RawR dst') =<< getExpr i
  
  codeOut (SL1 dst' (Neutral (Star dst')))

-- examples of compiling an addition
-- d <- d + 2      -- 2nd operand is a number
-- d <- d + s[1]   -- 2nd operand is a local or arg
-- d <- d + name   -- 2nd operand is a global
-- d <- d + r15    -- 2nd operand is in a register, might needed to be compiled first
-- d <- d + *r15   -- 2nd operand is a star expression, had to be compiled
-- d <- d + *2     -- 2nd operand is a star number
-- d <- d + &s[1]  -- 2nd operand is amp local or arg
-- d <- d + &name  -- 2nd operand is global

-- in the *r15, *2, &s[1], &name, the "value of the subexpression" isn't actually
-- located anywhere. So the idea that we always return the location of the subexpressions
-- value is wrong. Instead, the first subexpression's value needs to be in register d.
-- Then we analyze the second subexpression to see if we need to do a subcompilation
-- and get some value in a new temp. Sometimes we don't (2, name, *2, &s[1], &name)

-- and... & and * constructions don't play nice with the "location of value of subexpr"
-- story. at best it generates too many temporaries because we can use * and & in the
-- instruction directly, as shown above. But sometimes you need to use a temporary
-- nonetheless
  

divisionDance n rWanted i1 i2 = do
  tmp <- newTemp
  acquire RDX
  acquire RAX
  _  <- compileR i1 RAX =<< getExpr i1
  o2 <- compileR i2 tmp =<< getExpr i2
  ensure i1 RAX
  codeOut (RawR RDX `bareSL1` RawI 0)
  codeOut (idiv o2)
  -- forgetR the unwanted register
  -- forgetV i1, i2
  return (RawR rWanted)

bareSL1 a b = SL1 (Bare a) (Neutral (Bare b))
idiv b = (SL2 (Bare (RawR RDX)) (Bare (RawR RAX)) (IDIV (RawR RAX) b)

--  Plus | Minus | Modulo | Times | Division |
--  LessThan | LessThanEquals | GreaterThan | GreaterThanEquals | Equals | NotEquals |
--  BitXor | BitOr | BitAnd | ShiftL | ShiftR | LogicAnd | LogicOr

-- we can do Plus Minus Times

data BinaryOpClass = SimpleBinary | SetccBinary | DivisionBinary deriving Show

isSetccBinary :: BinaryOp -> Bool
isSetccBinary x = case x of
  LessThan          -> True
  GreaterThan       -> True
  LessThenEquals    -> True
  GreaterThanEquals -> True
  Equals            -> True
  NotEquals         -> True
  _                 -> False

binaryOpClass :: BinaryOp -> BinaryOpClass
binaryOpClass op = f op where
  f Division = DivisionBinary
  f Plus     = SimpleBinary
  f Minus    = SimpleBinary
  f Times    = SimpleBinary
  f BitAnd   = SimpleBinary
  f BitOr    = SimpleBinary
  f BitXor   = SimpleBinary
  f ShiftL   = SimpleBinary
  f ShiftR   = SimpleBinary
  f LessThan = SetccBinary
  f GreaterThan = SetccBinary
  f LessThenEquals = SetccBinary
  f GreaterThanEquals = SetccBinary
  f Equals = SetccBinary
  f NotEquals = SetccBinary


binaryOpInstruction :: BinaryOp -> (a -> a -> Ins a)
binaryOpInstruction op = f op where
  f Plus   = ADD
  f Minus  = SUB
  f Times  = IMUL
  f BitAnd = AND
  f BitOr  = OR
  f BitXor = XOR
  f ShiftL = SHL
  f ShiftR = SHR

{-RawR Reg |   -- register
  RawI Int |   -- immediate number
  RawG String |-- symbol (global, static local)
  RawS Int |   -- stack location
  RawF |       -- flags
  RawV1 Reg Int |       -- vector base offset
  RawV2 Reg Reg Int -}  -- vector base register offset

-- compile expression enough to get an lvalue, return location
-- also make sure any side effects happen, e.g. a[i++] = 5
compileL (ExName name) = do
  getVariableLocation name
compileL (ExVector i1 i2) = do
  
compileL (ExStar i) = do
  tmp <- newTemp
  compileR tmp 
  

  ExConst Constant |
  ExAmp a |
  ExUnary UnaryOp a |
  ExBinary BinaryOp a a |
  ExAssign a a |
  ExAssignOp BinaryOp a a |
  ExPreInc a |
  ExPreDec a |
  ExPostInc a |
  ExPostDec a |
  ExFunc a [a] |
  ExTernary a a a |
  -- LValue
  ExName String |
  ExStar a |
  ExVector a a
-}
