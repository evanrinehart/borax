module Compiler where

import Control.Monad.State

import Asm
import Expr

type Compiler = State CompileData
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
