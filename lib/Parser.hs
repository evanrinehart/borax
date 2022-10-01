module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import System.Exit
import Data.Fix

import Expr
import Syntax

type Parser = Parsec Void String
--type Error = ParseErrorBundle String Void

parse :: String -> String -> Either String Boron
parse filename text = case runParser program filename text of
  Right p -> return p
  Left bundle -> Left (errorBundlePretty bundle)

parseFile :: FilePath -> IO Boron
parseFile path = do
  txt <- readFile path
  case runParser program path txt of
    Right p  -> return p
    Left bundle -> do
      putStrLn (errorBundlePretty bundle)
      exitFailure

--printError :: Error -> IO ()
--printError = putStrLn . errorBundlePretty 

binaryOp :: Parser BinaryOp
binaryOp =
  BitOr <$ char '|' <|>
  BitAnd <$ char '&' <|>
  Equals <$ string "==" <|>
  NotEquals <$ string "!=" <|>
  LessThan <$ char '<' <|>
  LessThanEquals <$ string "<=" <|>
  GreaterThan <$ char '>' <|>
  GreaterThanEquals <$ string ">=" <|>
  ShiftL <$ string "<<" <|>
  ShiftR <$ string ">>" <|>
  Plus <$ char '+' <|>
  Minus <$ char '-' <|>
  Modulo <$ char '%' <|>
  Times <$ char '*' <|>
  Division <$ char '/'

ival :: Parser IVal
ival =
  IVConst <$> constant <|>
  IVName <$> validName

validName :: Parser String
validName = do
  c <- letterChar <|> char '_' <|> char '.'
  cs <- many (alphaNumChar <|> char '_' <|> char '.')
  remspace
  case c:cs of
    "auto"   -> fail "auto is a reserved word"
    "extrn"  -> fail "extrn is a reserved word"
    "case"   -> fail "case is a reserved word"
    "if"     -> fail "if is a reserved word"
    "while"  -> fail "while is a reserved word"
    "switch" -> fail "switch is a reserved word"
    "goto"   -> fail "goto is a reserved word"
    "break"  -> fail "break is a reserved word"
    "return" -> fail "return is a reserved word"
    _ -> return (c : cs)

constant :: Parser Constant
constant =
  ConstI <$> numericConstant <|>
  ConstS <$> stringConstant <|>
  ConstC <$> charConstant

numericConstant :: Parser Int
numericConstant = do
  ds <- some digitChar
  remspace
  case ds of
    "0"        -> return 0
    ('0':more) -> return (funnyOctal more)
    _          -> return (read ds)

funnyOctal :: String -> Int
funnyOctal = foldl1 (\a o -> a*8 + o) . map digitToInt

stringConstant :: Parser String
stringConstant = do
  char '"'
  str <- escapeSequence `manyTill` char '"'
  remspace
  return str

charConstant :: Parser [Char]
charConstant = do
  char '\''
  cs <- escapeSequence `manyTill` char '\''
  remspace
  return cs

escapeSequence :: Parser Char
escapeSequence = do
  c <- printChar
  case c of
    '*' -> do
      c2 <- printChar
      case c2 of
        '0' -> return '\0'
        'e' -> return '\EOT'
        '(' -> return '{'
        ')' -> return '}'
        't' -> return '\t'
        '*' -> return '*'
        '\'' -> return '\''
        '"' -> return '"'
        'n' -> return '\n'
        _ -> fail "unknown escape sequence"
    '\'' -> fail "empty char constant"
    _ -> return c  

remspace :: Parser ()
remspace = do
  space
  many $ do
    string "/*"
    anySingle `skipManyTill` (string "*/")
    space
  return ()

remspace1 :: Parser ()
remspace1 = do
  spaceChar
  remspace

getLineNo :: Parser Int
getLineNo = (unPos . sourceLine) <$> getSourcePos

program :: Parser Boron
program = do
  remspace
  defs <- many (try definition0 <|> try definition1 <|> definition2)
  return (Boron defs)

definition0 :: Parser Definition
definition0 = do
  lineNo <- getLineNo
  myName <- validName
  remspace
  maybeIVal <- optional ival
  remspace
  char ';'
  remspace
  return (DefV1 lineNo myName maybeIVal)

definition1 :: Parser Definition
definition1 = do
  lineNo <- getLineNo
  myName <- validName
  char '['
  remspace
  maybeDim <- optional numericConstant
  remspace
  char ']'
  remspace
  ivals <- ival `sepBy` (char ',' >> remspace)
  char ';'
  remspace
  return (DefVN lineNo myName maybeDim ivals)

definition2 :: Parser Definition
definition2 = do
  lineNo <- getLineNo
  myName <- validName
  char '('
  remspace
  params <- validName `sepBy` (char ',' >> remspace)
  char ')'
  remspace
  body <- statement
  remspace
  return (DefF (FunctionDef lineNo myName params body))

statement :: Parser Statement
statement = 
  autoStatement <|>
  extrnStatement <|>
  caseStatement <|>
  compoundStatement <|>
  conditionalStatement <|>
  whileStatement <|>
  switchStatement <|>
  gotoStatement <|>
  breakStatement <|>
  returnStatement <|>
  try labelStatement <|>
  rvalueStatement <|>
  nullStatement

autoDecl :: Parser (Name, Maybe Int)
autoDecl = do
  nomo <- validName
  size <- optional $ do
    char '['
    remspace
    k <- numericConstant
    char ']'
    remspace
    return k
  return (nomo, size)

autoStatement :: Parser Statement
autoStatement = do
  lineNo <- getLineNo
  string "auto"
  remspace1
  nameSizes <- sepBy1 autoDecl (char ',' >> remspace)
  char ';'
  remspace
  next <- statement
  return (AutoStatement lineNo nameSizes next)

extrnStatement :: Parser Statement
extrnStatement = do
  lineNo <- getLineNo
  string "extrn"
  remspace1
  names <- validName `sepBy1` (char ',' >> remspace)
  char ';'
  remspace
  next <- statement
  return (ExtrnStatement lineNo names next)

labelStatement :: Parser Statement
labelStatement = do
  lineNo <- getLineNo
  l <- validName
  char ':'
  remspace
  next <- statement
  return (LabelStatement lineNo l next)

caseStatement :: Parser Statement
caseStatement = do
  lineNo <- getLineNo
  string "case"
  remspace
  v <- constant
  char ':'
  remspace
  next <- statement
  return (CaseStatement lineNo v next)

compoundStatement :: Parser Statement
compoundStatement = do
  lineNo <- getLineNo
  char '{'
  remspace
  stats <- many statement
  char '}'
  remspace
  return (CompoundStatement lineNo stats)

conditionalStatement :: Parser Statement
conditionalStatement = do
  lineNo <- getLineNo
  string "if"
  remspace
  char '('
  remspace
  e <- anyExpr
  char ')'
  remspace
  next1 <- statement
  next2 <- optional $ do
    string "else"
    remspace
    statement
  return (ConditionalStatement lineNo e next1 next2)

whileStatement :: Parser Statement
whileStatement = do
  lineNo <- getLineNo
  string "while"
  remspace
  char '('
  remspace
  e <- anyExpr
  char ')'
  remspace
  body <- statement
  return (WhileStatement lineNo e body)

switchStatement :: Parser Statement
switchStatement = do
  lineNo <- getLineNo
  string "switch"
  remspace
  e <- anyExpr
  body <- statement
  return (SwitchStatement lineNo e body)

gotoStatement :: Parser Statement
gotoStatement = do
  lineNo <- getLineNo
  string "goto"
  remspace
  target <- anyExpr
  char ';'
  remspace
  return (GotoStatement lineNo target)

breakStatement :: Parser Statement
breakStatement = do
  lineNo <- getLineNo
  string "break"
  remspace
  char ';'
  remspace
  return (BreakStatement lineNo)

returnStatement :: Parser Statement
returnStatement = do
  lineNo <- getLineNo
  string "return"
  remspace
  r <- optional $ do
    char '('
    remspace
    e <- anyExpr
    char ')'
    remspace
    return e
  char ';'
  remspace
  return (ReturnStatement lineNo r)

rvalueStatement :: Parser Statement
rvalueStatement = do
  lineNo <- getLineNo
  v <- anyExpr
  char ';'
  remspace
  return (RValueStatement lineNo v)

nullStatement :: Parser Statement
nullStatement = do
  lineNo <- getLineNo
  char ';'
  remspace
  return (NullStatement lineNo)


-- expression parsers
nameExpr :: Parser Expr
nameExpr = do
  n <- validName
  remspace
  return (Fix (ExName n))

constExpr :: Parser Expr
constExpr = do
  c <- constant
  return (Fix (ExConst c))

parenExpr :: Parser Expr
parenExpr = do
  char '('
  remspace
  body <- anyExpr
  char ')'
  remspace
  return body

primaryExpr :: Parser Expr
primaryExpr = do
  h <- nameExpr <|> constExpr <|> parenExpr
  suffixes <- many (primaryEtc1 <|> primaryEtc2)
  return (foldl (.) id suffixes h)

primaryEtc1 :: Parser (Expr -> Expr)
primaryEtc1 = do
  char '['
  remspace
  body <- anyExpr
  char ']'
  remspace
  return (\h -> Fix (ExVector h body))

primaryEtc2 :: Parser (Expr -> Expr)
primaryEtc2 = do
  char '('
  remspace
  args <- anyExpr `sepBy` (char ',' >> remspace)
  char ')'
  remspace
  return (\h -> Fix (ExFunc h args))
  

prefixOp :: Parser (Expr -> Expr)
prefixOp =
  wrapunary LogicNot <$ (char '!' >> remspace) <|>
  wrapunary Negative <$ (char '-' >> remspace) <|>
  wrapunary BitComplement <$ (char '~' >> remspace) <|>
  starE <$ (char '*' >> remspace) <|>
  ampE  <$ (char '&' >> remspace) <|>
  plusPlusE   <$ (string "++" >> remspace) <|>
  minusMinusE <$ (string "--" >> remspace)

postfixOp :: Parser (Expr -> Expr)
postfixOp =
  ePlusPlus   <$ (string "++" >> remspace) <|>
  eMinusMinus <$ (string "--" >> remspace)

ePlusPlus e = Fix (ExPostInc e)
eMinusMinus e = Fix (ExPostDec e)
plusPlusE e = Fix (ExPreInc e)
minusMinusE e = Fix (ExPreDec e)
ampE e = Fix (ExAmp e)
starE e = Fix (ExStar e)
wrapunary op e = Fix (ExUnary op e)

primaryWithPostOp :: Parser Expr
primaryWithPostOp = do
  body <- primaryExpr
  wrappers <- many postfixOp
  return (foldl (\e f -> f e) body wrappers)

unaryExpr :: Parser Expr
unaryExpr = do
  wrappers <- reverse <$> many prefixOp
  body <- primaryWithPostOp
  return (foldl (\e f -> f e) body wrappers)

multOp :: Parser (Expr -> Expr -> Expr)
multOp =
  wrapbin Modulo   <$ (char '%' >> remspace) <|>
  wrapbin Times    <$ (char '*' >> remspace) <|>
  wrapbin Division <$ (char '/' >> remspace)

multChain :: Parser Expr
multChain = chainl1 unaryExpr multOp

additiveOp :: Parser (Expr -> Expr -> Expr)
additiveOp = 
  wrapbin Plus  <$ (char '+' >> remspace) <|>
  wrapbin Minus <$ (char '-' >> remspace)

additiveChain :: Parser Expr
additiveChain = chainl1 multChain additiveOp

shiftOp :: Parser (Expr -> Expr -> Expr)
shiftOp =
  wrapbin ShiftL <$ (string "<<" >> remspace) <|>
  wrapbin ShiftR <$ (string ">>" >> remspace)

shiftChain :: Parser Expr
shiftChain = chainl1 additiveChain shiftOp

relationalChain :: Parser Expr
relationalChain = chainl1 shiftChain op where
  op =
    wrapbin LessThan <$ (char '<' >> remspace) <|>
    wrapbin LessThanEquals <$ (string "<=" >> remspace) <|>
    wrapbin GreaterThan <$ (char '>' >> remspace) <|>
    wrapbin GreaterThanEquals <$ (string ">=" >> remspace)

equalityChain :: Parser Expr
equalityChain = chainl1 relationalChain op where
  op =
    wrapbin Equals <$ (string "==" >> remspace) <|>
    wrapbin NotEquals <$ (string "!=" >> remspace)


bitOrChain :: Parser Expr
bitOrChain = chainl1 bitAndChain op where
  op = wrapbin BitOr <$ (char '|' >> remspace)

bitAndChain :: Parser Expr
bitAndChain = chainl1 equalityChain op where
  op = wrapbin BitAnd <$ (char '&' >> remspace)

wrapbin :: BinaryOp -> Expr -> Expr -> Expr
wrapbin op e1 e2 = Fix (ExBinary op e1 e2)


-- parse an assignment operator, =, =+, etc
assignmentOp :: Parser (Expr -> Expr -> Expr)
assignmentOp = do
  char '='
  mop <- optional binaryOp
  remspace
  case mop of
    Nothing -> return (\e1 e2 -> Fix (ExAssign e1 e2))
    Just op -> return (\e1 e2 -> Fix (ExAssignOp op e1 e2))

assignChain :: Parser Expr
assignChain = chainr1 ternaries assignmentOp

ternaries :: Parser Expr
ternaries = do
  a <- bitOrChain 
  questionMaybe <- optional (char '?')
  remspace
  case questionMaybe of
    Nothing -> return a
    Just _ -> do
      b <- ternaries
      char ':'
      remspace
      c <- ternaries
      return (Fix (ExTernary a b c))

chainl1 :: Parser term -> Parser (term -> term -> term) -> Parser term
chainl1 term op = do
  t0 <- term
  loop t0 where
    loop accum = do
      combineMaybe <- optional op
      case combineMaybe of
        Nothing -> return accum
        Just combine -> do
          t <- term
          loop (accum `combine` t)

chainr1 :: Parser term -> Parser (term -> term -> term) -> Parser term
chainr1 term op = do
  t0 <- term
  builder <- loop
  return (builder t0) where
    loop = do
      combineMaybe <- optional op
      case combineMaybe of
        Nothing -> return id
        Just combine -> do
          t <- term
          builder <- loop
          return (\pre -> pre `combine` builder t)

anyExpr :: Parser Expr
anyExpr = assignChain
