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

parse :: String -> String -> Either String FileAST
parse filename text = case runParser program filename text of
  Right p -> return p
  Left bundle -> Left (errorBundlePretty bundle)

parseFile :: FilePath -> IO FileAST
parseFile path = do
  txt <- readFile path
  case runParser program path txt of
    Right p  -> return p
    Left bundle -> do
      putStrLn (errorBundlePretty bundle)
      exitFailure


ival :: Parser IVal
ival =
  IVConst <$> constant <|>
  IVName <$> validName

getLineNo :: Parser Int
getLineNo = (unPos . sourceLine) <$> getSourcePos

program :: Parser FileAST
program = do
  remspace
  defs <- many (try definition0 <|> try definition1 <|> definition2)
  return (FileAST defs)

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


-- | Expression parser

parseE :: String -> E
parseE str = case runParser (space >> anyExpr) "unknown" str of
  Left eb -> error (show eb)
  Right e -> e


unopKeyword :: Unop -> String
unopKeyword Minus = "negate"
unopKeyword Tilde = "complement"
unopKeyword Bang  = "not"

anyExpr :: Parser E
anyExpr = assignChain

assignChain :: Parser E
assignChain = chainr1 ternaries assignmentOp

ternaries :: Parser E
ternaries = do
  a <- bitOrChain
  questionMaybe <- optional (char '?')
  remspace
  case questionMaybe of
    Nothing -> return a
    Just _ -> do
      b <- anyExpr
      char ':'
      remspace
      c <- anyExpr
      return (ECond a b c)

bitOrChain :: Parser E
bitOrChain = chainl1 bitAndChain op where
  op = wrapbin BOr <$ (char '|' >> remspace)

bitAndChain :: Parser E
bitAndChain = chainl1 equalityChain op where
  op = wrapbin BAnd <$ (char '&' >> remspace)

equalityChain :: Parser E
equalityChain = chainl1 relationalChain op where
  op =
    wrapbin BNeq <$ (string "!=" >> remspace) <|>
    wrapbin BEq  <$ try (string "==" >> notFollowedBy (char '=') >> remspace)

relationalChain :: Parser E
relationalChain = chainl1 shiftChain op where
  op =
    wrapbin BLte <$ (string "<=" >> remspace) <|>
    wrapbin BLt  <$ (   char '<' >> remspace) <|>
    wrapbin BGte <$ (string ">=" >> remspace) <|>
    wrapbin BGt  <$ (   char '>' >> remspace)

shiftChain :: Parser E
shiftChain = chainl1 additiveChain shiftOp

additiveChain :: Parser E
additiveChain = chainl1 multChain additiveOp

multChain :: Parser E
multChain = chainl1 unaryExpr multOp

unaryExpr :: Parser E
unaryExpr = do
  wrappers <- reverse <$> many prefixOp
  body <- primaryWithPostOp
  return (foldl (\e f -> f e) body wrappers)

primaryWithPostOp :: Parser E
primaryWithPostOp = do
  body <- primaryExpr
  wrappers <- many postfixOp
  return (foldl (\e f -> f e) body wrappers)

primaryExpr :: Parser E
primaryExpr = do
  h <- nameExpr <|> constExpr <|> parenExpr
  suffixes <- many (primaryEtc1 <|> primaryEtc2)
  return (foldl (.) id suffixes h)

nameExpr :: Parser E
nameExpr = do
  x <- validName
  remspace
  return (EVar x)

constExpr :: Parser E
constExpr = do
  k <- constant
  return (EK k)

parenExpr :: Parser E
parenExpr = do
  char '('
  remspace
  body <- anyExpr
  char ')'
  remspace
  return body

primaryEtc1 :: Parser (E -> E)
primaryEtc1 = do
  char '['
  remspace
  body <- anyExpr
  char ']'
  remspace
  return (\h -> EVect h body)

primaryEtc2 :: Parser (E -> E)
primaryEtc2 = do
  char '('
  remspace
  args <- anyExpr `sepBy` (char ',' >> remspace)
  char ')'
  remspace
  return (\h -> ECall h args)

-- | Operator parsers

prefixOp :: Parser (E -> E)
prefixOp =
  wrapunary Bang <$ (char '!' >> remspace) <|>
  wrapunary Tilde <$ (char '~' >> remspace) <|>
  starE <$ (char '*' >> remspace) <|>
  ampE  <$ (char '&' >> remspace) <|>
  plusPlusE   <$ (string "++" >> remspace) <|>
  minusMinusE <$ (string "--" >> remspace) <|>
  wrapunary Minus <$ (char '-' >> remspace)

postfixOp :: Parser (E -> E)
postfixOp =
  ePlusPlus   <$ (string "++" >> remspace) <|>
  eMinusMinus <$ (string "--" >> remspace)

ePlusPlus e   = EPFix "_++" e
eMinusMinus e = EPFix "_--" e
plusPlusE e   = EPFix "++_" e
minusMinusE e = EPFix "--_" e
ampE e        = EAmp e
starE e       = EStar e
wrapunary op e = EUn op e

multOp :: Parser (E -> E -> E)
multOp =
  wrapbin BMod <$ (char '%' >> remspace) <|>
  wrapbin BMul <$ (char '*' >> remspace) <|>
  wrapbin BDiv <$ (char '/' >> remspace)

additiveOp :: Parser (E -> E -> E)
additiveOp = 
  wrapbin BAdd <$ (char '+' >> remspace) <|>
  wrapbin BSub <$ (char '-' >> remspace)

shiftOp :: Parser (E -> E -> E)
shiftOp =
  wrapbin BShl <$ (string "<<" >> remspace) <|>
  wrapbin BShr <$ (string ">>" >> remspace)

wrapbin :: Binop -> E -> E -> E
wrapbin op e1 e2 = EBin op e1 e2

assignmentOp :: Parser (E -> E -> E)
assignmentOp = do
  char '='
  mop <- optional binaryOp
  remspace
  case mop of
    Nothing -> return (\e1 e2 -> EAssign e1 e2)
    Just op -> return (\e1 e2 -> EAssignOp op e1 e2)

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

binaryOp :: Parser Binop
binaryOp =
  BNeq <$ string "!=" <|>
  snowflakeEq <|>
  BShl <$ string "<<" <|>
  BShr <$ string ">>" <|>
  BLte <$ string "<=" <|>
  BGte <$ string ">=" <|>
  BLt  <$ char '<' <|>
  BGt  <$ char '>' <|>
  BOr  <$ char '|' <|>
  BAnd <$ char '&' <|>
  BAdd <$ char '+' <|>
  BSub <$ char '-' <|>
  BMod <$ char '%' <|>
  BMul <$ char '*' <|>
  BDiv <$ char '/'

snowflakeEq :: Parser Binop
snowflakeEq = do
  string "=="
  notFollowedBy (char '=')
  return BEq

constant :: Parser K
constant =
  KN <$> numericConstant <|>
  KStr <$> stringConstant <|>
  charConstant

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

charConstant :: Parser K
charConstant = do
  char '\''
  cs <- escapeSequence `manyTill` char '\''
  remspace
  return (KC cs)

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

