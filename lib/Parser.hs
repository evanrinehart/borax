module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char

import Syntax

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

parse :: String -> String -> Either Error Program
parse filename = runParser program filename

printError :: Error -> IO ()
printError = putStrLn . errorBundlePretty 

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
  c <- letterChar
  cs <- many alphaNumChar
  remspace
  case c:cs of
    "auto"   -> fail "auto is a reserved word"
    "extrn"  -> fail "extrn is a reserved word"
    "case"   -> fail "case is a reserved word"
    "if"     -> fail "if is a reserved word"
    "while"  -> fail "while is a reserved word"
    "switch" -> fail "switch is a reserved word"
    "goto"   -> fail "goto is a reserved word"
    "return" -> fail "return is a reserved word"
    _ -> return (c : cs)

constant :: Parser Constant
constant =
  ConstNumber <$> numericConstant <|>
  ConstString <$> stringConstant <|>
  ConstChar <$> charConstant

numericConstant :: Parser Integer
numericConstant = do
  n <- some digitChar
  remspace
  return (read n)

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

program :: Parser Program
program = do
  remspace
  defs <- many (try definition1 <|> definition2)
  return (Program defs)

definition1 :: Parser Definition
definition1 = do
  myName <- validName
  bracket <- optional $ do
    char '['
    remspace
    dim <- optional constant
    remspace
    char ']'
    return dim
  remspace
  ivals <- ival `sepBy` (char ',' >> remspace)
  char ';'
  remspace
  case bracket of
    Just dim -> return (VectorDef myName dim ivals)
    Nothing  -> return (AtomicDef myName ivals)

definition2 :: Parser Definition
definition2 = do
  myName <- validName
  char '('
  remspace
  params <- validName `sepBy` (char ',' >> remspace)
  char ')'
  remspace
  body <- statement
  remspace
  return (FunctionDef myName params body)

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
  returnStatement <|>
  try labelStatement <|>
  rvalueStatement <|>
  nullStatement

autoDecl :: Parser (Name, Maybe Constant)
autoDecl = do
  nomo <- validName
  size <- optional $ do
    char '['
    remspace
    k <- constant
    char ']'
    remspace
    return k
  return (nomo, size)

autoStatement :: Parser Statement
autoStatement = do
  string "auto"
  remspace1
  nameSizes <- sepBy1 autoDecl (char ',' >> remspace)
  char ';'
  remspace
  next <- statement
  return (AutoStatement nameSizes next)

extrnStatement :: Parser Statement
extrnStatement = do
  string "extrn"
  remspace1
  names <- validName `sepBy1` (char ',' >> remspace)
  char ';'
  remspace
  next <- statement
  return (ExtrnStatement names next)

labelStatement :: Parser Statement
labelStatement = do
  l <- validName
  char ':'
  remspace
  next <- statement
  return (LabelStatement l next)

caseStatement :: Parser Statement
caseStatement = do
  string "case"
  remspace
  v <- constant
  char ':'
  remspace
  next <- statement
  return (CaseStatement v next)

compoundStatement :: Parser Statement
compoundStatement = do
  char '{'
  remspace
  stats <- many statement
  char '}'
  remspace
  return (CompoundStatement stats)

conditionalStatement :: Parser Statement
conditionalStatement = do
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
  return (ConditionalStatement e next1 next2)

whileStatement :: Parser Statement
whileStatement = do
  string "while"
  remspace
  char '('
  remspace
  e <- anyExpr
  char ')'
  remspace
  body <- statement
  return (WhileStatement e body)

switchStatement :: Parser Statement
switchStatement = do
  string "switch"
  remspace
  e <- anyExpr
  body <- statement
  return (SwitchStatement e body)

gotoStatement :: Parser Statement
gotoStatement = do
  string "goto"
  remspace
  target <- anyExpr
  char ';'
  remspace
  return (GotoStatement target)

returnStatement :: Parser Statement
returnStatement = do
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
  return (ReturnStatement r)

rvalueStatement :: Parser Statement
rvalueStatement = do
  v <- anyExpr
  char ';'
  remspace
  return (RValueStatement v)

nullStatement :: Parser Statement
nullStatement = do
  char ';'
  remspace
  return NullStatement


-- expression parsers
nameExpr :: Parser Expr
nameExpr = do
  n <- validName
  remspace
  return (NameExpr n)

constExpr :: Parser Expr
constExpr = do
  c <- constant
  return (ConstExpr c)

parenExpr :: Parser Expr
parenExpr = do
  char '('
  remspace
  body <- anyExpr
  char ')'
  remspace
  return (ParenExpr body)

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
  return (\h -> VectorExpr h body)

primaryEtc2 :: Parser (Expr -> Expr)
primaryEtc2 = do
  char '('
  remspace
  args <- anyExpr `sepBy` (char ',' >> remspace)
  char ')'
  remspace
  return (\h -> FunctionExpr h args)
  

prefixOp :: Parser (Expr -> Expr)
prefixOp =
  UnaryExpr LogicNot <$ (char '!' >> remspace) <|>
  UnaryExpr Negative <$ (char '-' >> remspace) <|>
  StarExpr <$ (char '*' >> remspace) <|>
  AmpersandExpr <$ (char '&' >> remspace) <|>
  PreIncDec PlusPlus <$ (string "++" >> remspace) <|>
  PreIncDec MinusMinus <$ (string "--" >> remspace)

postfixOp :: Parser (Expr -> Expr)
postfixOp =
  PostIncDec PlusPlus <$ (string "++" >> remspace) <|>
  PostIncDec MinusMinus <$ (string "--" >> remspace)

unaryExprPre :: Parser Expr
unaryExprPre = do
  hmm <- eitherP prefixOp primaryExpr
  case hmm of 
    Left wrap -> do
      body <- unaryExprPre
      return (wrap body)
    Right base -> return base

unaryExprPost :: Expr -> Parser Expr
unaryExprPost base = do
  wrapMaybe <- optional postfixOp
  case wrapMaybe of
    Nothing -> return base
    Just wrap -> unaryExprPost (wrap base)

unaryExpr :: Parser Expr
unaryExpr = unaryExprPre >>= unaryExprPost

multOp :: Parser (Expr -> Expr -> Expr)
multOp =
  BinaryExpr Modulo   <$ (char '%' >> remspace) <|>
  BinaryExpr Times    <$ (char '*' >> remspace) <|>
  BinaryExpr Division <$ (char '/' >> remspace)

multChain :: Parser Expr
multChain = chainl1 unaryExpr multOp

additiveOp :: Parser (Expr -> Expr -> Expr)
additiveOp = 
  BinaryExpr Plus  <$ (char '+' >> remspace) <|>
  BinaryExpr Minus <$ (char '-' >> remspace)

additiveChain :: Parser Expr
additiveChain = chainl1 multChain additiveOp

shiftOp :: Parser (Expr -> Expr -> Expr)
shiftOp =
  BinaryExpr ShiftL <$ (string "<<" >> remspace) <|>
  BinaryExpr ShiftR <$ (string ">>" >> remspace)

shiftChain :: Parser Expr
shiftChain = chainl1 additiveChain shiftOp

relationalChain :: Parser Expr
relationalChain = chainl1 shiftChain op where
  op =
    BinaryExpr LessThan <$ (char '<' >> remspace) <|>
    BinaryExpr LessThanEquals <$ (string "<=" >> remspace) <|>
    BinaryExpr GreaterThan <$ (char '>' >> remspace) <|>
    BinaryExpr GreaterThanEquals <$ (string ">=" >> remspace)

equalityChain :: Parser Expr
equalityChain = chainl1 relationalChain op where
  op =
    BinaryExpr Equals <$ (string "==" >> remspace) <|>
    BinaryExpr NotEquals <$ (string "!=" >> remspace)


bitOrChain :: Parser Expr
bitOrChain = chainl1 bitAndChain op where
  op = BinaryExpr BitOr <$ (char '|' >> remspace)

bitAndChain :: Parser Expr
bitAndChain = chainl1 equalityChain op where
  op = BinaryExpr BitAnd <$ (char '&' >> remspace)

-- parse an assignment operator, =, =+, etc
assignmentOp :: Parser (Expr -> Expr -> Expr)
assignmentOp = do
  char '='
  op <- optional binaryOp
  remspace
  return (AssignExpr (Assignment op))

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
      return (TernaryExpr a b c)

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
