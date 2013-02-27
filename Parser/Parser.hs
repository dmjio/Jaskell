module Parser where

import Decs

import System.IO
import Control.Monad
import Control.Applicative hiding ((<|>),many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.nestedComments = True
           , Token.identStart      = letter <|> char '_'
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = keys
           , Token.reservedOpNames = ops
           }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

reserved' r = (*>) (reserved r) (pure r)

jackParser :: Parser Jack
jackParser = whiteSpace >> classParser

classParser :: Parser Jack
classParser = do
  reserved "class"
  name <- identifier
  char '{'
  whiteSpace
  decs <- many oParser
  whiteSpace
  char '}'
  return $ Class name decs

oParser :: Parser Declaration
oParser = do
  try classVarDecs <|> try subDec

commaSep p  =  p `sepBy` (char ',' >> whiteSpace)

classVarDecs :: Parser Declaration
classVarDecs = do
  whiteSpace
  keyword <- reserved' "static" <|> reserved' "field"
  typ <- typeDecs
  name <- commaSep identifier
  semi
  return $ ClassVarDec keyword typ name

typeDecs :: Parser Type
typeDecs = do
  TypeKey <$> (reserved' "int" <|>
             reserved' "char" <|>
             reserved' "boolean" <|>
             reserved' "void")
    <|>
    TypeClass <$> try identifier

subDec :: Parser Declaration
subDec = do
   whiteSpace
   keyword <- (reserved' "constructor" <|> reserved' "function" <|> reserved' "method")
   typ <- typeDecs
   name <- identifier
   char '('
   list <- commaSep paramList
   char ')'
   body <- subBody
   return $ SubDec keyword typ name list body

paramList :: Parser Params
paramList = do
  whiteSpace
  typ <- typeDecs
  val <- identifier
  return $ Params (typ, val)

subBodyOr :: Parser SubBody
subBodyOr = do
  try (SubBodyVar <$> many varDecs)
 <|>
  try (SubBodyStatement <$> many statementsParser)

subBodyBoth :: Parser SubBody
subBodyBoth = do
  vd <- many varDecs
  statements <- many statementsParser
  return $ SubBody vd statements

subBody :: Parser SubBody
subBody = do
  whiteSpace
  char '{'
  sub <- try subBodyBoth <|> try subBodyOr
  char '}'
  return sub

varDecs :: Parser VarDec
varDecs = do
  whiteSpace
  var <- reserved' "var"
  typ <- typeDecs
  names <- commaSep identifier
  semi
  return $ VarDec typ names

statementsParser :: Parser Statement
statementsParser = do
  try subLetParser <|>  try letParser <|> try whileParser <|>
    try ifParser <|> try ifelseParser <|>
    try doParser <|> (try returnExpParser <|> try returnParser)

doParser :: Parser Statement
doParser = do
  whiteSpace
  d <- reserved' "do"
  subR <- subCallParser
  semi
  return $ Do d subR

returnParser :: Parser Statement
returnParser = do
  whiteSpace
  ret <- reserved' "return"
  semi
  return $ Return ret

returnExpParser :: Parser Statement
returnExpParser = do
  whiteSpace
  ret <- reserved' "return"
  expr <- getExprParser
  semi
  return $ ReturnExp ret expr

whileParser :: Parser Statement
whileParser = do
  whiteSpace
  while <- reserved' "while"
  char '('
  expr <- getExprParser
  char ')'
  whiteSpace
  char '{'
  whiteSpace
  stmt <- many statementsParser
  whiteSpace
  char '}'
  return $ While while expr stmt

ifParser :: Parser Statement
ifParser = do
  whiteSpace
  iff <- reserved' "if"
  char '('
  expr <- getExprParser
  char ')'
  whiteSpace
  char '{'
  stmt <- many statementsParser
  char '}'
  return $ If iff expr stmt

ifelseParser :: Parser Statement
ifelseParser = do
  whiteSpace
  iff <- reserved' "if"
  char '('
  expr <- getExprParser
  char ')'
  whiteSpace
  char '{'
  stmt <- many statementsParser
  char '}'
  el <- reserved' "else"
  char '{'
  stmt <- many1 statementsParser
  char '}'
  return $ IfElse iff expr stmt el stmt

--statement parsers
letParser :: Parser Statement
letParser = do
  whiteSpace
  l <- reserved' "let"
  var <- identifier
  char '='
  expr <- getExprParser
  semi
  return $ Let l var expr

subLetParser :: Parser Statement
subLetParser = do
  whiteSpace
  l <- reserved' "let"
  var <- identifier
  char '['
  expr <- getExprParser
  char ']'
  whiteSpace
  char '='
  whiteSpace
  expr2 <- getExprParser
  whiteSpace
  semi
  return $ SubLet l var expr expr2

subCallParser :: Parser SubCall
subCallParser = do
  whiteSpace
  try subParser <|> try classVarParser

subParser :: Parser SubCall
subParser = do
  whiteSpace
  subName <- identifier
  char '('
  expr <- commaSep getExprParser
  char ')'
  return $ SubCaller subName expr

classVarParser :: Parser SubCall
classVarParser = do
  whiteSpace
  classVarName <- identifier
  char '.'
  subName <- identifier
  char '('
  expr <- commaSep getExprParser
  char ')'
  return $ ClassCall classVarName subName expr

subRoutineParser :: Parser Term
subRoutineParser = do
  whiteSpace
  sub <- subCallParser
  return $ Subroutine sub


getExprParser :: Parser Expr
getExprParser = do
  whiteSpace
  term <- (try subRoutineParser <|>
                 try intVal <|>
                 try stringVal <|>
                 try keyWordVal <|>
                 try varExpParser <|>
                 try varVal <|>
                 try simpleExpr <|>
                 try unaryOpExpr)
  op <- many getExpP
  return $ ExprOpTerm term op

getExpP :: Parser (String,Term)
getExpP = do
  whiteSpace
  op <- choice $ map string ops
  term <- (try intVal <|>
          try stringVal <|>
          try keyWordVal <|>
          try varExpParser <|>
          try varVal <|>
          try subRoutineParser <|>
          try simpleExpr <|>
          try unaryOpExpr)
  return $ (op,term)

simpleExpr :: Parser Term
simpleExpr = do
  whiteSpace
  char '('
  expr <- getExprParser
  char ')'
  return $ SimpleExpr expr

varVal :: Parser Term
varVal = do
  whiteSpace
  var <- identifier
  return $ VarTerm var

unaryOpExpr :: Parser Term
unaryOpExpr = do
  whiteSpace
  op <- choice $ map string unaryops
  term <- getExprParser
  return $ Unary op term

varExpParser :: Parser Term
varExpParser = do
  whiteSpace
  varName <- identifier
  char '['
  expr <- getExprParser
  char ']'
  return $ VarExpr varName expr

keyWordVal :: Parser Term
keyWordVal = do
  whiteSpace
  try aKeys <|> try oKeys <|> try dKeys

symbols :: Parser Term
symbols = do
  whiteSpace
  op <- choice $ map string ops
  return $ Symbol op


aKeys :: Parser Term
aKeys = do
  whiteSpace
  a <- choice $ map string akeys
  return $ KeywordTerm a

dKeys :: Parser Term
dKeys = do
  whiteSpace
  a <- choice $ map string dkeys
  return $ KeywordTerm a

oKeys :: Parser Term
oKeys = do
  whiteSpace
  a <- choice $ map string okeys
  return $ KeywordTerm a

intVal :: Parser Term
intVal = do
  whiteSpace
  x <- integer
  return $ IntConst x

symbolVal :: Parser Term
symbolVal = do
  whiteSpace
  x <- choice $ map string ops
  return $ Symbol x

stringVal :: Parser Term
stringVal = do
  whiteSpace
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ StringConst x

identVal :: Parser Term
identVal = do
  whiteSpace
  atom <- identifier
  return $ VarTerm atom

parseString :: String -> Jack
parseString str =
   case parse jackParser "" str of
     Left e  -> error $ show e
     Right r -> r

main :: IO ()
main = do
  putStrLn "Please enter the name of your jack file (i.e. Main)"
  fileName <- getLine
  file <- readFile (fileName++".jack")
  let ast = parseString file
  putStrLn $ show ast

parseFile :: String -> IO Jack
parseFile file =
   do program  <- readFile file
      case parse jackParser "" program of
        Left e  -> print e >> fail "parse error"
        Right r -> return r
