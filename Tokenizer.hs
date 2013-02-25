module Main where

import System.Environment
import Data.Char
import Data.List
import Control.Monad
import Control.Applicative hiding ((<|>),many)
import Text.ParserCombinators.Parsec hiding (oneOf)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

keys = ["class", "function", "method", "static", "var", "boolean", "null", "this", "let", "do", "if", "else", "while", "return"]
okeys = ["constructor", "field", "void", "int", "true"]
dkeys = ["char", "false"]
ops =  ["{", "+", ".", "}",",", "(", ")", "[","]", "-", "*", "/", "&", "|", "&", "<", ">", "=", "~" , ";"]

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
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer

data Tkens = StringConst String
              | IntConst Integer
              | Symbol String
              | Keyword String
              | Identifier String deriving (Show)

tkens :: Parser [Tkens]
tkens = do
  results <- many1 (try symbolVal <|>
                    try oKeyVal <|> try dKeyVal <|> try keyWordVal <|>
                    try stringVal <|>
                    try identVal <|>
                    try intVal)
  return results

intVal :: Parser Tkens
intVal = do
  whiteSpace
  x <- integer
  return $ IntConst x

symbolVal :: Parser Tkens
symbolVal = do
  whiteSpace
  x <- choice $ map string ops
  return $ Symbol x

symNOWSVal :: Parser Tkens
symNOWSVal = do
  whiteSpace
  x <- choice $ map string ops
  return $ Symbol x

stringVal :: Parser Tkens
stringVal = do
  whiteSpace
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ StringConst x

identVal :: Parser Tkens
identVal = do
  whiteSpace
  atom <- identifier
  return $ Identifier atom

dKeyVal :: Parser Tkens
dKeyVal = do
  whiteSpace
  x <- choice $ map string dkeys
  return $ Keyword x

oKeyVal :: Parser Tkens
oKeyVal = do
  whiteSpace
  x <- choice $ map string okeys
  return $ Keyword x

keyWordVal :: Parser Tkens
keyWordVal = do
  whiteSpace
  x <- choice $ map string keys
  return $ Keyword x

parseString :: Parser [Tkens] -> String -> [Tkens]
parseString p str =
     case parse p "" str of
       Left e  -> error $ show e
       Right r -> r

main :: IO ()
main = do
  putStrLn "Please enter the name of your jack file (i.e. Main)"
  fileName <- getLine
  file <- readFile (fileName++".jack")
  let ast = parseString tkens (file)
  writeFile (fileName++"T.xml") $ ("<tokens>\n" ++ (conAST ast) ++ "</tokens>\n")
  putStrLn $  "Completed, " ++ (fileName ++ "T.xml created...")

conAST :: [Tkens] -> String
conAST [] = ""
conAST (k:xs) =
  case k of
    (Identifier k) -> "<identifier> " ++ k  ++ " </identifier>\n" ++ conAST xs
    (Keyword k) -> "<keyword> " ++ k ++ " </keyword>\n" ++ conAST xs
    (Symbol k) -> "<symbol> " ++ (check k) ++ " </symbol>\n" ++ conAST xs
    (StringConst k) -> "<stringConstant> " ++ k ++ " </stringConstant>\n" ++ conAST xs
    (IntConst k) -> "<integerConstant> " ++ show k ++ " </integerConstant>\n" ++ conAST xs

check :: String -> String
check k
  | k == "<" = "&lt;"
  | k == ">" = "&gt;"
  | k == "&" = "&amp;"
  | otherwise =  k
