module Decs where

type Identifier = String
type Keyword = String

type ClassName = String
type SubName = String
type VarName = String

--program structure
data Jack = Class Identifier [Declaration] deriving (Show)

data Declaration = ClassVarDec Keyword Type [VarName] |
                   SubDec Keyword Type SubName [Params] SubBody
                   deriving (Show)

data Type = TypeKey Keyword | TypeClass ClassName deriving (Show)

data Params = Params (Type, VarName) deriving (Show)

--Statements
data Statement = Let Keyword VarName Expr
               | SubLet Keyword VarName Expr Expr
               | If Keyword Expr [Statement]
               | IfElse Keyword Expr [Statement] Keyword [Statement]
               | While Keyword Expr [Statement]
               | Do Keyword SubCall
               | ReturnExp Keyword Expr
               | NoReturn
               | Return Keyword deriving (Show)

data VarDec = VarDec Type [VarName] deriving (Show)

data SubBody = SubBodyVar [VarDec]
               | SubBodyStatement [Statement]
               | SubBody [VarDec] [Statement]
               | NoSubBody deriving (Show)

data Expr = ExprOpTerm Term [(Keyword, Term)] deriving (Show)

data Term = IntConst Integer
            | StringConst String
            | KeywordTerm Keyword
            | VarTerm VarName
            | VarExpr VarName Expr
            | SimpleExpr Expr
            | Unary String Expr
            | UnaryTerm String Term
            | Subroutine SubCall
            | Symbol String deriving (Show)

data Op = Add | Subtract | Multiply | Divide | And | Or | Less | Greater | Equals | NullOp | Dot
              deriving (Show)

data UnaryOp = Neg | Negate | NullUnaryOp deriving (Show)

data SubCall = SubCaller SubName [Expr] |
               ClassCall ClassName SubName [Expr] |
               VarCall VarName SubName [Expr] deriving (Show)

keys = ["class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return"]

ops =  ["+", "-", "*", "/", "&", "|", "&", "<", ">", "=", "." ]

unaryops = ["~", "-"]

getOp :: String -> Op
getOp x = case x of
  "+" -> Add
  "-" -> Subtract
  "*" -> Multiply
  "/" -> Divide
  "&" -> And
  "|" -> Or
  "<" -> Less
  ">" -> Greater
  "=" -> Equals
  "." -> Dot
  otherwise -> NullOp

getUnaryOp :: String -> UnaryOp
getUnaryOp x = case x of
  "~" -> Negate
  "-" -> Neg
  otherwise -> NullUnaryOp

akeys = ["class", "function", "method", "static", "var", "boolean", "null", "this", "let", "do", "if", "else", "while", "return"]
okeys = ["constructor", "field", "void", "int", "true"]
dkeys = ["char", "false"]
