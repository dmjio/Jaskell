module SymbolTable where

import Control.Applicative
import Debug.Trace
import Parser (parseString)
import Data.Maybe
import Decs 

type Name = String
type Kind = String

getClassMap = parseClassTable 
getSubDecMap = parseSubDecTable 

parseClassTable ast = case ast of 
  Class ident decs -> parseDecs decs 0 0 where
    parseDecs [] _ _  = []
    parseDecs (x:xs) numStatic numField = parseCVD x numStatic numField where
      parseCVD (ClassVarDec k typ []) numStatic numField = parseDecs xs numStatic numField
      parseCVD (ClassVarDec k typ (n:ns)) numStatic numField  = 
        case k of
          "field" -> [(n, (typ, "field", numField))] ++ parseCVD (ClassVarDec k typ ns) numStatic (numField+1)
          "static" -> [(n, (typ, "static", numStatic))] ++ parseCVD (ClassVarDec k typ ns) (numStatic+1) numField
      parseCVD _ _ _ = []

parseSubDecTable ast = case ast of 
  Class ident decs -> parseDecs decs where
    parseDecs [] = []
    parseDecs decs = parseSVD decs where
      parseSVD ((SubDec k typ name params body):xs) = 
        addParamsAndBody name k params body typ ident ++ parseDecs xs
      parseSVD (_:xs) = parseDecs xs

buildFuncMap ast = case ast of
  Class ident decs -> parseDecs decs where
    parseDecs [] = []
    parseDecs decs = parseSVD decs where
      parseSVD [] = [] 
      parseSVD ((SubDec kind typ name params body):xs) = (name, kind) : parseSVD xs

lastTwo (a,b,c) = (b,c)

getSymNum :: String -> String -> Jack -> (String, Int)
getSymNum val func ast = case lookup val (getClassMap ast) of
  Just x -> lastTwo x
  Nothing -> case lookup func (getSubDecMap ast) of
    Just x -> case lookup val x of
      Just x -> lastTwo x

getFuncType funcName ast = let result = lookup funcName (buildFuncMap ast)
                           in fromJust result

addParamsAndBody name key params body typ ident = [(name, ((addParams key params 0 typ ident) 
                                                           ++ (makeBody body 0)))]
addParams _ [] _ _ _ = []
addParams key ps num typ className = case key of 
  "method" -> [("this", ((TypeClass className), "argument", num))] ++ addP "argument" (num+1) ps
  otherwise -> addP "argument" num ps 

addP kind num [] = []
addP kind num (p:ps) = ap p kind num where 
        ap (Params (typ,varname)) kind num  = [(varname, (typ, kind, num))] ++ addP kind (num+1) ps

makeBody (SubBody [] _) _ = []
makeBody (SubBodyVar []) _  = []
makeBody (SubBodyVar vars) num = mkV vars num 
makeBody (SubBody vars _) num = mkV vars num 
makeBody (NoSubBody) _= []
makeBody (SubBodyStatement _) _ = []
      
mkV :: [VarDec] -> Int -> [(String, (Type, String, Int))]
mkV [] num = []
mkV (x:xs) num = parseDec x num ++ mkV xs (length $ parseDec x num)

parseDec :: VarDec -> Int -> [(String, (Type, String, Int))]
parseDec (VarDec typ []) _ = []  
parseDec (VarDec typ names) num = addNames names typ num 

addNames :: [String] -> Type -> Int -> [(String, (Type, String, Int))]
addNames [] _ _ = []
addNames (name:names) typ num = [(name, (typ, "local", num))] ++ addNames names typ (num+1)

-- mkVarDec (VarDec typ []) _ = []
-- mkVarDec (VarDec typ (name:names)) num = 
--   [(name, (typ, "local", num))] ++ mkVarDec (VarDec typ names) (num+1)                

getSubInfo funcName ast pred = let result = lookup funcName $ getSubDecMap ast
                               in length $ filter pred $ fromJust result

getFieldCount funcName ast = getSubInfo funcName ast (\(_,(_,arg,_)) -> arg == "local") 
getParamCount funcName ast = getSubInfo funcName ast (\(_,(_,arg,_)) -> arg == "argument")

getFuncReturnTypeList ast = case ast of 
  Class ident decs -> getd decs

getClassName ast = case ast of
  Class ident decs -> ident

getd [] = []
getd (x:xs) = getDec x ++ getd xs where
  getDec (SubDec key typ subname params body) = [(subname, getBody body)] where
    getBody bod = case bod of
      (SubBodyStatement ss) -> any isReturn ss
      (SubBody _ kk) -> any isReturn kk
  getDec _ = []

isReturn :: Statement -> Bool
isReturn x = case x of
  ReturnExp _ _ -> True
  otherwise -> False



