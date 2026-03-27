--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) (\v -> k (n*v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] a b =
    if even x then a x
    else b x
evenoddk (x:xs) a b = 
    if even x then evenoddk xs (\e -> a (x+e)) b
    else evenoddk xs a (\o -> b (x+o))


--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (AppExp _ _) = False
isSimple (IntExp _) = True
isSimple (VarExp _) = True

isSimple (OpExp _ a b) = isSimple a && isSimple b
isSimple (IfExp a b c) = isSimple a && isSimple b && isSimple c

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
-- cpsExp = undefined

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp a b) k n = 
    if isSimple b
        then (AppExp (AppExp a b) k, n)
    else
        let (v, n1) = gensym n
            newC = LamExp v (AppExp (AppExp a (VarExp v)) k)
        in cpsExp b newC n1
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op a b) k n 
    | isSimple a && isSimple b = (AppExp k (OpExp op a b), n)
    | isSimple a = 
        let (v, n1) = gensym n
            newC = LamExp v (AppExp k (OpExp op a (VarExp v)))
        in cpsExp b newC n1
    | isSimple b = 
        let (v, n1) = gensym n
            newC = LamExp v (AppExp k (OpExp op (VarExp v) b))
        in cpsExp a newC n1
    | otherwise = 
        let (v1, n1) = gensym n
            (v2, n2) = gensym n1
            new_b = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
            (cpsB, n3) = cpsExp b new_b n2
            new_a = LamExp v1 cpsB
        in cpsExp a new_a n3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp a b c) k n =
    if isSimple a then
        let (cpsB, n1) = cpsExp b k n
            (cpsC, n2) = cpsExp c k n1
        in (IfExp a cpsB cpsC, n2)
    else 
        let (v, n1) = gensym n
            (cpsB, n2) = cpsExp b k n1
            (cpsC, n3) = cpsExp c k n2
            newC = LamExp v (IfExp (VarExp v) cpsB cpsC)
        in cpsExp a newC n3
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl name params body) = 
    let
        newParams = params ++ ["k"]
        (cpsBody, _) = cpsExp body (VarExp "k") 1
    in Decl name newParams cpsBody
