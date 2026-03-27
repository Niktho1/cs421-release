module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = case H.lookup s env of
    Just v -> v
    Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case H.lookup op intOps of
        Nothing -> ExnVal "Unknown Op"
        Just o -> case (op, v1, v2) of
            ("/", _, IntVal 0) -> ExnVal "Division by 0"
            _                  -> liftIntOp o v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case H.lookup op boolOps of
        Nothing -> ExnVal "Unknown Op"
        Just o -> liftBoolOp o v1 v2

eval (CompOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case H.lookup op compOps of
        Nothing -> ExnVal "Unknown Op"
        Just o -> liftCompOp o v1 v2

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    let v1 = eval e1 env
    in case v1 of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env =
    case eval e1 env of
        CloVal params body cEnv ->
            let argV = map (`eval` env) args
            in case [s | ExnVal s <- argV] of
                (s:_) -> ExnVal s
                [] ->
                    let binds = H.fromList (zip params argV)
                        newEnv = H.union binds cEnv
                    in eval body newEnv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = 
    let (vars, exp) = unzip pairs
        argV = map (`eval` env) exp
    in case [s | ExnVal s <- argV] of 
        (s:_) -> ExnVal s
        [] -> 
            let binds = H.fromList (zip vars argV)
                newEnv = H.union binds env
            in eval body newEnv

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = 
    let v = eval e env
        newEnv = H.insert var v env
    in ("", penv, newEnv)

--- ### Sequencing

exec (SeqStmt []) penv env = ([], penv, env)
exec (SeqStmt (s:ss)) penv env = 
    let (out1, penv1, env1) = exec s penv env
        (out2, penv2, env2) = exec (SeqStmt ss) penv1 env1
    in (out1++out2, penv2, env2)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    case eval e1 env of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = 
    let newPenv = H.insert name p penv
    in ("", newPenv, env)

exec (CallStmt name args) penv env = 
    case H.lookup name penv of
        Just (ProcedureStmt _ pArgs body) -> 
            let argV = map (`eval` env) args
                binds = H.fromList (zip pArgs argV)
                tempEnv = H.union binds env
                (out, newPenv, newEnv) = exec body penv tempEnv
            in (out, newPenv, newEnv)
        Nothing -> ("exn: Error", penv, env)
        
