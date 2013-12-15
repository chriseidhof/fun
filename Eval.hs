module Eval where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import AST

data Context = Context (M.Map String Result)
 deriving Show

emptyContext = Context M.empty

eval :: Expr -> Result
eval e = evalInContext e emptyContext 

assign :: String -> Result -> Context -> Context
assign s v (Context c) = Context $ M.insert s v c

data Result = ResultValue Value | ResultBlock Expr
 deriving Show

evalInContext :: Expr -> Context -> Result
evalInContext (Val v) _ = ResultValue v
evalInContext (Var v) (Context c) = fromJust $ M.lookup v c
evalInContext (App (Lam s e) r) c = evalInContext e (assign s (evalInContext r c) c)
evalInContext (If cond l r) c = case evalInContext cond c of
                                 (ResultValue (BoolValue True)) -> evalInContext l c
                                 _                -> evalInContext r c
evalInContext l@(Lam x e) (Context c) = ResultBlock (Lam x $ simplify x e c)
evalInContext e _ = error $ "Can't eval " ++ show e

simplify x e c = case (evalInContext e $ Context $ M.delete x c) of
  ResultValue v -> Val v
  ResultBlock e -> e


--Val Value
--Var String
--Lam String Expr
--App Expr Expr
--If Expr Expr Expr
