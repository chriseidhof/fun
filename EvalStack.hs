module EvalStack where

import AST
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Result = ResultValue Value | ResultBlock Expr
 deriving Show

data Op = PushInt Int
        | PushBool Bool
        | PushString String
        | Pop
        | OpRet
        | Branch
        | Read Int 
        | Call String
          deriving Show


ppOp :: Op -> String
ppOp (PushInt x) = "int " ++ show x
ppOp (PushBool b) = "bool " ++ if b then "1" else "0"
ppOp (PushString s) = "string "++ show s
ppOp Pop = "pop"
ppOp OpRet = "ret"
ppOp Branch = "branch"
ppOp (Read i) = "read " ++ show i
ppOp (Call b) = "call " ++ b 

data DeBruijn = DVal Value
          | DVar Int
          | DLam DeBruijn
          | DApp DeBruijn DeBruijn
          | DIf DeBruijn DeBruijn DeBruijn
          | DBuiltin String
          deriving Show

debruijn :: Expr -> DeBruijn
debruijn e = debruijn' e M.empty

-- todo: offset should go to compile
debruijn' :: Expr -> M.Map String Int -> DeBruijn
debruijn' (Val v) _ = DVal v
debruijn' (Var i) m = DVar $ (fromJust $ M.lookup i m)
debruijn' (Lam s e) m = DLam $ debruijn' e (M.insert s 0 $ M.map (+1) m)
debruijn' (App l r) m = DApp (debruijn' l m) (debruijn' r m)
debruijn' (If c l r) m = DIf (debruijn' c  m) (debruijn' l  m) (debruijn' r  m)
debruijn' (Builtin s _) m = DBuiltin s

compile :: DeBruijn -> [Op]
compile e = compile' e 0

compile' :: DeBruijn -> Int -> [Op]
compile' (DVal (IntValue v)) _ = [PushInt v]
compile' (DVal (BoolValue b)) _ = [PushBool b]
compile' (DVal (StringValue s)) _ = [PushString s]
compile' (DVar v) o = [Read $ v+o]
compile' (DApp l r) o = (compile' r o) ++ (compile' l $ o +1)
compile' (DIf cond l r)  o = (compile' l o) ++ (compile' r $ o + 1) ++ (compile' cond $ o + 2) ++ [Branch] -- todo: this is not a lazy if
compile' (DLam e) o = (compile' e (o-1)) ++ [OpRet]
compile' (DBuiltin s) o = [Call s]



{-
evalInContext (Val v) _ = ResultValue v
evalInContext (Var v) (Context c) = fromJust $ M.lookup v c
evalInContext (App (Lam s e) r) c = evalInContext e (assign s (evalInContext r c) c)
evalInContext (If cond l r) c = case evalInContext cond c of
                                 (ResultValue (BoolValue True)) -> evalInContext l c
                                 _                -> evalInContext r c
evalInContext l@(Lam x e) (Context c) = ResultBlock (Lam x $ simplify x e c)
evalInContext e _ = error $ "Can't eval " ++ show e
-}
