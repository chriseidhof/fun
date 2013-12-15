module EvalStack where

import AST
import qualified Data.Map as M
import Data.Maybe (fromJust)
import StdLib
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

data Result = ResultValue Value | ResultBlock Expr
 deriving Show

data Op = PushInt Integer
        | PushBool Bool
        | PushString String
        | Pop
        | OpRet
        | Branch
        | Read Int 
        | Call String Int
        | Label Int
        | Comment String
        | Goto Int
        | FRet
          deriving Show


ppOp :: Op -> String
ppOp (PushInt x) = "int " ++ show x
ppOp (PushBool b) = "bool " ++ if b then "1" else "0"
ppOp (PushString s) = "string "++ show s
ppOp Pop = "pop"
ppOp OpRet = "ret"
ppOp Branch = "branch"
ppOp (Read i) = "read " ++ show i
ppOp (Call b i) = unwords ["call", b, show i]
ppOp (Label b) = "label " ++ show b 
ppOp (Comment c) = "// " ++ c
ppOp (Goto i) = "goto " ++ show i
ppOp (FRet) = "fret"

data DeBruijn = DVal Value
          | DVar Int
          | DLam DeBruijn
          | DApp DeBruijn DeBruijn
          | DIf DeBruijn DeBruijn DeBruijn
          | DBuiltin String Int
          | DGoto Int
          deriving Show



debruijn :: Expr -> M.Map String Int -> DeBruijn
debruijn e funcs = runReader (debruijn' e) (CompilerState funcs M.empty)


fromJustE :: String -> Maybe a -> a
fromJustE _ (Just x) = x
fromJustE s Nothing = error s

data CompilerState = CompilerState { functionLabels :: M.Map String Int, variableOffsets :: M.Map String Int }

adjustOffsets f (CompilerState l o) = CompilerState l (f o)

debruijn' :: Expr -> Reader CompilerState DeBruijn
debruijn' (Val v) = pure $ DVal v
debruijn' (Var i) = ask >>= \(CompilerState funcs m) -> case M.lookup i m of
                           Just x -> return $ DVar x
                           Nothing -> return $ DGoto $ fromJustE ("var " ++ i) $ M.lookup i funcs
debruijn' (Lam s e) = DLam <$> local (adjustOffsets (M.insert s 0 . M.map (+1))) (debruijn' e)
debruijn' (App l r) = DApp <$> debruijn' l <*> debruijn' r
debruijn' (If c l r) = DIf <$> debruijn' c <*> debruijn' l <*> debruijn' r
debruijn' (Builtin s i) = return $ DBuiltin s i

type Label = Int
type Addresses = M.Map String Int

compile :: Label -> Addresses -> [Definition] -> [Op]
compile freshLabel addresses defs = concat $ evalState (mapM (compileDefinition addresses) defs) 0

compileDefinition :: Addresses -> Definition -> State Label [Op]
compileDefinition addresses (Definition name e) = do
     code <- compile' (debruijn e addresses) 0
     return $ [Comment name, Label pos] ++ code ++ returns
 where returns = if name == "main" then [] else [FRet]
       pos = fromJust $ M.lookup name addresses

type Offset = Int

compile' :: DeBruijn -> Offset -> State Label [Op]
compile' (DVal (IntValue v)) _ = pure [PushInt v]
compile' (DVal (BoolValue b)) _ = pure [PushBool b]
compile' (DVal (StringValue s)) _ = pure [PushString s]
compile' (DVar v) o = pure [Read $ v+o]
compile' (DApp l r) o = (++) <$> compile' r o <*> compile' l (o +1)
compile' (DIf cond l r)  o = do 
  l' <- compile' l o
  r' <- compile' r $ o + 1
  c' <- compile' cond $ o + 2
  return $ l' ++ r' ++ c' ++ [Branch] -- todo: this is not a lazy if
compile' (DLam e) o = do
   e' <- (compile' e (o-1))
   return $ e' ++ [OpRet]
compile' (DBuiltin s i) o = pure [Call s i]
compile' (DGoto i) o = pure [Goto i]



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
