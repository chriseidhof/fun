module EvalStack where

import AST
import qualified Data.Map as M
import Data.Maybe (fromJust)
import StdLib
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Ops

-- TODO: type-check
-- TODO: lambda-lift 

compile :: Label -> FunctionInfo -> [Definition] -> [Op]
compile freshLabel addresses defs = concat $ evalState (mapM (compileDefinition addresses) defs) freshLabel

data Result = ResultValue Value | ResultBlock Expr
 deriving Show

data DeBruijn = DVal Value
          | DVar Int
          | DLam DeBruijn
          | DApp DeBruijn DeBruijn
          | DIf DeBruijn DeBruijn DeBruijn
          | DBuiltin String Int
          | DCall FunctionLabel Arity
          deriving Show

type FunctionLabel = Int
type Arity = Int

type FunctionInfo = M.Map String AnalysisResult

data AnalysisResult = AnalysisResult { functionLabel :: FunctionLabel, arity :: Arity }
 deriving Show

debruijn :: Expr -> FunctionInfo -> DeBruijn
debruijn e funcs = runReader (debruijn' e) (CompilerState funcs M.empty)


fromJustE :: String -> Maybe a -> a
fromJustE _ (Just x) = x
fromJustE s Nothing = error s

data CompilerState = CompilerState { functionLabels :: FunctionInfo, variableOffsets :: M.Map String Int}

adjustOffsets f (CompilerState l o) = CompilerState l (f o)

debruijn' :: Expr -> Reader CompilerState DeBruijn
debruijn' (Val v) = pure $ DVal v
debruijn' (Var i) = ask >>= \(CompilerState funcs m) -> case M.lookup i m of
                           Just x -> return $ DVar x
                           Nothing -> return $ DCall (functionLabel info) (arity info)
                            where info = fromJustE ("var " ++ i) $ M.lookup i funcs
debruijn' (Lam s e) = DLam <$> local (adjustOffsets (M.insert s 0 . M.map (+1))) (debruijn' e)
debruijn' (App l r) = DApp <$> debruijn' l <*> debruijn' r
debruijn' (If c l r) = DIf <$> debruijn' c <*> debruijn' l <*> debruijn' r
debruijn' (Builtin s i) = return $ DBuiltin s i

type Label = Int

compileDefinition :: FunctionInfo -> Definition -> State Label [Op]
compileDefinition addresses (Definition name e) = do
     code <- compile' (debruijn e addresses) 0
     return $ [Comment name, Label pos] ++ code ++ returns
 where returns = if name == "main" then [] else [FRet]
       pos = functionLabel . fromJust $ M.lookup name addresses

type Offset = Int

freshInt :: State Label Label
freshInt = do
  i <- get
  put (i + 1)
  return i

compile' :: DeBruijn -> Offset -> State Label [Op]
compile' (DVal (IntValue v)) _ = pure [PushInt v]
compile' (DVal (BoolValue b)) _ = pure [PushBool b]
compile' (DVal (StringValue s)) _ = pure [PushString s]
compile' (DVar v) o = pure [Read $ v+o]
compile' (DApp l r) o = (++) <$> compile' r o <*> compile' l (o +1)
compile' (DIf cond l r)  o = do 
  labelL <- freshInt
  labelR <- freshInt
  labelEnd <- freshInt
  c' <- compile' cond (o+2)
  l' <- compile' l o
  r' <- compile' r o
  return $ [PushInt $ toEnum labelL] ++ [PushInt $ toEnum labelR] ++ [Comment $ show o] ++ c' ++ [Branch] ++ [Label labelL] ++ l' ++ [Goto labelEnd] ++ [Label labelR] ++ r' ++ [Label labelEnd]
compile' (DLam e) o = do
   e' <- (compile' e o)
   return $ e' ++ [OpRet]
compile' (DBuiltin s i) o = pure [Call s i]
compile' (DCall i offset) o = pure [Comment $ show o, PushSP offset, Goto i]
