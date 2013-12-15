module StdLib where

import AST
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Data.Maybe (maybe)

type StdLib = M.Map String Expr

stdlib :: StdLib
stdlib = M.fromList [("add", Builtin "+" 2) 
                    ,("mul", Builtin "*" 2)
                    ,("min", Builtin "-" 2)
                    ,("eq", Builtin "==" 2)
                    ]

bindStdLib :: Definition -> Definition
bindStdLib (Definition n e) = Definition n $ runReader (f e) stdlib
 where f :: Expr -> Reader StdLib Expr
       f v@(Var s) = maybe v id . M.lookup s <$> ask
       f v@(Val _) = pure v
       f (Lam s e) = Lam s <$> local (M.delete s) (f e)
       f (App l r) = App <$> f l <*> f r
       f (If c l r) = If <$> f c <*> f l <*> f r
       f b@(Builtin _ _) = pure b


{-
data Expr = Val Value
          | Var String
          | Lam String Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Builtin String Int
-}
