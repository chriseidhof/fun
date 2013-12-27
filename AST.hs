module AST where

data Value = IntValue Integer
           | StringValue String
           | BoolValue Bool
           deriving Show

data Definition = Definition {name :: String, body ::  Expr}
 deriving Show

data Expr = Val Value
          | Var String
          | Lam String Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Builtin String Int
           deriving Show

example = App (App (Lam "x" $ (Lam "y" $ body)) (Val $ IntValue 17)) (Val $ BoolValue True)
 where body = If (op ">" (Var "x") (Val $ IntValue 10)) (Var "x") (op "^" (Var "y") (Var "y"))
       op s l r = (App (App (Builtin s 2) l) r)
