module AST where

data Value = IntValue Int
           | StringValue String
           | BoolValue Bool
           deriving Show

data Expr = Val Value
          | Var String
          | Lam String Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Builtin String Int
           deriving Show


{-
example = App (App (Lam "x" $ (Lam "y" $ body)) (Val $ IntValue 17)) (Val $ BoolValue True)
 where body = If (Var "x") (Var "x") (Var "y")
 -}

example = App (App (Lam "x" $ (Lam "y" $ body)) (Val $ IntValue 17)) (Val $ BoolValue True)
 -- where body = If (Var "x") (Var "x") (App (App (Builtin "add" 2) (Var "y")) (Var "y"))
 where body = If (op ">" (Var "x") (Val $ IntValue 10)) (Var "x") (op "^" (Var "y") (Var "y"))
       op s l r = (App (App (Builtin s 2) l) r)


{-
example = body 17 True
 where body x y = if x then x else (add y x)
-}


