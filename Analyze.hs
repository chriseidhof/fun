module Analyze where

import AST

analyzeArity :: Expr -> Int
analyzeArity (Lam _ l) = 1 + analyzeArity l
analyzeArity _         = 0
