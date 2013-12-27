module Ops (Op (..)) where

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
        | PushSP Int

instance Show Op where
  show = ppOp

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
ppOp (PushSP o) = "pushsp " ++ show o
