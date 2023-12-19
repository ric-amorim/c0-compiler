module AST where 

import Lexer

type Decl = (Type,String)

data Fn = Func Type String [(Type,String)] [Stm] 
          deriving Show

data Type = Int 
          | Bool
          | String
          | TyFun [Type] Type
          deriving (Eq,Show)


data Stm = PrintInt Exp
         | PrintStr Exp
         | While Exp Stm
         | For [Simple] Exp [Simple] Stm
         | If Exp Stm
         | IfElse Exp Stm Stm
         | Ret (Maybe Exp)
         | Continue
         | Break 
         | Block [Stm]
         | Simple Simple
        deriving Show

data Simple = DefVar Type String (Maybe Exp)
            | Assign String Exp
            | Incr String
            | Decr String
            | Expression Exp
            deriving Show

data Exp = Num Int 
         | Ident String
         | BoolVal Bool
         | StringVal String
         | FnCall String [Exp]
         | ScanInt
         | Op BinOp Exp Exp 
         | Op1 UnOp Exp
         deriving (Eq, Show)

data BinOp = Add | Sub | Mult | Div | Mod | And | Or | Lt | Gt | Eq | Le | Ge | Ne  
         deriving (Eq, Show)

data UnOp = Neg | Not
          deriving (Eq, Show)



