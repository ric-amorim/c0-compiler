module IR where

import AST (BinOp(..)) 
import AST (UnOp(..))

data Instr = MOVE Temp Temp    -- temp1 := temp2
           | MOVEI Temp Int              -- temp1 := num
           | MOVES Temp Label String              -- temp1 := num
           | OP BinOp Temp Temp Temp     -- temp1 := temp2 op temp3
           | OP1 UnOp Temp Temp         -- temp1 := op temp2
           | OPI BinOp Temp Temp Int     -- temp1 := temp2 op num
           | LABEL Label
           | LABELF Label
           | JUMP Label
           | COND Temp BinOp Temp Label Label
           | CONDF Temp BinOp Temp Label Label
           | COND1 Temp Label Label
           | COND1F Temp Label Label
           | CALL Temp Temp [Temp]
           | RETURNV Temp
           | RETURN
           deriving (Eq,Show)

type Temp = String 
type Label = String
type Arg   = String 
type Saved = String



