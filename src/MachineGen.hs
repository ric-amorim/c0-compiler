module MachineGen where 

import CodeGen
import IR
import AST

import Control.Monad.State (State)
import qualified Control.Monad.State as State

createArg :: [Temp] -> Int-> String
createArg [] _= ""
createArg (x:xs) n = "\tmove $a"++show n++", $"++x++"\n"++createArg xs (n+1)

opDecode :: Instr -> String 
opDecode (OP op t1 t2 t3) 
   = case op of
     Add -> "\tadd $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"  
     Sub -> "\tsub $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n" 
     Mult -> "\tmul $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Div -> "\tdiv $"++t2++", $"++t3++"\n\tmflo $"++t1++"\n"
     Mod -> "\tdiv $"++t2++", $"++t3++"\n\tmfhi $" ++t1++"\n"
     And -> "\tand $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Or -> "\tor $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Lt -> "\tslt $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Gt -> "\tsgt $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Eq -> "\tseq $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Le -> "\tsle $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Ge -> "\tsge $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"
     Ne -> "\tsne $" ++t1++ ", $" ++t2++ ", $" ++t3++ "\n"

opDecode (OP1 op t1 t2) 
   = case op of 
      Not -> "\tnor $" ++t1++ ", $zero ,$" ++t2++"\n" 
      Neg -> "\tsub $" ++t1++ ", $zero ,$" ++t2++"\n"

opDecode (OPI op t1 t2 n)
   = case op of 
      Add -> "\taddi $" ++t1++ ", $" ++t2++ ", " ++show n++"\n"

callDecode :: Instr -> String 
callDecode (CALL t1 "scanInt" []) ="\tli $v0, 5\n\tsyscall\n\tmove $"++t1++", $v0\n"
callDecode (CALL t1 "print_int" [exp]) = "\tli $v0, 1\n\tadd $a0, $"++exp++", $zero\n"++
                                         "\tsyscall\n"
callDecode (CALL t1 "print_str" [exp]) = "\tli $v0, 4\n\tadd $a0, $"++exp++", $zero\n"++
                                         "\tsyscall\n"
callDecode (CALL t1 str exp)  = saveStack exp 4 ++createArg exp 0++ "\tjal "++str++"\n"++
                                        loadStack exp 4++ "\tmove $"++t1++", $v0\n"

saveStack :: [Temp]-> Int -> String
saveStack [] n = "" 
saveStack (x:xs) n 
 | n == 4  = "\taddiu $sp, $sp, "++(show (0-n))++"\n\tsw $"++ x++", "++(show n)++
             "($sp)\n" ++ saveStack xs (n+4)
 | otherwise = "\tsw $"++ x++", "++(show n)++"($sp)\n" ++ saveStack xs (n+4)

loadStack :: [Temp] -> Int -> String 
loadStack [] n =  "\taddiu $sp, $sp, "++(show (n-4))++"\n"
loadStack (x:xs) n = "\tlw $" ++x++ ", "++(show n)++"($sp)\n" 
                     ++ loadStack xs (n+4)


prelude :: String
prelude = "\tsw $fp, -4($sp)\n\tsw $ra, -8($sp)\n\tmove $fp, $sp\n\taddiu $sp, $sp, -12\n"

epilogue :: String 
epilogue = "\tmove $sp, $fp\n\tlw $ra, -8($sp)\n\tlw $fp, -4($sp)\n\tjr $ra\n"


condDecode :: Instr -> String 
condDecode (COND t1 op t2 ltrue lfalse)
  = let str = case op of -- inverted
                  Lt -> "bge "
                  Gt -> "ble "
                  Eq -> "bne "
                  Le -> "bgt "
                  Ge -> "blt " 
                  Ne -> "beq "
    in  "\t" ++str++ "$" ++t1++ ",$" ++t2++ ", " ++lfalse++ "\n"
condDecode (CONDF t1 op t2 ltrue lfalse)
  = let str = case op of 
                  Lt -> "blt "
                  Gt -> "bgt "
                  Eq -> "beq "
                  Le -> "ble "
                  Ge -> "bge " 
                  Ne -> "bne "
    in  "\t" ++str++ "$" ++t1++ ",$" ++t2++ ", " ++ltrue++ "\n"
condDecode (COND1 t1 ltrue lfalse)
  = "\tbeq $" ++t1++ ", $zero, " ++lfalse++ "\n"
     

instrDecode :: Instr -> String 
instrDecode code 
   = case code of 
        MOVE t1 t2 -> "\tmove $" ++t1++ ", $" ++t2++ "\n"
        MOVES t1 l str -> "\tla $" ++t1++ ", " ++l++ "\n"
        MOVEI t1 n -> "\tli $" ++t1++ ", " ++show n++"\n"
        OP _ _ _ _-> opDecode code
        OP1 _ _ _-> opDecode code
        OPI _ _ _ _-> opDecode code
        LABEL l-> l ++ ":\n"
        LABELF l-> case l of 
                     "main" -> l ++ ":\n\tsw $ra, 0($sp)\n"
                     _ -> l ++ ":\n" ++ prelude
        JUMP l-> "\tj " ++ l ++ "\n"
        COND _ _ _ _ _->condDecode code
        CONDF _ _ _ _ _->condDecode code
        COND1 _ _ _-> condDecode code
        COND1F _ _ _-> condDecode code
        CALL _ _ _-> callDecode code
        RETURN->"\tjr $ra\n"
        RETURNV t-> "\tmove $v0, $" ++ t ++"\n"++ epilogue
         

datas :: Label -> [String] -> String 
datas _ [] = "\t.data\n"
datas l (str:strs) = l ++ ": .asciiz \""++str++"\"\n" ++ datas l strs

funcDecode :: [Instr] -> String
funcDecode [] = ""
funcDecode ((LABELF "main"):xs) =instrDecode (LABELF "main") ++funcDecodeS xs
funcDecode (x:xs) =instrDecode x ++ funcDecode xs

funcDecodeS :: [Instr] -> String
funcDecodeS [] = ""
funcDecodeS ((CALL t str exp ):xs) = case str of 
                                       "print_int" -> instrDecode (CALL t str exp) ++ funcDecodeS xs
                                       "print_str" -> instrDecode (CALL t str exp) ++ funcDecodeS xs 
                                       "scanInt" -> instrDecode (CALL t str exp) ++ funcDecodeS xs
                                       _    ->     createArg exp 0 ++
                                                   "\tjal "++str++"\n"++
                                                   "\tmove $"++t++", $v0\n"++
                                                   funcDecode xs
funcDecodeS (x:xs) =instrDecode x ++ funcDecodeS xs



progDecode :: [Instr] -> String
progDecode prog = "\tjal main\n\tli $v0, 10\n\tsyscall\n"++ 
                   (funcDecode prog) ++ 
                   "\tlw $ra, 0($sp)\n\tjr $ra\n" 

