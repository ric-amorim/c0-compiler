module CodeGen where

import IR
import AST

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Set (Set)
import qualified Data.Set as Set

-- symbol table:
-- map identifiers to temporaries
type Table = Map String Temp

-- the "supply" for generating temporaries, labels, args and saved
-- counter for temporaries,labels, args and labelsString
type Supply  = (Int, Int,Int,Int)           

-- get a new temporary
newTemp :: State Supply Temp
newTemp = do (temps,labels,args,lstr) <- State.get
             State.put (temps+1,labels,args,lstr)
             return ("t"++show temps)

newArg :: State Supply Temp
newArg = do (temps,labels,args,lstr) <- State.get
            State.put (temps,labels,args+1,lstr)
            return ("a"++show temps)

-- get a new label
newLabel :: State Supply Label
newLabel = do (temps,labels,args,lstr) <- State.get
              State.put (temps,labels+1,args,lstr)
              return ("Label"++show labels)

newLabelStr :: State Supply Label
newLabelStr = do (temps,labels,args,lstr) <- State.get
                 State.put (temps,labels,args,lstr+1)
                 return ("str"++show lstr)

-- get several temporaries
newTemps :: Int -> State Supply [Temp]
newTemps 0 = return []
newTemps n  = do
               t <- newTemp
               ts <- newTemps (n-1)
               return (t:ts)

newArgs :: Int -> State Supply [Temp]
newArgs 0 = return []
newArgs n  = do
               t <- newArg
               ts <- newArgs (n-1)
               return (t:ts)
-- make new table with new variables
newTabl :: Table -> [Stm] -> State Supply Table
newTabl tabl stms
  = do let defVar = Set.fromList [ x | Simple (DefVar _ x _) <- stms]
       let defVarFor = Set.fromList [ x | For [DefVar _ x _] _ _ _ <- stms]
       targs <- newTemps (length defVar + length defVarFor)
       let newList = Set.toList defVar ++ Set.toList defVarFor
       let createtabl = Map.toList (Map.fromList (zip newList targs)) 
       let oldtabl = Map.toList tabl
       let newtabl = Map.fromList (oldtabl ++ createtabl) 
       return (newtabl)


-- give back `n' temporaries for reuse
reuseTemps :: Int -> State Supply ()
reuseTemps n
  = do (temps, labels,args,lstr) <- State.get
       State.put (temps-n, labels,args,lstr)

-------------------------------------------------------------------------------

-- translate an expression
transExpr :: Exp -> Table -> Temp -> State Supply [Instr]
transExpr (Ident x) tabl dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "undefined variable for ident"

transExpr (Num n) table dest = return [MOVEI dest n]

-- !!!!!!!
transExpr (BoolVal b) table dest = return [MOVEI dest (if b then 1 else 0)] 

-- !!!!!!!
transExpr (StringVal s) table dest = do label <- newLabelStr
                                        return [MOVES dest label s]

transExpr (FnCall id args) tabl dest
  = do (code, temps) <- transArgs args tabl
       return (code ++ [CALL dest id temps])

transExpr (ScanInt) table dest = return [CALL dest "scanInt" []]

transExpr (Op op e1 e2) table dest = 
    do  t1 <- newTemp 
        t2 <- newTemp
        code1 <- transExpr e1 table t1
        code2 <- transExpr e2 table t2
        return (code1 ++ code2 ++ [OP op dest t1 t2])

transExpr (Op1 op e) table dest = 
    do t1 <- newTemp
       code1 <- transExpr e table t1
       return (code1 ++ [OP1 op dest t1])

-- translate functions arguments
-- each one gets a new temporary
transArgs :: [Exp] -> Table -> State Supply ([Instr], [Temp])
transArgs [] tabl = return ([], [])
transArgs (exp:exps) tabl
      = do temp <- newTemp
           code <- transExpr exp tabl temp 
           (code', temps') <- transArgs exps tabl
           return (code++code', temp:temps')


-- translate a statement
transStm :: Stm -> Table -> Label -> Label -> State Supply [Instr]
transStm (Simple simple) tabl _ _ = transSimple simple tabl

transStm (If cond stm1) tabl lcond lend
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond cond tabl ltrue lfalse False
       code1  <- transStm stm1 tabl lcond lend
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])

transStm (IfElse cond stm1 stm2) tabl lcond  lend
  = do ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond cond tabl  ltrue lfalse False
       code1 <- transStm stm1 tabl lcond lend
       code2 <- transStm stm2 tabl lcond lend
       return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++
               [LABEL lend])

transStm  (While cond stm) tabl _ _
  = do lcond <- newLabel
       lbody <- newLabel
       lend <- newLabel
       code1 <- transCond cond tabl  lbody lend False
       code2 <- transStm stm tabl lcond lend 
       return ([LABEL lcond] ++ code1 ++
               [LABEL lbody] ++ code2 ++
               [JUMP lcond, LABEL lend])

transStm (For init cond incr stm) tabl _ _
  = do lbody <- newLabel
       lnext <- newLabel
       lcond <- newLabel
       lend  <- newLabel
       code0 <- case init of 
                     [] -> return []
                     [t] -> transStm (Simple t) tabl "" ""
       code3 <- transCond cond tabl lbody lend True
       case incr of 
            [] -> do code1 <- transStm  stm tabl lcond lend
                     return (code0 ++ [LABEL lbody] ++ code1 ++ 
                          [LABEL lcond] ++ code3 ++ [LABEL lend])
            [incr] -> do code1 <- transStm stm tabl lnext lend
                         code2 <- transStm (Simple incr) tabl "" ""
                         return (code0 ++ [LABEL lbody] ++ code1 ++ 
                                 [LABEL lnext] ++ code2 ++ code3 ++ 
                                 [LABEL lend])


transStm (Ret Nothing) tabl _ _ = return [RETURN]
transStm (Ret (Just expr)) tabl _ _ =
  do dest <- newTemp
     code <- transExpr expr tabl dest
     return (code ++ [RETURNV dest])

transStm (Continue) tabl lcond _ = return [JUMP lcond]

transStm (Break) tabl _ lend  = return [JUMP lend]

transStm (PrintInt expr) tabl _ _ 
 = do dest <- newTemp
      transExpr (FnCall "print_int" [expr]) tabl dest 


transStm (PrintStr expr ) tabl _ _ 
 = do dest <- newTemp
      transExpr (FnCall "print_str" [expr]) tabl dest

transStm (Block stms) tabl lcond lend 
   = do  newtabl <- newTabl tabl stms
         transStmList stms newtabl lcond lend

-- translate a simple statement (auxiliar for transStm)
transSimple :: Simple -> Table -> State Supply [Instr]
transSimple (Assign var expr) tabl
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable for assignment"
      Just dest -> transExpr expr tabl dest

transSimple (Incr var) tabl
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable for increment"
      Just dest -> return [OPI Add dest dest 1]

transSimple (Decr var) tabl 
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable for decrement"
      Just dest -> return [OPI Sub dest dest 1]

transSimple (Expression expr) tabl 
  = do temp <- newTemp 
       transExpr expr tabl temp 

transSimple (DefVar _ var (Just expr)) tabl
  = do temp <- newTemp
       code <- transExpr expr tabl temp 
       case Map.lookup var tabl of 
         Just dest -> return (code ++ [MOVE dest temp])

transSimple (DefVar _ var Nothing) tabl 
   = return []

-- translate a condition
transCond :: Exp -> Table -> Label -> Label -> Bool -> State Supply [Instr]
transCond (Op rel e1 e2) tabl ltrue lfalse b
  | rel == Lt || rel == Gt || rel == Le || rel == Eq || rel == Ge || rel == Ne 
    || rel == And || rel == Or =
      do temp1 <- newTemp
         temp2 <- newTemp 
         code1 <- transExpr e1 tabl temp1
         code2 <- transExpr e2 tabl temp2
         reuseTemps 2
         case b of
            True ->return ( code1 ++ code2 ++
                    [CONDF temp1 rel temp2 ltrue lfalse] )
            False -> return ( code1 ++ code2 ++ 
                    [COND temp1 rel temp2 ltrue lfalse] )
transCond (FnCall b arg) tabl ltrue lfalse bool
   = do temp1 <- newTemp 
        temp2 <- newTemp
        code <- transExpr (FnCall b arg) tabl temp2
        reuseTemps 2
        case bool of 
           True -> return (code ++ [COND1F temp1 ltrue lfalse])
           False -> return (code ++ [COND1 temp1 ltrue lfalse])
        

-- translate a list of statements
-- translate individual statements and join the resulting instructions
transStmList :: [Stm] -> Table -> Label -> Label -> State Supply [Instr]
transStmList [] tabl _ _= return []
transStmList (stm:rest) tabl lcond lend = do
  code1 <- transStm stm tabl lcond lend
  code2 <- transStmList rest tabl lcond lend
  return (code1 ++ code2)

-- translate a function definition
transFunDef :: Fn -> State Supply [Instr]
transFunDef (Func _ var args stms) 
  = do targs <- newArgs (length args)      -- temporaries for arguments
       -- setup symbol table
       let table = Map.fromList (zip argsVal targs)
       -- translate the body
       newtabl <- newTabl table stms
       code <- transStmList stms newtabl "" ""
       -- return the code
       return (LABELF var : code)
    where
      argsVal = [ x | (_,x) <- args]   

transProgram :: [Fn] -> State Supply [Instr]
transProgram [] = return []
transProgram (x:xs) = do fun <- transFunDef x 
                         (temps, labels,args,lstr) <- State.get
                         State.put (0, labels,0,lstr)
                         prog <- transProgram xs
                         return (fun ++ prog)

