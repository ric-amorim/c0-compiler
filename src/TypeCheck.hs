module TypeCheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map

-- type environment (i.e. symbol table)
type TypeEnv = Map String Type

----------------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------------
checkExp :: TypeEnv -> Exp -> Type
checkExp env (Num n) = Int
checkExp env (Ident x) = case Map.lookup x env of
    Nothing -> error "undeclared variable"
    Just t -> t
checkExp env (Op Add e1 e2) 
   = let t1 = checkExp env e1 
         t2 = checkExp env e2
      in if t1==Int && t2==Int then Int
         else error "type error in +"

----------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------
checkStm :: TypeEnv -> Maybe Type -> Stm -> Bool
checkStm env tyret (Simple (Assign var expr))
  = case Map.lookup var env of
      Nothing -> error "undeclared variable"
      Just t1 -> let t2 = checkExp env expr
                 in if t1 == t2  then True
                    else error "type error in assignment"

checkStm env tyret (Simple (DefVar tyvar var expr))
  = case Map.lookup var env of 
      Just _ -> error "variable already declared"
      Nothing -> case expr of
                   Nothing -> True
                   Just e1 -> checkStm (Map.insert var tyvar env) tyret (Simple (Assign var e1)) 

checkStm env tyret (IfElse cond stm1 stm2)
  = let t0 = checkExp env cond                           
    in if t0 == Bool then
         checkStm env tyret stm1 &&
         checkStm env tyret stm2
       else error "type error: condition should be bool"

checkStm env tyret (If cond stm)
  = let t0 = checkExp env cond                           
    in if t0 == Bool then
         checkStm env tyret stm 
       else error "type error: condition should be bool"

checkStm env tyret (While exp stm1)
  = let t0 = checkExp env exp
    in if t0 == Bool then checkStm env tyret stm1
       else error "type error: condition should be bool"
{-
checkStm env tyret (Block decls stms)
  = let env' = extendEnv env decls
    in all (checkStm env' tyret) stms
-}
checkStm env (Just typ) (Ret (Just exp))
  = checkExp env exp == typ
checkStm env Nothing (Ret _)
  = error "type error: return outside of function"

-- extend an environment with a list of declarations
extendEnv :: TypeEnv -> [Decl] -> TypeEnv
extendEnv env [] = env
extendEnv env ((v,t):rest) = extendEnv (Map.insert t v env) rest

-------------------------------------------------------------------------------
-- Function definitions
-------------------------------------------------------------------------------
checkFunDef :: TypeEnv -> Fn -> TypeEnv
checkFunDef env (Func tyret fun decls stm)
  = let tyargs = map fst decls
        env' = extendEnv env ((TyFun tyargs tyret,fun):decls)
    in if checkStm env' (Just tyret) (head stm) then
         extendEnv env [(TyFun tyargs tyret,fun)] 
       else error "type error in function definition"

checkProgram :: TypeEnv -> [Fn] -> [TypeEnv]
checkProgram env [] = [env]
checkProgram env (x:xs) = checkFunDef env x : checkProgram env xs 
                        

