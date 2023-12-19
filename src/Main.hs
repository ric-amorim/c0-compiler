module Main where 

import Lexer
import Parser
import AST
import IR
import TypeCheck
import CodeGen    
import MachineGen

import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State

-- starting values for temporary and label counters
initialSupply :: Supply
initialSupply = (0, 0,0,0)

-- run a code generation action with initial supply
runCodeGen :: State Supply [Instr] -> [Instr]
runCodeGen gen = State.evalState gen initialSupply

main = do
     txt <- getContents
     print (parser $ alexScanTokens txt)
     let pars = parser $ alexScanTokens txt -- get ast
     --let typeCheck = checkProgram Map.empty pars
     --printTC typeCheck
     let code = runCodeGen $ transProgram pars -- get middle code
     printIR code
     let machine = progDecode code 
     putStr machine

printIR :: [Instr] -> IO ()
printIR = mapM_ print

printTC :: [TypeEnv] -> IO ()
printTC = mapM_ print

-- examples ----------------------------------------------------------------
{- apagar dps
example1 :: Exp
example1
    = StringVal "Hello World!"

example2 :: Stm
example2
  = IfElse
    (Op Lt (Ident "x") (Num 0))
    (Simple (Assign "y" (Num 1)))
    (Simple (Assign "y" (Num 2)))

example3 :: Stm
example3
  = While (Op Lt (Num 0) (Ident "b"))
    ( Block [ Simple (Assign "r" (Op Mod (Ident "a") (Ident "b")))
            , Simple (Assign "a" (Ident "b"))
            , Simple (Assign "b" (Ident "r"))
            , Simple (Incr "a")
            , Simple (Decr "b")
            , Break
            , Continue
            , Simple (Assign "r" (ScanInt))
            ] 
    )

example4 :: Exp
example4
  = Op Add
    (FnCall "f" [Op Sub (Num 1) (Num 2)])
    (FnCall "g" [Op Mult (Num 3) (Num 4)])


example5 :: Fn
example5 = Func Int "max3" [(String,"x"),(String,"y"),(String,"z")] 
           [  (Simple (DefVar String "r" (Just (Num 1)))) 
              , Block [ (Simple (DefVar String "r" (Just (Ident "x")))), PrintInt (Ident "r") ]
              , PrintInt (Ident "r")
                ]

example6 :: Stm
example6
  = For [DefVar String "i" (Just (Num 0))]
        (Op Lt (Ident "i") (Num 10))
        [Incr "i"]
        (Block [ PrintInt (Ident "i"),Break, Continue, Simple (Assign "i" (Num 1)) ])
-}      
