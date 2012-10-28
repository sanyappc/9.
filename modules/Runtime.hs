{-
- Module :Runtime.hs
- Description : The runtime module for our interpreter.
- Stability : experimental
-}

module Runtime where

import NDType
import NDParse
import NDAction
import NDActionHandlers

import Text.Printf
import Data.Map
{- Read about maps: http://book.realworldhaskell.org/read/data-structures.html -}
{- http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html -}

------------------------------------------------------------------------
{- Data structure for functions implementation. -}
------------------------------------------------------------------------
data Func = Func { actions::[NDAction] }

------------------------------------------------------------------------
{- Data structure for program representation -}
------------------------------------------------------------------------
data Program = Program { stack::[NDTYPE], funcs::(Map String Func)}

------------------------------------------------------------------------
{- Per line loop of runtime -}
------------------------------------------------------------------------
loop::Program -> String -> Program

loop prog str = execute (parser str) prog
------------------------------------------------------------------------
{- Execute Actions... -}
{- actions->stack->changed_stack -}
------------------------------------------------------------------------
execute::[NDAction] -> Program -> Program

execute [] prog = prog
execute (NDExit:xs) prog = prog
execute (x:xs) prog = execute xs (doNDAction x prog)
------------------------------------------------------------------------
{- Execution of single NDAction -}
------------------------------------------------------------------------
doNDAction::NDAction -> Program -> Program

doNDAction NDPop prog = prog{stack = aPop (stack prog)} 
doNDAction (NDPush a) prog = prog{stack = aPush (stack prog) a}
doNDAction NDSwap prog = prog{stack = aSwap (stack prog)}  

--doNDAction NDDSwap stack = aDSwap stack	-- Коля реализовал .
doNDAction NDRotR prog = prog{stack = aRotR (stack prog)}
doNDAction NDRotL prog = prog{stack = aRotL (stack prog)}
doNDAction NDDup prog = prog{stack = aDup (stack prog)}
doNDAction NDAdd prog = prog{stack = aAdd (stack prog)}
doNDAction NDSub prog = prog{stack = aSub (stack prog)}
doNDAction NDMul prog = prog{stack = aMul (stack prog)}
doNDAction DivD prog = prog{stack = aDivD (stack prog)}
doNDAction Div prog = prog{stack = aDiv (stack prog)}
doNDAction Mod prog = prog{stack = aMod (stack prog)}
--doNDAction GE stack = aGE stack			-- Коля реализовал .
--doNDAction LE stack = aLE stack			-- Коля реализовал .
doNDAction G prog = prog{stack = aGT (stack prog)}
doNDAction L prog = prog{stack = aLT (stack prog)}
doNDAction E prog = prog{stack = aE (stack prog)}
--doNDAction NE stack = aNE stack			-- Коля реализовал .
doNDAction NOT prog = prog{stack = aNot (stack prog)}
doNDAction AND prog = prog{stack = aAnd (stack prog)}
doNDAction OR prog = prog{stack = aOr (stack prog)}
doNDAction XOR prog = prog{stack = aXor (stack prog)}
doNDAction (NDIf true false) Program{stack = (x:xs), funcs = f}
	| toBool x = execute true Program{stack = xs, funcs = f}
	| otherwise = execute false Program{stack = xs, funcs = f}
doNDAction (NDNewFunction name acts) prog
	| member name (funcs prog) = prog{funcs = Data.Map.adjust (\x -> Func{actions = acts}) name (funcs prog) }
	| otherwise = prog{funcs = Data.Map.insert name Func{actions=acts} (funcs prog) }
doNDAction (NDCallFunction name) prog
	| member name (funcs prog) = execute (actions ((funcs prog) ! name)) prog
	| otherwise = error $"Input Error : Function " ++ name ++ " was not declared."
doNDAction NDExit prog = prog
doNDAction (NDSPutFunction name) prog = prog{stack = (NDFUNC name):(stack prog)}
-- doNDAction NDSCallFunction prog
------------------------------------------------------------------------
{- Function return Bool from NDBool or generate error -}
------------------------------------------------------------------------
toBool::NDTYPE -> Bool
toBool (NDTYPEb True) = True
toBool (NDTYPEb False) = False
toBool bool = error "Type Error : Incompatible types (should be - Bool)"

