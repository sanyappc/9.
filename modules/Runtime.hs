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

execute x Program{stack = (NDTYPErr err:xs), funcs = f} = Program{stack = (NDTYPErr err:xs), funcs = f}
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
doNDAction NDDSwap prog = prog{stack = aDSwap (stack prog)}
doNDAction NDRotR prog = prog{stack = aRotR (stack prog)}
doNDAction NDRotL prog = prog{stack = aRotL (stack prog)}
doNDAction NDDup prog = prog{stack = aDup (stack prog)}
doNDAction NDAdd prog = prog{stack = aAdd (stack prog)}
doNDAction NDSub prog = prog{stack = aSub (stack prog)}
doNDAction NDMul prog = prog{stack = aMul (stack prog)}
doNDAction DivD prog = prog{stack = aDivD (stack prog)}
doNDAction Div prog = prog{stack = aDiv (stack prog)}
doNDAction Mod prog = prog{stack = aMod (stack prog)}
doNDAction GE prog = prog{stack = aGE (stack prog)}
doNDAction LE prog = prog{stack = aLE (stack prog)}
doNDAction G prog = prog{stack = aGT (stack prog)}
doNDAction L prog = prog{stack = aLT (stack prog)}
doNDAction E prog = prog{stack = aE (stack prog)}
doNDAction NE prog = prog{stack = aNE (stack prog)}
doNDAction NOT prog = prog{stack = aNot (stack prog)}
doNDAction AND prog = prog{stack = aAnd (stack prog)}
doNDAction OR prog = prog{stack = aOr (stack prog)}
doNDAction XOR prog = prog{stack = aXor (stack prog)}
doNDAction (NDIf true false) Program{stack = (x:xs), funcs = f}
	| toBool x = execute true Program{stack = xs, funcs = f}
	| otherwise = execute false Program{stack = xs, funcs = f}
doNDAction (NDNewFunction (NDTYPEf name) acts) prog
	| member name (funcs prog) = prog{funcs = Data.Map.adjust (\x -> Func{actions = acts}) name (funcs prog) }
	| otherwise = prog{funcs = Data.Map.insert name Func{actions=acts} (funcs prog) }
doNDAction (NDCallFunction (NDTYPEf name)) Program{stack = x, funcs = f}
	| member name f = execute (actions (f ! name)) Program{stack = x, funcs = f}
	| otherwise = Program{stack = aPush x (NDTYPErr ("Runtime Error. You've tried call undeclared function <" ++ name ++ ">.")), funcs = f}
doNDAction NDExit prog = prog
doNDAction NDSCallFunction Program{stack = (x:xs), funcs = f}  
	| isFunc x =  doNDAction (NDCallFunction x) Program{stack = xs, funcs = f}
	| otherwise = Program{stack = aPush (x:xs) (NDTYPErr "Stack Error. While trying to call function from stack, incompatible type was detected."), funcs = f}
--	| otherwise = error "DataStack Error : calling function from stack. Incompatible type (expected: NDTYPEf)."

------------------------------------------------------------------------
{- Function return Bool from NDBool or generate error -}
------------------------------------------------------------------------
isFunc (NDTYPEf func) = True
isFunc _ = False

toBool::NDTYPE -> Bool
toBool (NDTYPEb True) = True
toBool (NDTYPEb False) = False
toBool bool = error "Type Error : Incompatible types (should be - Bool)"

