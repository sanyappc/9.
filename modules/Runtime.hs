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
data Func = Func { actions::[NDActionPos] }

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
execute::[NDActionPos] -> Program -> Program

execute x Program{stack = (NDTYPErr err:xs), funcs = f} = Program{stack = (NDTYPErr err:xs), funcs = f}
execute [] prog = prog
execute ((NDActionPos NDExit _ _ _ _):xs) prog = prog
execute (x:xs) prog = execute xs (doNDAction x prog)
------------------------------------------------------------------------
{- Execution of single NDAction -}
------------------------------------------------------------------------
doNDAction::NDActionPos -> Program -> Program

doNDAction (NDActionPos NDPop _ _ _ _) prog =
	prog{stack = aPop (stack prog)} 

doNDAction (NDActionPos (NDPush a) _ _ _ _) prog =
	prog{stack = aPush (stack prog) a}

doNDAction (NDActionPos NDSwap _ _ _ _) prog = prog{stack =
	aSwap (stack prog)}  

doNDAction (NDActionPos NDDSwap _ _ _ _) prog =
	prog{stack = aDSwap (stack prog)}

doNDAction (NDActionPos NDRotR _ _ _ _) prog =
	prog{stack = aRotR (stack prog)}

doNDAction (NDActionPos NDRotL _ _ _ _) prog =
	prog{stack = aRotL (stack prog)}

doNDAction (NDActionPos NDDup _ _ _ _) prog =
	prog{stack = aDup (stack prog)}

doNDAction (NDActionPos NDAdd _ _ _ _) prog =
	prog{stack = aAdd (stack prog)}

doNDAction (NDActionPos NDSub _ _ _ _) prog =
	prog{stack = aSub (stack prog)}

doNDAction (NDActionPos NDMul _ _ _ _) prog =
	prog{stack = aMul (stack prog)}

doNDAction (NDActionPos DivD _ _ _ _) prog =
	prog{stack = aDivD (stack prog)}

doNDAction (NDActionPos Div _ _ _ _) prog =
	prog{stack = aDiv (stack prog)}

doNDAction (NDActionPos Mod _ _ _ _) prog =
	prog{stack = aMod (stack prog)}

doNDAction (NDActionPos GE _ _ _ _) prog =
	prog{stack = aGE (stack prog)}

doNDAction (NDActionPos LE _ _ _ _) prog =
	prog{stack = aLE (stack prog)}

doNDAction (NDActionPos G _ _ _ _) prog =
	prog{stack = aGT (stack prog)}

doNDAction (NDActionPos L _ _ _ _) prog =
	prog{stack = aLT (stack prog)}

doNDAction (NDActionPos E _ _ _ _) prog =
	prog{stack = aE (stack prog)}

doNDAction (NDActionPos NE _ _ _ _) prog =
	prog{stack = aNE (stack prog)}

doNDAction (NDActionPos NOT _ _ _ _) prog =
	prog{stack = aNot (stack prog)}

doNDAction (NDActionPos AND _ _ _ _) prog =
	prog{stack = aAnd (stack prog)}

doNDAction (NDActionPos OR _ _ _ _) prog =
	prog{stack = aOr (stack prog)}

doNDAction (NDActionPos XOR _ _ _ _) prog =
	prog{stack = aXor (stack prog)}

doNDAction (NDActionPos (NDIf true false) _ _ _ _) Program{stack = (x:xs), funcs = f}
	| toBool x = execute true Program{stack = xs, funcs = f}
	| otherwise = execute false Program{stack = xs, funcs = f}

doNDAction (NDActionPos (NDNewFunction (NDTYPEf name) acts) _ _ _ _) prog
	| member name (funcs prog) = prog{funcs = Data.Map.adjust (\x -> Func{actions = acts}) name (funcs prog) }
	| otherwise = prog{funcs = Data.Map.insert name Func{actions=acts} (funcs prog) }

doNDAction (NDActionPos (NDCallFunction (NDTYPEf name)) xx yy _ _) Program{stack = x, funcs = f}
	| member name f = execute (actions (f ! name)) Program{stack = x, funcs = f}
	| otherwise = Program{stack = aPush x (NDTYPErr ("Runtime error at line " ++ (show xx) ++ " and pos " ++ (show yy) ++ ": You've tried call undeclared function <" ++ name ++ ">.")), funcs = f}

doNDAction (NDActionPos NDExit _ _ _ _) prog =
	prog

doNDAction (NDActionPos NDSCallFunction xx yy _ _) Program{stack = (x:xs), funcs = f}  
	| isFunc x =  doNDAction (NDActionPos (NDCallFunction x) 0 0 0 0) Program{stack = xs, funcs = f}
	| otherwise = Program{stack =  aPush (x:xs) (NDTYPErr ("Stack Error at line " ++ (show xx) ++ "and pos " ++ (show yy) ++ ": While trying to call function from stack, incompatible type was detected.")), funcs = f}
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

