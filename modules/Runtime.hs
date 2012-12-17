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
import Data.Map

------------------------------------------------------------------------
-- Data structure for functions implementation.
------------------------------------------------------------------------
data Func = Func { actions::[NDActionPos] }

------------------------------------------------------------------------
--  Data structure for program representation 
------------------------------------------------------------------------
data Program = Program { stack::[NDTYPE], funcs::(Map String Func)}

------------------------------------------------------------------------
-- Execute Actions...
-- actions->stack->changed_stack
------------------------------------------------------------------------
execute::[NDActionPos] -> Program -> Program
execute _ Program{stack = (NDTYPErr err:xs), funcs = f} =
    Program{stack = (NDTYPErr err:xs), funcs = f}
execute [] prog = 
	prog
execute ((NDActionPos NDExit _ _ _ _):xs) prog = 
	prog
execute (x:xs) prog =
	execute xs (check x (doNDAction x prog))
	
check::NDActionPos -> Program -> Program
check (NDActionPos _ xx yy _ _) Program{stack = (NDTYPErr err:xs), funcs = f} =
	Program{stack = (NDTYPErr ("error: line: " ++ (show xx) ++ " col: " ++ (show yy) ++ ": " ++ err):xs), funcs = f}
check _ prog = prog

executeCGI::[NDActionPos] -> Program -> Program
executeCGI _ Program{stack = (NDTYPErr err:xs), funcs = f} =
    Program{stack = (NDTYPErr err:xs), funcs = f}
executeCGI [] prog = 
	prog
executeCGI ((NDActionPos NDExit _ _ _ _):xs) prog = 
	prog
executeCGI (x:xs) prog =
	executeCGI xs (checkCGI x (doNDAction x prog))
	
checkCGI::NDActionPos -> Program -> Program
checkCGI (NDActionPos _ xx yy _ _) Program{stack = (NDTYPErr err:xs), funcs = f} =
	Program{stack = (NDTYPErr ("error: col: " ++ (show yy) ++ ": " ++ err):xs), funcs = f}
checkCGI _ prog = prog

------------------------------------------------------------------------
--  Execution of single NDAction 
------------------------------------------------------------------------
ecallf = "calling function from stack"

doNDAction::NDActionPos -> Program -> Program
doNDAction (NDActionPos NDPop _ _ _ _) prog =
	prog{stack = aPop (stack prog)} 
doNDAction (NDActionPos (NDPush a) _ _ _ _) prog =
	prog{stack = aPush (stack prog) a}
doNDAction (NDActionPos NDSwap _ _ _ _) prog = 
	prog{stack = aSwap (stack prog)}  
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
doNDAction (NDActionPos (NDIf true _) _ _ _ _) Program{stack = ((NDTYPEb True):xs), funcs = f} =
	execute true Program{stack = xs, funcs = f}
doNDAction (NDActionPos (NDIf _ false) _ _ _ _) Program{stack = ((NDTYPEb False):xs), funcs = f} =
	execute false Program{stack = xs, funcs = f}
doNDAction (NDActionPos (NDIf _ _) _ _ _ _) Program{stack = ss, funcs = f} =
	Program{stack = ((NDTYPErr "if statement: incompatible type"):ss), funcs = f}
doNDAction (NDActionPos (NDNewFunction (NDTYPEf name) acts) _ _ _ _) prog
	| member name (funcs prog) = prog{funcs = Data.Map.adjust (\x -> Func{actions = acts}) name (funcs prog) }
	| otherwise = prog{funcs = Data.Map.insert name Func{actions=acts} (funcs prog) }
doNDAction (NDActionPos (NDCallFunction (NDTYPEf name)) _ _ _ _) Program{stack = x, funcs = f}
	| member name f = execute (actions (f ! name)) Program{stack = x, funcs = f}
	| otherwise = Program{stack = aPush x (NDTYPErr ("undeclared function: \"" ++ name ++ "\"")), funcs = f}
doNDAction (NDActionPos NDExit _ _ _ _) prog =
	prog
doNDAction (NDActionPos NDSCallFunction xx yy xxx yyy) Program{stack = [], funcs = f} =
	Program{stack = [NDTYPErr $ ecallf++erempty], funcs = f}
doNDAction (NDActionPos NDSCallFunction xx yy xxx yyy) Program{stack = (x:xs), funcs = f}  
	| isFunc x =  doNDAction (NDActionPos (NDCallFunction x) xx yy xxx yyy) Program{stack = xs, funcs = f}
	| otherwise = Program{stack = ((NDTYPErr $ ecallf++": incompatible type"):x:xs), funcs = f}
doNDAction _ Program{stack = xs, funcs = f} = 
	Program{stack = ((NDTYPErr ("runtime"++erunkn)):xs), funcs = f}

------------------------------------------------------------------------
isFunc (NDTYPEf func) = True
isFunc _ = False
