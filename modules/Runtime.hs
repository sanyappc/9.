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

------------------------------------------------------------------------
{- Per line with shell -}
------------------------------------------------------------------------
-- shell::[NDTYPE] -> [NDTYPE]



------------------------------------------------------------------------
{- Per line loop of runtime -}
------------------------------------------------------------------------
loop::[NDTYPE] -> String -> [NDTYPE]

loop stack str =
	execute (parser str) stack

------------------------------------------------------------------------
{- Execute Actions... -}
{- actions->stack->changed_stack -}
------------------------------------------------------------------------
execute::[NDAction] -> [NDTYPE] -> [NDTYPE]

execute [] stack = stack
execute (x:xs) stack = execute xs (doNDAction x stack)

------------------------------------------------------------------------
{- Execution of single NDAction -}
------------------------------------------------------------------------
doNDAction::NDAction -> [NDTYPE] -> [NDTYPE]

doNDAction NDPop stack = aPop stack
doNDAction (NDPush a) stack = aPush stack a
doNDAction NDSwap stack = aSwap stack
-- doNDAction NDDSwap stack = aDSwap stack	-- Коля пока не реализовал .
doNDAction NDRotR stack = aRotR stack
doNDAction NDRotL stack = aRotL stack
doNDAction NDDup stack = aDup stack
doNDAction NDAdd stack = aAdd stack
doNDAction NDSub stack = aSub stack
doNDAction NDMul stack = aMul stack
doNDAction DivD stack = aDivD stack
doNDAction Div stack = aDiv stack
doNDAction Mod stack = aMod stack
-- doNDAction GE stack = aGE stack			-- Коля пока не реализовал .
-- doNDAction LE stack = aGE stack			-- Коля пока не реализовал .
doNDAction G stack = aGT stack
doNDAction L stack = aLT stack
doNDAction E stack = aE stack
-- doNDAction NE stack = aNE stack			-- Коля пока не реализовал .
doNDAction NOT stack = aNot stack
doNDAction AND stack = aAnd stack
doNDAction OR stack = aOr stack
doNDAction XOR stack = aXor stack
doNDAction (NDIf true false) (x:xs)
	| toBool x = execute true xs
	| otherwise = execute false xs


------------------------------------------------------------------------
{- Function return Bool from NDBool or generate error -}
------------------------------------------------------------------------
toBool::NDTYPE -> Bool
toBool (NDTYPEb True) = True
toBool (NDTYPEb False) = False
toBool bool = error "Type Error : the value is not Boolean!!!"
