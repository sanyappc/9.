{-
 - Module : NDAction.hs
 - Description : Модуль, для описания типа Action, для манипулирования со стеком.
 - Stability : experimental
-}

module NDAction where

import NDType

-------------------------------------------------------------------------------
{- Тип Action предназначен для описания базовых функций нашего языка -}
------------------------------------------------------------------------------
data NDAction = NDPush NDTYPE
	|NDPop
	|NDSwap
	|NDDSwap
	|NDRotR								-- для смещения стека по кольцу.
	|NDRotL								--------------------------------
	|NDDup
	|NDAdd
	|NDSub
	|NDMul
	|DivD								-- для деления в рамкам Double
	|Div								-- целая часть деления
	|Mod								-- остаток от деления
	|GE									-- сравнения >:<:==:>=:<=:<>
	|LE
	|G
	|L
	|E
	|NE
	|NOT								-- Boolean
	|AND
	|OR
	|XOR
	|TOP								-- отображает верхушку стека
	|PRINT								-- отображает стек
	|NDIf [NDAction] [NDAction]			-- условие NDIf [При True] [При False]
	|NDNewFunction NDTYPE [NDAction]	-- объявление функции
	|NDCallFunction NDTYPE				-- вызов функции
	|NDSCallFunction					-- вызов функции с вершины стека
	|NDExit         					-- выход из п/программы, т.е. функции
	|NDCat
-------------------------------------------------------------------------------
