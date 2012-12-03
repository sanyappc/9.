{-
 - Module : NDAction.hs
 - Description : Модуль, для описания типа Action, для манипулирования со стеком.
 - Stability : experimental
-}

module NDAction where

import NDType

data NDCoords = NDCoords(Integer, Integer) (Integer, Integer)

-------------------------------------------------------------------------------
{- Тип Action предназначен для описания базовых функций нашего языка -}
-------------------------------------------------------------------------------
data NDAction = NDPush NDCoords NDTYPE			--
	|NDPop NDCoords								-- 
	|NDSwap NDCoords							--
	|NDDSwap NDCoords							--
	|NDRotR NDCoords							-- для смещения стека по кольцу
	|NDRotL NDCoords							-- -||-
	|NDDup NDCoords								--
	|NDAdd NDCoords								--
	|NDSub NDCoords								--
	|NDMul NDCoords								--
	|DivD NDCoords								-- для деления Double
	|Div NDCoords								-- целая часть деления
	|Mod NDCoords								-- остаток от деления
	|GE NDCoords								-- сравнения >:<:==:>=:<=:<>
	|LE NDCoords								--
	|G NDCoords									--
	|L NDCoords									--
	|E NDCoords									--
	|NE NDCoords								--
	|NOT NDCoords								--
	|AND NDCoords								--
	|OR NDCoords								--
	|XOR NDCoords								--
	|NDIf NDCoords [NDAction] [NDAction]		-- условие NDIf [При True] [При False]
	|NDNewFunction NDCoords NDTYPE [NDAction]	-- объявление функции
	|NDCallFunction NDCoords NDTYPE				-- вызов функции
	|NDSCallFunction NDCoords					-- вызов функции с вершины стека
	|NDExit NDCoords         					-- выход из п/программы, т.е. функции
	|NDCat NDCoords								-- nyan
-------------------------------------------------------------------------------
