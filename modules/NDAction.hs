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
	|NDNext			-- для смещения стека по кольцу.
	|NDPrev
	|NDDup
	|NDAdd
	|NDSub
	|NDMul
	|DivD			-- для деления в рамкам Double
	|Div			-- целая часть деления
	|Mod			-- остаток от деления
	|GE			-- сравнения >:<:==:>=:<=:<>
	|LE
	|G
	|L
	|E
	|NE
	|NOT			-- Boolean
	|AND
	|OR
	|XOR
	|TOP			-- отображает верхушку стека
	|PRINT			-- отображает стек
	deriving (Show, Read)
-------------------------------------------------------------------------------


