{-
 - Module : NDType.hs
 - Description : Модуль, описывающий типы данных, используемые для манипулирования на стеке
 - Stability : experimental
 -}

module NDType where

--------------------------------------------------------------------------------
{- Описание типов нашего языка -}
--------------------------------------------------------------------------------
data NDTYPE = NDTYPEi Integer
	|NDTYPEd Double
	|NDTYPEc Char
	|NDTYPEs String
	|NDTYPEb Bool
	deriving (Show, Read)
--------------------------------------------------------------------------------

{-
 - Usage example...
main::IO ()

main = print (func (NDTYPEi 10)) >> print (func (NDTYPEd 10.0))

func::NDTYPE -> Int

func (NDTYPEi a) = a::Int
func (NDTYPEd a) = 100
-}
