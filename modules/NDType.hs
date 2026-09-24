{-
 - Module : NDType.hs
 - Description : Модуль, описывающий типы данных, используемые для манипулирования на стеке,
 -		и тип Action (они взаимно рекурсивны из-за цитат)
 - Stability : experimental
 -}

module NDType where

-------------------------------------------------------------------------------
-- Описание типов нашего языка
-------------------------------------------------------------------------------
data NDTYPE = 
	 NDTYPEi Integer	--
	|NDTYPEd Double		--
	|NDTYPEc Char		--
	|NDTYPEs String		--
	|NDTYPEb Bool		--
	|NDTYPEf String		--
	|NDTYPEq String [NDActionPos]	-- цитата: исходный текст и программа
	|NDTYPErr String 	-- ошибки...
-------------------------------------------------------------------------------
-- Тип Action предназначен для описания базовых функций нашего языка
-------------------------------------------------------------------------------
data NDAction = NDPush NDTYPE
	|NDPop					-- 
	|NDSwap 				--
	|NDDSwap				--
	|NDRotR					-- для смещения стека по кольцу
	|NDRotL					-- -||-
	|NDDup					--
	|NDAdd					--
	|NDSub					--
	|NDMul					--
	|DivD					-- для деления Double
	|Div					-- целая часть деления
	|Mod					-- остаток от деления
	|GE						-- сравнения >:<:==:>=:<=:<>
	|LE						--
	|G						--
	|L						--
	|E						--
	|NE						--
	|NOT					--
	|AND					--
	|OR						--
	|XOR					--
	|NDIf [NDActionPos] [NDActionPos]	-- условие NDIf [При True] [При False]
	|NDNewFunction NDTYPE [NDActionPos]-- объявление функции
	|NDCallFunction NDTYPE		-- вызов функции
	|NDSCallFunction		-- вызов функции с вершины стека
	|NDExit         		-- выход из п/программы, т.е. функции
	|NDCat					-- конкатенация строк и цитат (9.)
	|NDDip					-- x [q] dip: выполнить q под x
-------------------------------------------------------------------------------
-- NDActionPos NDAction start_line start_col end_line end_col
data NDActionPos = NDActionPos NDAction Int Int Int Int

-------------------------------------------------------------------------------
-- Замена стандартного show для нормального отображения 
-- кириллицы на стеке
-------------------------------------------------------------------------------
showNew [] = "stack: []"
showNew (a:[]) = "stack: ["++(showType a)++"]"
showNew a = showLoop a "stack: ["

showLoop (a:[]) string = string ++ (showType a) ++ "]"
showLoop (a:b) string = showLoop b (string++(showType a)++",") 

showNewLn [] = "stack is empty"
showNewLn (a:[]) = showType a
showNewLn (a:b) = showType a ++ "\n" ++ showNewLn b

showType (NDTYPEi a) = "NDTYPEi "++show a
showType (NDTYPEd a) = "NDTYPEd "++show a
showType (NDTYPEc a) = "NDTYPEc '"++(replaceChar a)++"'"
showType (NDTYPEs a) = "NDTYPEs \""++(replaceString a [])++"\""
showType (NDTYPEb a) = "NDTYPEb "++show a
showType (NDTYPEf a) = "NDTYPEf "++ a
showType (NDTYPEq a _) = "NDTYPEq ["++(replaceString a [])++"]"
showType (NDTYPErr a) = "NDTYPErr "++ a 

replaceString [] c = c
replaceString (a:b) c = replaceString b (c++(replaceChar a))
replaceChar a = case a of
                     '\\' -> "\\\\"
                     '\n' -> "\\n"
                     '\r' -> "\\r"
                     '\t' -> "\\t"
                     '\v' -> "\\v"
                     '\f' -> "\\f"
                     '\a' -> "\\a"
                     '\b' -> "\\b"
                     '\"' -> "\\\""
                     '\'' -> "\\\'"
                     _ -> [a]
