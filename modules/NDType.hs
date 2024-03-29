{-
 - Module : NDType.hs
 - Description : Модуль, описывающий типы данных, используемые для манипулирования на стеке
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
	|NDTYPErr String 	-- ошибки...
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
