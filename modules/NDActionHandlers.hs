{-
 - Module : NDActionHandlers.hs
 - Description : Модуль, описывающий обработчики событий типа ActionNode
 - Stability : experimental. На данный момент реализованы не все ф-ции.
 -}


 module NDActionHandlers where
 import NDType

------------------------------------------------------------------------------- 
{-

usage:  - все функции начинаются с префикса a - action. 	
	- в качестве аргумента - [NDTYPE]

*NDActionHandlers> let x = [NDTYPEi 7, NDTYPEd 8]

*NDActionHandlers> aPrint x
"[NDTYPEd 8.0,NDTYPEi 7]"

*NDActionHandlers> aMul x
[NDTYPEd 56.0]

*NDActionHandlers> aE x
[NDTYPEb False]

*NDActionHandlers> let x = [NDTYPEi 7, NDTYPEd 8, NDTYPEb True]

-- так как наш стек предствлен наоборот, то RotL - это сдвиг данного списка влево.
-- аналогично с RotR.

*NDActionHandlers> aPrint  x
"[NDTYPEb True,NDTYPEd 8.0,NDTYPEi 7]"

*NDActionHandlers> aPrint $ aRotL x
"[NDTYPEd 8.0,NDTYPEi 7,NDTYPEb True]"

*NDActionHandlers> aPrint $ aRotR x
"[NDTYPEi 7,NDTYPEb True,NDTYPEd 8.0]"

*NDActionHandlers> aPrint [NDTYPEs "hello, ", NDTYPEs "9.!"]
"[NDTYPEs \"9.!\",NDTYPEs \"hello, \"]"
*NDActionHandlers> aPrint $ aConcat [NDTYPEs "hello, ", NDTYPEs "9.!"]
"[NDTYPEs \"9.!hello, \"]"

*NDActionHandlers> aPop x
[NDTYPEd 8.0,NDTYPEb True]

-}
--------------------------------------------------------------------------------
 {-
	1. Операции с элементами на стеке
 -}
--------------------------------------------------------------------------------
 {-добавление элемента в стек-}
 aPush :: [NDTYPE] -> NDTYPE -> [NDTYPE]
 aPush stack x = x:stack

 {-дублирование последнего элемента на стеке-}
 aDup :: [NDTYPE] -> [NDTYPE]
 aDup [] = error "DataStack Error : in Dup. Empty Stack." 
 aDup  (h:t) = h:h:t
 
 aPop :: [NDTYPE] -> [NDTYPE] 
 aPop [] = error "DataStack Error : in Pop. Empty Stack."
 aPop (h:t) = t

 aRotR :: [NDTYPE] -> [NDTYPE]
 aRotR [] = error "DataStack Error : in RotR. Empty Stack."
 aRotR (h:t) = t ++ [h]

 aRotL :: [NDTYPE] -> [NDTYPE] 
 aRotL [] = error "DataStack Error : in RotL. Empty Stack."
 aRotL  stack = (last stack):(init stack)  

 aSwap :: [NDTYPE] -> [NDTYPE] 
 aSwap [] = error "DataStack Error : in Swap. Empty Stack."
 aSwap [x] = error "DataStack Error : in Swap. Not enough elements."
 aSwap (x:y:t) = y:x:t

 aTop :: [NDTYPE] -> String
 aTop [] =  error "DataStack Error : in Top. Empty Stack."
 aTop stack = show $ (!!0) stack

 aPrint :: [NDTYPE] -> String
 aPrint = show.reverse

--------------------------------------------------------------------------------
 {-  
	2. Функции, возвращающищие значения типа Int 
 -}
--------------------------------------------------------------------------------

 aDiv :: [NDTYPE] -> [NDTYPE]
 aDiv ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEi (div x y):t)
 aDiv stack = error "DataStack Error : In DivI. Incompatible types" 

 aMod :: [NDTYPE] -> [NDTYPE]
 aMod ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEi (mod x y):t)
 aMod stack = error "DataStack Error : In Mod. Incompatible types" 


--------------------------------------------------------------------------------
{-
	3. Функции, возвращающие значения типа Int и Double
-} 
--------------------------------- -----------------------------------------------
 
 castToDouble :: (Double -> Double -> Double) -> NDTYPE -> NDTYPE -> NDTYPE
 castToDouble f (NDTYPEi x) (NDTYPEi y) = NDTYPEd (f (read (show x)::Double) (read (show y)::Double))
 castToDouble f (NDTYPEi x) (NDTYPEd y) = NDTYPEd (f (read (show x)::Double) y)
 castToDouble f (NDTYPEd x) (NDTYPEi y) = NDTYPEd (f x (read (show y)::Double))
 castToDouble f (NDTYPEd x) (NDTYPEd y) = NDTYPEd (f x y)
 castToDouble f x y = error "DataStack Error : Incompatible iypes (should be - Double | Int)" 
 
 aAdd :: [NDTYPE] -> [NDTYPE]
 aAdd ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((+) x y)):t)
 aAdd (x:y:t) = (castToDouble (+) x y) : t
 
 aSub :: [NDTYPE] -> [NDTYPE]
 aSub ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((-)x y)):t)
 aSub (x:y:t) = (castToDouble (-) x y) : t


 aMul :: [NDTYPE] -> [NDTYPE]
 aMul ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((*)x y)):t)
 aMul (x:y:t) = (castToDouble (*) x y) : t

 
 aDivD :: [NDTYPE] -> [NDTYPE]
 aDivD (x:y:t) = (castToDouble (/) x y) : t

--------------------------------------------------------------------------------
 {- 
 	4. Операции со строками 
 -}
--------------------------------------------------------------------------------

 aConcat ::[NDTYPE] -> [NDTYPE]
 aConcat ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEs (y ++ x)):t
 aConcat ((NDTYPEs x):(NDTYPEc y):t) = (NDTYPEs ([y] ++ x)):t
 aConcat ((NDTYPEc x):(NDTYPEs y):t) = (NDTYPEs (y ++ [x])):t
 aConcat ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEs ([y] ++ [x])):t
 aConcat stack = error "DataStack Error : Incompatible iypes (should be - Char | String)" 

  -------------------------------------------------------------------------------
 {-
    5. функции, возвращающие значетие типа Bool
 -}
 -------------------------------------------------------------------------------

 {- функция сравнения двух элементов 
	операции вида : >, <, <=, >=, != ...
	возвращает значение типа Boolean
 -}

 aNot :: [NDTYPE] -> [NDTYPE]	
 aNot ((NDTYPEb x):t) = (NDTYPEb (not x)):t
 aNot stack = error "DataStack Error : in Not. Incomparable types"

 aAnd :: [NDTYPE] -> [NDTYPE]	
 aAnd ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((&&) x y)):t
 aAnd stack = error "DataStack Error : in And. Incomparable types"

 aOr :: [NDTYPE] -> [NDTYPE]	
 aOr ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((||) x y)):t
 aOr stack = error "DataStack Error : in Or. Incomparable types"

 aXor :: [NDTYPE] -> [NDTYPE]	
 aXor ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((/=) x y)):t
 aXor stack = error "DataStack Error : in Xor. Incomparable types"

 aE :: [NDTYPE] -> [NDTYPE]	
 aE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((==) x y)):t
 aE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((==) (read (show x)::Double) y)):t
 aE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((==) x (read (show y)::Double))):t
 aE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((==) x y)):t
 aE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((==) x y)):t
 aE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((==) x y)):t
 aE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((==) x y)):t
 aE stack = error "DataStack Error : in E. Incomparable types"


 aGT :: [NDTYPE] -> [NDTYPE]	
 aGT ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((>) x y)):t
 aGT ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((>) (read (show x)::Double) y)):t
 aGT ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((>) x (read (show y)::Double))):t
 aGT ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((>) x y)):t
 aGT ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((>) x y)):t
 aGT ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((>) x y)):t
 aGT ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((>) x y)):t
 aGT stack = error "DataStack Error : in GT. Incomparable types"

 aLT :: [NDTYPE] -> [NDTYPE]	
 aLT ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((<) x y)):t
 aLT ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((<) (read (show x)::Double) y)):t
 aLT ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((<) x (read (show y)::Double))):t
 aLT ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((<) x y)):t
 aLT ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((<) x y)):t
 aLT ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((<) x y)):t
 aLT ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((<) x y)):t
 aLT stack = error "DataStack Error : in LT. Incomparable types"
