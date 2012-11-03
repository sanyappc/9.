{-
 - Module : NDActionHandlers.hs
 - Description : Модуль, описывающий обработчики событий типа ActionNode
 - Stability : experimental. 
 		v1.1 
 		- исправлено выполнение бинарных операций
 		- добавлены операции (>=, <=, /=, DSwap) 
 		- исправлен вывод ошибок
 -}


 module NDActionHandlers where
 import NDType

------------------------------------------------------------------------------- 
{-
usage:  - все функции начинаются с префикса a - action. 	
	- в качестве аргумента - [NDTYPE]
	- все бинарные операции  инвертируются на стеке
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
 aDup [] = error "DataStack Error : in Dup. Empty stack." 
 aDup  (h:t) = h:h:t
 
 aPop :: [NDTYPE] -> [NDTYPE] 
 aPop [] = error "DataStack Error : in Pop. Empty stack."
 aPop (h:t) = t

 aRotR :: [NDTYPE] -> [NDTYPE]
 aRotR [] = error "DataStack Error : in RotR. Empty stack."
 aRotR (h:t) = t ++ [h]

 aRotL :: [NDTYPE] -> [NDTYPE] 
 aRotL [] = error "DataStack Error : in RotL. Empty stack."
 aRotL  stack = (last stack):(init stack)  

 aSwap :: [NDTYPE] -> [NDTYPE] 
 aSwap (x:y:t) = y:x:t
 aSwap stack = error "DataStack Error : in Swap. Too few elements."

 aDSwap :: [NDTYPE] -> [NDTYPE] 
 aDSwap (a:b:c:d:t) = (c:d:a:b:t)
 aDSwap stack = error "DataStack Error : in DSwap. Too few elements."

 aTop :: [NDTYPE] -> String
 aTop [] =  error "DataStack Error : in Top. Empty Stack."
 aTop stack = show $ (!!0) stack

 aPrint :: [NDTYPE] -> String
 aPrint = show

--------------------------------------------------------------------------------
 {-  
	2. Функции, возвращающищие значения типа Int 
 -}
--------------------------------------------------------------------------------

 aDiv :: [NDTYPE] -> [NDTYPE]
 aDiv ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEi (div y x):t)
 aDiv stack = if ((length stack) >= 2) 
 	then error "DataStack Error : In Div. Incompatible types" 
 	else error "DataStack Error : In Div. Not enough elements."
 aMod :: [NDTYPE] -> [NDTYPE]
 aMod ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEi (mod y x):t)
 aMod stack = if ((length stack) >= 2) 
 	then error "DataStack Error : In Mod. Incompatible types" 
 	else error "DataStack Error : In Mod. Not enough elements."

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
 castToDouble f x y = error "DataStack Error : in castToDouble. Incompatible types (expected: Double | Int)." 
 
 aAdd :: [NDTYPE] -> [NDTYPE]
 aAdd ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((+) y x)):t)
 aAdd (x:y:t) = (castToDouble (+) y x) : t
 aAdd stack = error "DataStack Error : in Add. Too few elements."
 
 aSub :: [NDTYPE] -> [NDTYPE]
 aSub ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((-)y x)):t)
 aSub (x:y:t) = (castToDouble (-) y x) : t
 aSub stack = error "DataStack Error : in Sub. Too few elements."

 aMul :: [NDTYPE] -> [NDTYPE]
 aMul ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((*)y x)):t)
 aMul (x:y:t) = (castToDouble (*) y x) : t
 aMul stack = error "DataStack Error : in Mul. Too few elements."
 
 aDivD :: [NDTYPE] -> [NDTYPE]
 aDivD (x:y:t) = (castToDouble (/) y x) : t
 aDivD stack = error "DataStack Error : in DivD. Too few elements."

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
 aConcat stack = if ((length stack) >= 2) 
 					then error "DataStack Error : in Concat. Incompatible types (expected: Char | String)." 
 					else error "DataStack Error : in Concat. Too few elements." 
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
 aNot ((NDTYPEi x):t) = (NDTYPEi (-x)):t -- отрицание
 aNot ((NDTYPEd x):t) = (NDTYPEd (-x)):t	
 aNot ((NDTYPEb x):t) = (NDTYPEb (not x)):t
 aNot stack = if ((length stack) >= 1) 
 					then error "DataStack Error : in Not. Incompatible types (expected: Bool | Doble | Int)." 
 					else error "DataStack Error : in Not. Too few elements." 

 aAnd :: [NDTYPE] -> [NDTYPE]	
 aAnd ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((&&) y x)):t
 aAnd stack = if ((length stack) >= 1) 
 					then error "DataStack Error : in And. Incompatible types (expected: Bool)." 
 					else error "DataStack Error : in And. Too few elements."

 aOr :: [NDTYPE] -> [NDTYPE]	
 aOr ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((||) y x)):t
 aOr stack = if ((length stack) >= 1) 
 					then error "DataStack Error : in Or. Incompatible types (expected: Bool)." 
 					else error "DataStack Error : in Or. Too few elements."

 aXor :: [NDTYPE] -> [NDTYPE]	
 aXor ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((/=) y x)):t
 aXor stack = if ((length stack) >= 1) 
 					then error "DataStack Error : in Xor. Incompatible types (expected: Bool)." 
 					else error "DataStack Error : in Xor. Too few elements."

 aE :: [NDTYPE] -> [NDTYPE]	
 aE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((==) y x)):t
 aE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((==) y (read (show x)::Double))):t
 aE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((==) (read (show y)::Double) x)):t
 aE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((==) y x)):t
 aE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((==) y x)):t
 aE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((==) y x)):t
 aE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((==) y x)):t
 aE stack = if (length stack >= 2) 
 				then error "DataStack Error : in E. Incompatible types."
 				else error "DataStack Error : in E. Too few elements."

 aNE :: [NDTYPE] -> [NDTYPE]	
 aNE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((/=) y x)):t
 aNE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((/=) y (read (show x)::Double))):t
 aNE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((/=) (read (show y)::Double) x)):t
 aNE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((/=) y x)):t
 aNE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((/=) y x)):t
 aNE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((/=) y x)):t
 aNE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((/=) y x)):t
 aNE stack = if (length stack >= 2) 
 				then error "DataStack Error : in NE. Incompatible types."
 				else error "DataStack Error : in NE. Too few elements."


 aGT :: [NDTYPE] -> [NDTYPE]	
 aGT ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((>) y x)):t
 aGT ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((>) y (read (show x)::Double))):t
 aGT ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((>) (read (show y)::Double) x)):t
 aGT ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((>) y x)):t
 aGT ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((>) y x)):t
 aGT ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((>) y x)):t
 aGT ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((>) y x)):t
 aGT stack = if (length stack >= 2) 
 				then error "DataStack Error : in GT. Incompatible types."
 				else error "DataStack Error : in GT. Too few elements."

 aLT :: [NDTYPE] -> [NDTYPE]	
 aLT ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((<) y x)):t
 aLT ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((<) y (read (show x)::Double))):t
 aLT ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((<) (read (show y)::Double) x)):t
 aLT ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((<) y x)):t
 aLT ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((<) y x)):t
 aLT ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((<) y x)):t
 aLT ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((<) y x)):t
 aLT stack = if (length stack >= 2) 
 				then error "DataStack Error : in LT. Incompatible types"
 				else error "DataStack Error : in LT. Too few elements."

 aGE :: [NDTYPE] -> [NDTYPE]	
 aGE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((>=) y x)):t
 aGE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((>=) y (read (show x)::Double))):t
 aGE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((>=) (read (show y)::Double) x)):t
 aGE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((>=) y x)):t
 aGE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((>=) y x)):t
 aGE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((>=) y x)):t
 aGE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((>=) y x)):t
 aGE stack = if (length stack >= 2) 
 				then error "DataStack Error : in GE. Incompatible types"
 				else error "DataStack Error : in GE. Too few elements."

 aLE :: [NDTYPE] -> [NDTYPE]	
 aLE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((<=) y x)):t
 aLE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((<=) y (read (show x)::Double))):t
 aLE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((<=) (read (show y)::Double) x)):t
 aLE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((<=) y x)):t
 aLE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((<=) y x)):t
 aLE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((<=) y x)):t
 aLE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((<=) y x)):t
 aLE stack = if (length stack >= 2) 
 				then error "DataStack Error : in LE. Incompatible types"
 				else error "DataStack Error : in LE. Too few elements."
