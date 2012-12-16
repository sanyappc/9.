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

erempty = ": empty stack"
ermism = ": type mismatch"
ernen = ": not enough of elements"
erunkn = ": unknown error"

--------------------------------------------------------------------------------
--	1. Операции с элементами на стеке
--------------------------------------------------------------------------------
--добавление элемента в стек
aPush :: [NDTYPE] -> NDTYPE -> [NDTYPE]
aPush stack x = x:stack

--дублирование последнего элемента на стеке
aDup :: [NDTYPE] -> [NDTYPE]
aDup [] = [NDTYPErr $ "dup"++erempty]
aDup (h:t) = h:h:t
 
aPop :: [NDTYPE] -> [NDTYPE] 
aPop [] = [NDTYPErr $ "pop"++erempty]
aPop (h:t) = t

aRotR :: [NDTYPE] -> [NDTYPE]
aRotR [] = [NDTYPErr $ "rotr"++erempty]
aRotR (h:t) = t ++ [h]

aRotL :: [NDTYPE] -> [NDTYPE] 
aRotL [] = [NDTYPErr $ "rotl"++erempty]
aRotL stack = (last stack):(init stack)  

aSwap :: [NDTYPE] -> [NDTYPE]
aSwap [] = [NDTYPErr $ "swap"++erempty]
aSwap (x:y:t) = y:x:t
aSwap stack = (NDTYPErr $ "swap"++ernen):stack

aDSwap :: [NDTYPE] -> [NDTYPE]
aDSwap [] = [NDTYPErr $ "dswap"++erempty]
aDSwap (a:b:c:d:t) = (c:d:a:b:t)
aDSwap stack = (NDTYPErr $ "dswap"++ernen):stack

--------------------------------------------------------------------------------
--	2. Функции, возвращающищие значения типа Int 
--------------------------------------------------------------------------------

ediv = "div"
aDiv :: [NDTYPE] -> [NDTYPE]
aDiv [] = [NDTYPErr $ ediv++erempty]
aDiv [x] = (NDTYPErr $ ediv++ernen):[x]
aDiv ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEi (div y x):t)
aDiv stack = (NDTYPErr $ ediv++ermism):stack 

emod = "mod"
aMod :: [NDTYPE] -> [NDTYPE]
aMod [] = [NDTYPErr $ emod++erempty]
aMod [x] = (NDTYPErr $ emod++ernen):[x]
aMod ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEi (mod y x):t)
aMod stack = (NDTYPErr $ emod++ermism):stack 

---------------------------------------------------------------------------------
--	3. Функции, возвращающие значения типа Int и Double
--------------------------------- -----------------------------------------------
 
castToDouble :: String -> (Double -> Double -> Double) -> NDTYPE -> NDTYPE -> [NDTYPE]
castToDouble _ f (NDTYPEi x) (NDTYPEi y) = [NDTYPEd (f (read (show x)::Double) (read (show y)::Double))]
castToDouble _ f (NDTYPEi x) (NDTYPEd y) = [NDTYPEd (f (read (show x)::Double) y)]
castToDouble _ f (NDTYPEd x) (NDTYPEi y) = [NDTYPEd (f x (read (show y)::Double))]
castToDouble _ f (NDTYPEd x) (NDTYPEd y) = [NDTYPEd (f x y)]
castToDouble a f x y = (NDTYPErr $ a++ermism):y:x:[] 
 
eadd = "+"
aAdd :: [NDTYPE] -> [NDTYPE]
aAdd [] = [NDTYPErr $ eadd++erempty]
aAdd [x] = (NDTYPErr $ eadd++ernen):[x]
aAdd ((NDTYPEc x):(NDTYPEc y):t) = ((NDTYPEs $ y:x:[]):t)
aAdd ((NDTYPEc x):(NDTYPEs y):t) = ((NDTYPEs $ y++[x]):t)
aAdd ((NDTYPEs x):(NDTYPEc y):t) = ((NDTYPEs $ y:x):t)
aAdd ((NDTYPEs x):(NDTYPEs y):t) = ((NDTYPEs $ y ++ x):t)
aAdd ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi $ y + x):t)
aAdd (x:y:t) = (castToDouble eadd (+) y x) ++ t

esub = "-"
aSub :: [NDTYPE] -> [NDTYPE]
aSub [] = [NDTYPErr $ esub++erempty]
aSub [x] = (NDTYPErr $ esub++ernen):[x]
aSub ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((-)y x)):t)
aSub (x:y:t) = (castToDouble esub (-) y x) ++ t

emul = "*"
aMul :: [NDTYPE] -> [NDTYPE]
aMul [] = [NDTYPErr $ emul++erempty]
aMul [x] = (NDTYPErr $ emul++ernen):[x]
aMul ((NDTYPEi x):(NDTYPEi y):t) = ((NDTYPEi((*)y x)):t)
aMul (x:y:t) = (castToDouble emul (*) y x) ++ t

edivd = "/"
aDivD :: [NDTYPE] -> [NDTYPE]
aDivD [] = [NDTYPErr $ edivd++erempty]
aDivD [x] = (NDTYPErr $ edivd++ernen):[x]
aDivD (x:y:t) = (castToDouble edivd (/) y x) ++ t

-------------------------------------------------------------------------------
--
--  4. функции, возвращающие значетие типа Bool
--
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- функция сравнения двух элементов 
--	операции вида : >, <, <=, >=, != ...
--	возвращает значение типа Boolean
-------------------------------------------------------------------------------
aNot :: [NDTYPE] -> [NDTYPE]
aNot [] = [NDTYPErr $ "~"++erempty]
aNot ((NDTYPEi x):t) = (NDTYPEi (-x)):t -- отрицание
aNot ((NDTYPEd x):t) = (NDTYPEd (-x)):t	
aNot ((NDTYPEb x):t) = (NDTYPEb (not x)):t
aNot stack = (NDTYPErr $ "~"++ermism):stack 

eand = "&&"
aAnd :: [NDTYPE] -> [NDTYPE]
aAnd [] = [NDTYPErr $ eand++erempty]
aAnd [x] = (NDTYPErr $ eand++ernen):[x]
aAnd ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((&&) y x)):t
aAnd stack = (NDTYPErr $ eand++ermism):stack

eor = "||"
aOr :: [NDTYPE] -> [NDTYPE]
aOr [] = [NDTYPErr $ eor++erempty]
aOr [x] = (NDTYPErr $ eor++ernen):[x]
aOr ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((||) y x)):t
aOr stack = (NDTYPErr $ eor++ermism):stack


exor = "xor"
aXor :: [NDTYPE] -> [NDTYPE]
aXor [] = [NDTYPErr $ exor++erempty]
aXor [x] = (NDTYPErr $ exor++ernen):[x]
aXor ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((/=) y x)):t
aXor stack = (NDTYPErr $ exor++ermism):stack

ee = "=="
aE :: [NDTYPE] -> [NDTYPE]	
aE [] = [NDTYPErr $ ee++erempty]
aE [x] = (NDTYPErr $ ee++ernen):[x]
aE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((==) y x)):t
aE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((==) y (read (show x)::Double))):t
aE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((==) (read (show y)::Double) x)):t
aE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((==) y x)):t
aE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((==) y x)):t
aE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((==) y x)):t
aE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((==) y x)):t
aE stack = (NDTYPErr $ ee++ermism):stack

ene = "<>"
aNE :: [NDTYPE] -> [NDTYPE]	
aNE [] = [NDTYPErr $ ene++erempty]
aNE [x] = (NDTYPErr $ ene++ernen):[x]
aNE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((/=) y x)):t
aNE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((/=) y (read (show x)::Double))):t
aNE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((/=) (read (show y)::Double) x)):t
aNE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((/=) y x)):t
aNE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((/=) y x)):t
aNE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((/=) y x)):t
aNE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((/=) y x)):t
aNE stack = (NDTYPErr $ ene++ermism):stack

egt = ">"
aGT :: [NDTYPE] -> [NDTYPE]	
aGT [] = [NDTYPErr $ egt++erempty]
aGT [x] = (NDTYPErr $ egt++ernen):[x]
aGT ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((>) y x)):t
aGT ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((>) y (read (show x)::Double))):t
aGT ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((>) (read (show y)::Double) x)):t
aGT ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((>) y x)):t
aGT ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((>) y x)):t
aGT ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((>) y x)):t
aGT ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((>) y x)):t
aGT stack = (NDTYPErr $ egt++ermism):stack

elt = "<"
aLT :: [NDTYPE] -> [NDTYPE]	
aLT [] = [NDTYPErr $ elt++erempty]
aLT [x] = (NDTYPErr $ elt++ernen):[x]
aLT ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((<) y x)):t
aLT ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((<) y (read (show x)::Double))):t
aLT ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((<) (read (show y)::Double) x)):t
aLT ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((<) y x)):t
aLT ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((<) y x)):t
aLT ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((<) y x)):t
aLT ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((<) y x)):t
aLT stack = (NDTYPErr $ elt++ermism):stack

ege = ">="
aGE :: [NDTYPE] -> [NDTYPE]	
aGE [] = [NDTYPErr $ ege++erempty]
aGE [x] = (NDTYPErr $ ege++ernen):[x]
aGE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((>=) y x)):t
aGE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((>=) y (read (show x)::Double))):t
aGE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((>=) (read (show y)::Double) x)):t
aGE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((>=) y x)):t
aGE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((>=) y x)):t
aGE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((>=) y x)):t
aGE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((>=) y x)):t
aGE stack = (NDTYPErr $ ege++ermism):stack

ele = "<="
aLE :: [NDTYPE] -> [NDTYPE]	
aLE [] = [NDTYPErr $ ele++erempty]
aLE [x] = (NDTYPErr $ ele++ernen):[x]
aLE ((NDTYPEi x):(NDTYPEi y):t) = (NDTYPEb ((<=) y x)):t
aLE ((NDTYPEi x):(NDTYPEd y):t) = (NDTYPEb ((<=) y (read (show x)::Double))):t
aLE ((NDTYPEd x):(NDTYPEi y):t) = (NDTYPEb ((<=) (read (show y)::Double) x)):t
aLE ((NDTYPEd x):(NDTYPEd y):t) = (NDTYPEb ((<=) y x)):t
aLE ((NDTYPEc x):(NDTYPEc y):t) = (NDTYPEb ((<=) y x)):t
aLE ((NDTYPEs x):(NDTYPEs y):t) = (NDTYPEb ((<=) y x)):t
aLE ((NDTYPEb x):(NDTYPEb y):t) = (NDTYPEb ((<=) y x)):t
aLE stack = (NDTYPErr $ ele++ermism):stack
