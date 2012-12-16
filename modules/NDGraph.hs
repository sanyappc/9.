module NDGraph where

import NDType
import NDAction
import NDActionHandlers
import NDParse
import Data.Map

--------------------------------------------------------------------------------
{- Data structure for functions implementation. -}
--------------------------------------------------------------------------------
data Func = Func { actions::[NDActionPos] }

--------------------------------------------------------------------------------
{- Data structure for program representation -}
--------------------------------------------------------------------------------
data P = P{ stack::[NDTYPE],
			tmp::[String],
			funcs::(Map String Func),
			res::(String, [((Int, Int),String)]),
			i::Integer,
			prev::String,
			owner::String
		}
--------------------------------------------------------------------------------
{-
   graph init
-}
--------------------------------------------------------------------------------
graphInit::String

graphInit = "digraph dot{\n"

--------------------------------------------------------------------------------
{-
   finalize the graph
-}
--------------------------------------------------------------------------------
-----------graph-----out--
graphEnd::String -> String

graphEnd g = g ++ "}"

--------------------------------------------------------------------------------
{- 
   special function witch using step by step execution of code. Return graph and
   some features for steper.
-}
--------------------------------------------------------------------------------
------------------src--------graph----line--col----stack---
executeByStepEx::String -> (String, [((Int, Int), String)])

executeByStepEx src =
	(heh (lets (parser src) P{	stack = [],
								tmp = [],
								funcs = fromList [],
								res = (graphInit, []),
								i = 0,
								prev = "",
								owner = ""
								}))
	where
		heh:: P -> (String, [((Int, Int), String)])
		heh P{stack = s, tmp = ts, funcs = _, res = (graph, stack), i = -1, prev = prev, owner = _} =
			((graphEnd graph), stack)

		heh P{stack = s, tmp = ts, funcs = _, res = (graph, stack), i = _, prev = _, owner = _} =
			((graphEnd graph), stack ++ [((-1, -1), showSuper (s, ts))])

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
lets::[NDActionPos] -> P -> P

lets _ P{stack = (NDTYPErr err:xs), tmp = (t:ts), funcs = f, res = (graph, stack), i = i, prev = prev, owner = owner} =
	P{stack = xs, tmp = ts, funcs = f, res = (graph, stack ++ [((-2, -2), err)]), i = -1, prev = err, owner = owner}

lets (act:acts) prog =
	lets acts (check act (execution act prog))

lets [] p =
	p

check::NDActionPos -> P -> P

check (NDActionPos _ xx yy _ _) P{stack = (NDTYPErr err:xs), tmp = ts, funcs = f, res = r, i = i, prev = prev, owner = owner} =
	P{stack = (NDTYPErr ("error: line: " ++ (show xx) ++ " col: " ++ (show yy) ++ ": " ++ err):xs),
		funcs = f,
		tmp = (owner:ts),
		res = r,
		i = i,
		prev = prev,
		owner = owner
	}

check _ p = 
	p
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
execution::NDActionPos -> P -> P
-- pop
execution (NDActionPos NDPop x y _ _) P{stack = s, tmp = (t:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aPop s,
		tmp = ts,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"pop\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (t:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- push
execution (NDActionPos (NDPush a) x y _ _) P{stack = s, tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aPush s a,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"push " ++ (showT a) ++ "\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, ts))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- swap
execution (NDActionPos NDSwap x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aSwap s,
		tmp = (owner:owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"swap\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
--dswap
execution (NDActionPos NDDSwap x y _ _) P{stack = s, tmp = (a:b:c:d:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aDSwap s,
		tmp = (owner:owner:owner:owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"double swap\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:c:d:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- rotr

---
execution (NDActionPos NDRotR x y _ _) P{stack = s, tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aRotR s,
		tmp = rot owner ts,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"rotr\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, ts))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
execution (NDActionPos NDRotL x y _ _) P{stack = s, tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aRotL s,
		tmp = rot owner ts,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"rotl\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, ts))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- dup
execution (NDActionPos NDDup x y _ _) P{stack = s, tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aDup s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"dup\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, ts))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- add
execution (NDActionPos NDAdd x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aAdd s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"+\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- sub
execution (NDActionPos NDSub x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aSub s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"-\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- divd
execution (NDActionPos DivD x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aDivD s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"/\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- div
execution (NDActionPos Div x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aDiv s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"div\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- mul
execution (NDActionPos NDMul x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aMul s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"*\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- mod
execution (NDActionPos Mod x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aMod s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"mod\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- ge
execution (NDActionPos GE x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aGE s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \">=\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- le
execution (NDActionPos LE x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aLE s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"<=\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- g
execution (NDActionPos G x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aGT s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \">\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- l
execution (NDActionPos L x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aLT s,
		tmp =(owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"<\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- e
execution (NDActionPos E x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aE s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"=\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- ne
execution (NDActionPos NE x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aNE s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"<>\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- not
execution (NDActionPos NOT x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aNot s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"not\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- and
execution (NDActionPos AND x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aAnd s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"&\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- or
execution (NDActionPos OR x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aOr s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"|\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}
-- xor
execution (NDActionPos XOR x y _ _) P{stack = s, tmp = (a:b:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{	stack = aXor s,
		tmp = (owner:ts),
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"xor\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showSuper (s, (a:b:ts)))]
				),
		i = i + 1,
		prev = "node" ++ (show i),
		owner = owner
	}

-- if statement
execution (NDActionPos (NDIf true false) x y _ _) P{stack = (s:ss), tmp = (t:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner}
	| toBool s =
		over owner (lets true P{
				stack = ss,
				tmp = ts,
				funcs = f,
				res = 	(g ++ (newCluster i "if true branch"),
						stack ++ [((x, y), showSuper ((s:ss), (t:ts)))]
						),
				i = i + 1,
				prev = prev,
				owner = owner
			})
	| otherwise =
		over owner (lets false P{
				stack = ss,
				tmp = ts,
				funcs = f,
				res = 	(g ++ (newCluster i "if false branch"),
						stack ++ [((x, y), showSuper (ss, (t:ts)))]
						),
				i = i + 1,
				prev = prev,
				owner = owner
			})		
-- add new function
execution (NDActionPos (NDNewFunction (NDTYPEf name) acts) x y _ _) P{stack = s, tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner}
	| member name f =
		P{
			stack = s,
			tmp = ts,
			funcs = Data.Map.adjust (\x -> Func{actions = acts}) name f,
			res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"declare" ++ name ++ "\"];\n" ++ (link i prev),
					stack ++ [((x, y), showSuper (s, ts))]
					),
			i = i + 1,
			prev = "node" ++ (show i),
			owner = owner
		}
	| otherwise =
		P{
			stack = s,
			tmp = ts,
			funcs = Data.Map.insert name Func{actions=acts} f,
			res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"declare" ++ name ++ "\"];\n" ++ (link i prev),
					stack ++ [((x, y), showSuper (s, ts))]
					),
			i = i + 1,
			prev = "node" ++ (show i),
			owner = owner
		}
-- call func from list...
execution (NDActionPos (NDCallFunction (NDTYPEf name)) x y _ _) P{stack = s, tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner}
	| member name f =
		over owner (lets (actions (f ! name)) P{
					stack = s,
					tmp = ts,
					funcs = f,
					res = 	(g ++ (newCluster i ("function " ++ name)),
							stack ++ [((x, y), showSuper (s, ts))]
							),
					i = i + 1,
					prev = prev,
					owner = name					
				}
			)
	| otherwise =
		P{
			stack = aPush s (NDTYPErr ("undeclared function: \"" ++ name ++ "\"")),
			tmp = (owner:ts),
			funcs = f,
			res =	(g, stack),
			i = i + 1,
			prev = prev,
			owner = owner
		}
-- exit ?!
execution (NDActionPos NDExit _ _ _ _) p =
	p
-- call from stack function
execution (NDActionPos NDSCallFunction x y xx yy) P{stack = [], tmp = ts, funcs = f, res = (g, stack), i = i, prev = prev, owner = owner} =
	P{
		stack = [NDTYPErr $ ecallf++erempty],
		tmp = [owner],
		funcs = f,
		res = (g, stack ++ [((x,y), showSuper ([], []))]),
		i = i,
		prev = prev,
		owner = owner
	}

execution (NDActionPos NDSCallFunction x y xx yy) P{stack = (s:ss), tmp = (t:ts), funcs = f, res = (g, stack), i = i, prev = prev, owner = owner}
	| isFunc s = 
		execution (NDActionPos (NDCallFunction s) x y xx yy) P{
			stack = ss,
			tmp = ts,
			funcs = f,
			res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"call function from stack\"];\n" ++ (link i prev),
					stack ++ [((x, y), showSuper (ss, (t:ts)))]
					),
			i = i + 1,
			prev = "node" ++ (show i),
			owner = owner
		}
	| otherwise =
		P{
			stack = ((NDTYPErr $ ecallf++": incompatible type"):s:ss),
			tmp = (owner:ts),
			funcs = f,
			res =	(g, stack),
			i = i + 1,
			prev = prev,
			owner = owner
		}
-- заглушка
execution _ p =
	p

--------------------------------------------------------------------------------
-- error func
--------------------------------------------------------------------------------
ecallf = "calling function from stack"
--------------------------------------------------------------------------------
-- support function for close subgraphs
--------------------------------------------------------------------------------
over::String -> P -> P
over owner P{stack = s, tmp = tmp, funcs = f, res = (g, stack), i = i, prev = prev, owner = _} =
	P{stack = s, tmp = tmp, funcs = f, res = (g ++ endCluster, stack), i = i, prev = prev, owner = owner}

--------------------------------------------------------------------------------
-- testing the value to be a bool
--------------------------------------------------------------------------------
toBool::NDTYPE -> Bool
toBool (NDTYPEb True) = True
toBool _ = False

--------------------------------------------------------------------------------
-- testing type to be a function
--------------------------------------------------------------------------------
isFunc (NDTYPEf func) = True
isFunc _ = False

--------------------------------------------------------------------------------
-- function for making subgraph (cluster)
--------------------------------------------------------------------------------
--------------index-----text-----result
newCluster::Integer -> String -> String

newCluster i text =
	"subgraph cluster" ++ (show i) ++ "{\n\tlabel = \"" ++ text ++ "\";\n"

endCluster::String

endCluster = "}\n"
--------------------------------------------------------------------------------
-- function for making relation in the graph
--------------------------------------------------------------------------------
link::Integer -> String -> String

link _ "" =
	""

link i prev =
	prev ++ "->node" ++ (show i) ++ ";\n"

--- хитрый костыль
rot::String -> [String] -> [String]

rot owner (x:xs) = 
	(owner:(rot owner xs))

rot _ [] =
	[]

--- няшный стек
showSuper::([NDTYPE], [String]) -> String

showSuper ([],[])=
	"stack is empty"
showSuper ((a:[]), (owner:[])) =
	showType a ++ (ownerPrint owner)
showSuper ((a:b), (owner:owners)) =
	showType a ++ (ownerPrint owner) ++ "\n" ++ showSuper (b, owners)

showSuper (_,[]) =
	""

showSuper ([], _) = 
	""

ownerPrint::String -> String

ownerPrint "" =
	""

ownerPrint owner =
	"\t<" ++ owner ++ ">"


showT (NDTYPEi a) = "NDTYPEi "++show a
showT (NDTYPEd a) = "NDTYPEd "++show a
showT (NDTYPEc a) = "NDTYPEc '"++ [a] ++ "'"
showT (NDTYPEs a) = "NDTYPEs \\\"" ++ (replaceString a []) ++ "\\\""
showT (NDTYPEb a) = "NDTYPEb "++show a
showT (NDTYPEf a) = "NDTYPEf "++ a
showT (NDTYPErr a) = "NDTYPErr "++ a 
