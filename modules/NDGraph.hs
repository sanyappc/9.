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
data P = P{ stack::[NDTYPE], funcs::(Map String Func), res::(String, [((Int, Int), String)]), i::Integer, prev::String}

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
	(res (lets (parser src) P{stack = [], funcs = fromList [], res = (graphInit, []), i = 0, prev = ""}))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
lets::[NDActionPos] -> P -> P

lets _ P{stack = (NDTYPErr err:xs), funcs = f, res = (graph, stack), i = i, prev = prev} =
	P{stack = xs, funcs = f, res = (graph, stack ++ [((-2, -2), err)]), i = i, prev = prev}

lets (act:acts) prog =
	lets acts (check act (execution act prog))

lets [] p =
	p

check::NDActionPos -> P -> P

check (NDActionPos _ xx yy _ _) P{stack = (NDTYPErr err:xs), funcs = f, res = r, i = i, prev = prev} =
	P{stack = (NDTYPErr ("error: line: " ++ (show xx) ++ " col: " ++ (show yy) ++ ": " ++ err):xs),
		funcs = f,
		res = r,
		i = i,
		prev = prev
	}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
execution::NDActionPos -> P -> P
-- pop
execution (NDActionPos NDPop x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aPop s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"pop\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- push
execution (NDActionPos (NDPush a) x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aPush s a,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"push\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- swap
execution (NDActionPos NDSwap x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aSwap s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"swap\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
--dswap
execution (NDActionPos NDDSwap x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aDSwap s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"double swap\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- rotr
execution (NDActionPos NDRotR x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aRotR s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"rotr\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- dup
execution (NDActionPos NDDup x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aDup s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"dup\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- add
execution (NDActionPos NDAdd x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aAdd s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"+\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- sub
execution (NDActionPos NDSub x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aSub s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"-\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- divd
execution (NDActionPos DivD x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aDivD s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"/\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- div
execution (NDActionPos Div x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aDiv s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"div\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- mod
execution (NDActionPos Mod x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aMod s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"mod\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- ge
execution (NDActionPos GE x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aGE s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \">=\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- le
execution (NDActionPos LE x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aLE s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"<=\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- g
execution (NDActionPos G x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aGT s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \">\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- l
execution (NDActionPos L x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aLT s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"<\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- e
execution (NDActionPos E x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aE s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"=\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- ne
execution (NDActionPos NE x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aNE s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"<>\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- not
execution (NDActionPos NOT x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aNot s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"not\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- and
execution (NDActionPos AND x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aAnd s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"&\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- or
execution (NDActionPos OR x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aOr s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"|\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}
-- xor
execution (NDActionPos XOR x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = aXor s,
		funcs = f,
		res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"xor\"];\n" ++ (link i prev),
				stack ++ [((x, y),
				showNew s)]
				),
		i = i + 1,
		prev = "node" ++ (show i)
	}

-- if statement
execution (NDActionPos (NDIf true false) x y _ _) P{stack = (s:ss), funcs = f, res = (g, stack), i = i, prev = prev}
	| toBool s =
		over (lets true P{
				stack = ss,
				funcs = f,
				res = 	(g ++ (newCluster i "if true branch"),
						stack ++ [((x, y), showNew ss)]
						),
				i = i + 1,
				prev = prev
			})
	| otherwise =
		over (lets false P{
				stack = ss,
				funcs = f,
				res = 	(g ++ (newCluster i "if false branch"),
						stack ++ [((x, y), showNew ss)]
						),
				i = i + 1,
				prev = prev
			})		
-- add new function
execution (NDActionPos (NDNewFunction (NDTYPEf name) acts) x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev}
	| member name f =
		P{
			stack = s,
			funcs = Data.Map.adjust (\x -> Func{actions = acts}) name f,
			res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"declare" ++ name ++ "\"];\n" ++ (link i prev),
					stack ++ [((x, y), showNew s)]
					),
			i = i + 1,
			prev = "node" ++ (show i)
		}
	| otherwise =
		P{
			stack = s,
			funcs = Data.Map.insert name Func{actions=acts} f,
			res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"declare" ++ name ++ "\"];\n" ++ (link i prev),
					stack ++ [((x, y), showNew s)]
					),
			i = i + 1,
			prev = "node" ++ (show i)
		}
-- call func from list...
execution (NDActionPos (NDCallFunction (NDTYPEf name)) x y _ _) P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev}
	| member name f =
		over (
				lets (actions (f ! name)) P{
					stack = s,
					funcs = f,
					res = 	(g ++ (newCluster i ("function " ++ name)),
							stack ++ [((x, y), showNew s)]
							),
					i = i + 1,
					prev = prev					
				}
			)
	| otherwise =
		P{
			stack = aPush s (NDTYPErr ("undeclared function: \"" ++ name ++ "\"")),
			funcs = f,
			res =	(g, stack),
			i = i + 1,
			prev = prev
		}
-- exit ?!
execution (NDActionPos NDExit _ _ _ _) p =
	p
-- call from stack function
execution (NDActionPos NDSCallFunction x y xx yy) P{stack = [], funcs = f, res = (g, stack), i = i, prev = prev} =
	P{
		stack = [NDTYPErr $ ecallf++erempty],
		funcs = f,
		res = (g, stack ++ [((x,y), showNew [])]),
		i = i,
		prev = prev
	}

execution (NDActionPos NDSCallFunction x y xx yy) P{stack = (s:ss), funcs = f, res = (g, stack), i = i, prev = prev}
	| isFunc s = 
		execution (NDActionPos (NDCallFunction s) x y xx yy) P{
			stack = ss,
			funcs = f,
			res = 	(g  ++ "\tnode" ++ (show i) ++ "[label = \"call function from stack\"];\n" ++ (link i prev),
					stack ++ [((x, y), showNew ss)]
					),
			i = i + 1,
			prev = "node" ++ (show i)
		}
	| otherwise =
		P{
			stack = ((NDTYPErr $ ecallf++": incompatible type"):s:ss),
			funcs = f,
			res =	(g, stack),
			i = i + 1,
			prev = prev
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
over::P -> P
over P{stack = s, funcs = f, res = (g, stack), i = i, prev = prev} =
	P{stack = s, funcs = f, res = (g ++ endCluster, stack), i = i, prev = prev}

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
	"\tnode" ++ (show (i - 1)) ++ "->node" ++ (show i) ++ ";\n"
