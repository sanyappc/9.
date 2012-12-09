module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
import System.Environment(getArgs)
import Data.Map(fromList)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main::IO ()

main = do
	args <- getArgs
	filename <- return $ (!!) args 0
	code <- readFile filename 
	initGUI
	(Just xml) <- xmlNew "gtk.xml"
	window <- xmlGetWidget xml castToWindow "window"
	onDestroy window mainQuit
	codel <- xmlGetWidget xml castToLabel "code"
	stackl <- xmlGetWidget xml castToLabel "stack"
	prevb <- xmlGetWidget xml castToButton "prev" 
	nextb <- xmlGetWidget xml castToButton "next"
	ecur <- xmlGetWidget xml castToEntry "ecur"
	curb <- xmlGetWidget xml castToButton "bcur"
	scur <- xmlGetWidget xml castToLabel "scur"
	sline <- xmlGetWidget xml castToLabel "sline"
	scol <- xmlGetWidget xml castToLabel "scol"
	stack <- return $ runProg (parser code) Program{stack = [], funcs = fromList []}
	code <- firstmakeCode (lines code) 1 []

	(current,((a,b),pstack)) <- getStack 0 stack
	--set codel [ labelText := makeCode (a,b) code ]
	labelSetMarkup codel ("<tt>"++ makeCode (a,b) code ++"</tt>")
	set stackl [ labelText := pstack ]
	set ecur [ entryText := show current ]
	set scur [ labelText := show current ]
	set sline [ labelText := showCur a ]
	set scol [ labelText := showCur b ]
	
	onClicked nextb $ do
		current <- get scur labelText
		(current,((a,b),pstack)) <- getStack ((read current::Int)+1) stack
		--set codel [ labelText := makeCode (a,b) code ]
		labelSetMarkup codel ("<tt>"++ makeCode (a,b) code ++"</tt>")
		set stackl [ labelText := pstack ]
		set ecur [ entryText := show current ]
		set scur [ labelText := show current ]
		set sline [ labelText := showCur a ]
		set scol [ labelText := showCur b ]
		--current <- return (current + 1)
		--tcurrent <- get current entryText
		--proglist <- runGTK proglist actionlist (read tcurrent :: Int)
		--labelSetText stackl (showNew (stack (proglist !! (read tcurrent :: Int))))
		-- set stackl [ labelText := showNew (stack (proglist !! current)) ]
	onClicked prevb $ do
		current <- get scur labelText
		(current,((a,b),pstack)) <- getStack ((read current::Int)-1) stack
		set codel [ labelText := makeCode (a,b) code ]
		set stackl [ labelText := pstack ]
		set ecur [ entryText := show current ]
		set scur [ labelText := show current ]
		set sline [ labelText := showCur a ]
		set scol [ labelText := showCur b ]
	onClicked curb $ do
		current <- get ecur entryText
		(current,((a,b),pstack)) <- getStack (read current::Int) stack
		set codel [ labelText := makeCode (a,b) code ]
		set stackl [ labelText := pstack ]
		set ecur [ entryText := show current ]
		set scur [ labelText := show current ]
		set sline [ labelText := showCur a ]
		set scol [ labelText := showCur b ]
	--onClicked prevb $ do
		--current <- return (current - 1)
		--labelSetText stackl (showNew (stack (proglist !! 0)))
		-- set stackl [ labelText := showNew (stack (proglist !! current)) ]
	widgetShowAll window
	mainGUI
	
showCur (-2) = "err"
showCur (-1) = "end"
showCur a = show a
firstmakeCode [] _ result = return $ unlines result
firstmakeCode (x:code) i result =
	firstmakeCode code (i+1) (result++[(show i ++ "> " ++ x)])
makeCode (-2,-2) code = code
makeCode (-1,-1) code = code
makeCode (a,b) code =
	unlines $ (take a split) ++ [replicate ( b - 1 + plus ) ' '++"^"] ++ (drop a split)
		where
			split = lines code
			plus = length ((show a) ++ "> ")
getStack i stack =
	if (i > (length stack -1)) 
		then return (length stack -1,last stack)
		else if (i < 0) 
				then return (0, stack !! 0)
				else return (i, stack !! i)
runProg :: [NDActionPos] -> Program -> [((Int,Int),String)]
runProg _ Program{stack = (NDTYPErr err:xs), funcs = f} = [((-2,-2),err)]
runProg [] prog = [((-1,-1),showNewLn $ stack prog)]
runProg ((NDActionPos x a b c d):xs) prog =
	([((a,b),showNewLn $ stack prog)]++(runProg xs (check (NDActionPos x a b c d) (doNDAction (NDActionPos x a b c d) prog))))
{-
runGTK proglist actionlist current = do
	if (length proglist > current) 
	then return proglist
	else return $ execsome (drop (length proglist -1) (take (current+1) actionlist)) proglist
-}	
{-
execsome [] prog = prog	
execsome (x:xs) prog = execsome xs (prog++[(check x (doNDAction x (prog !! (length prog -1))))])
-}
