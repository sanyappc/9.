module Main where

import System.Environment(getArgs,getProgName)
import System.Directory(doesFileExist)
import System.FilePath(takeExtension,takeFileName)

import NDGraph(executeByStepEx)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main::IO ()
main = do
	args <- getArgs
	name <- getProgName
	if (length args < 1) 
		then error ("error: no input file specified!\nusage: " ++ name ++ " filename")
		else return ()
	filename <- return $ (!!) args 0
	exists <- doesFileExist filename
	if (not exists)
		then error ("error: " ++ takeFileName filename ++": no such file or directory")
		else return()
	if (takeExtension filename /= ".9")
		then error ("error: " ++ takeFileName filename ++": file format not recognized")
		else return()
	code <- readFile filename
	-- gui initialisation
	initGUI
	xml <- xmlNew "gtk.xml"
	xml <- case xml of
		Nothing -> error "error: interface file gtk.xml was not found!"
		Just xml -> return xml
	window <- xmlGetWidget xml castToWindow "window"
	onDestroy window mainQuit
	windowSetTitle window ("9. stepper: " ++ takeFileName filename)
	pxm <- pixbufNewFromXPMData logo
	windowSetIcon window (Just pxm)
	codel <- xmlGetWidget xml castToLabel "code"
	stackl <- xmlGetWidget xml castToLabel "stack"
	prevb <- xmlGetWidget xml castToButton "prev" 
	nextb <- xmlGetWidget xml castToButton "next"
	ecur <- xmlGetWidget xml castToEntry "ecur"
	curb <- xmlGetWidget xml castToButton "bcur"
	scur <- xmlGetWidget xml castToLabel "scur"
	sall <- xmlGetWidget xml castToLabel "sall"
	sline <- xmlGetWidget xml castToLabel "sline"
	scol <- xmlGetWidget xml castToLabel "scol"
	stack <- return $ snd (executeByStepEx code)
	-- let's go to first step
	code <- firstmakeCode (lines code) 1 []
	set sall [ labelText := show (length stack - 1) ]
	setStep (show 0) 0 stack code codel stackl ecur scur sline scol
	-- action handlers
	onClicked nextb $ do
		current <- get scur labelText
		setStep current 1 stack code codel stackl ecur scur sline scol
	onClicked prevb $ do
		current <- get scur labelText
		setStep current (-1) stack code codel stackl ecur scur sline scol
	onClicked curb $ do
		current <- get ecur entryText
		setStep current 0 stack code codel stackl ecur scur sline scol
	afterEntryActivate ecur $ do
		current <- get ecur entryText
		setStep current 0 stack code codel stackl ecur scur sline scol
	widgetShowAll window
	mainGUI
-- setStep sets the new step	
setStep current i stack code codel stackl ecur scur sline scol = do
	(current,((a,b),pstack)) <- getStack ((read current::Int)+i) stack
	--set codel [ labelText := makeCode (a,b) code ]
	labelSetMarkup codel ("<tt>"++ makeCode (a,b) code ++"</tt>")
	set stackl [ labelText := pstack ]
	set ecur [ entryText := show current ]
	set scur [ labelText := show current ]
	set sline [ labelText := showCur a ]
	set scol [ labelText := showCur b ]
-- showCur shows current posotion in the code.
showCur (-2) = "err"
showCur (-1) = "end"
showCur a = show a
-- firstmakeCode appends line numbers in the code.
firstmakeCode [] _ result = return $ unlines result
firstmakeCode (x:code) i result =
	firstmakeCode code (i+1) (result++[(show i ++ "> " ++ x)])
-- makeCode sets ^ to the current position in the code.
makeCode (-2,-2) code = code
makeCode (-1,-1) code = code
makeCode (a,b) code =
	unlines $ (take a split) ++ [replicate ( b - 1 + plus ) ' '++"^"] ++ (drop a split)
		where
			split = lines code
			plus = length ((show a) ++ "> ")
-- getStack gets the i state in stack.
getStack i stack =
	if (i > (length stack -1)) 
		then return (length stack -1,last stack)
		else if (i < 0) 
				then return (0, stack !! 0)
				else return (i, stack !! i)
				
--it's the old version of first-level executing without owners.
--runProg :: [NDActionPos] -> Program -> [((Int,Int),String)]
--runProg _ Program{stack = (NDTYPErr err:xs), funcs = f} = [((-2,-2),err)]
--runProg [] prog = [((-1,-1),showNewLn $ stack prog)]
--runProg ((NDActionPos x a b c d):xs) prog =
--	([((a,b),showNewLn $ stack prog)]++(runProg xs (check (NDActionPos x a b c d) (doNDAction (NDActionPos x a b c d) prog))))

-- logo just for fun
logo = ["35 35 11 1",
	" \tc None",
	".\tc #0E1897",
	"+\tc #0E1898",
	"@\tc #0F1897",
	"#\tc #0E1997",
	"$\tc #0E1896",
	"%\tc #0F1998",
	"&\tc #0E1998",
	"*\tc #0F1898",
	"=\tc #0F1997",
	"-\tc #0F1996",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                .+@#@$             ",
	"              #%%%%%%%%.           ",
	"            .%%%%%%%%%%%$          ",
	"            %%%%%%%%%%%%%          ",
	"           &%%%%+   *%%%%.         ",
	"           %%%%.     %%%%@         ",
	"          &%%%%      %%%%&         ",
	"          %%%%%      %%%%*         ",
	"          =%%%%     =%%%%.         ",
	"          *%%%%.   .%%%%%.         ",
	"          +%%%%%%%%%%%%%%=         ",
	"           =%%%%%%%%%%%%%          ",
	"            @%%%%%%++%%%#          ",
	"             .&=#%  +%%%=          ",
	"                   =%%%.           ",
	"          .      @+%%%%      %%%   ",
	"          %%%%%%%%%%%%%      %%%   ",
	"         @%%%%%%%%%%%.      -%%%   ",
	"         +%%%%%%%%%+        %%%%   ",
	"           @+@+&%#          =%%%   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   ",
	"                                   "]
