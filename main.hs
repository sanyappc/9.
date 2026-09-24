module Main where

import Runtime
import NDType
import NDParse
import NDAction
import NDActionHandlers
import System.Console.Haskeline
import System.Console.Haskeline.History(addHistoryUnlessConsecutiveDupe)
import Data.Map(fromList)
import Control.Monad.IO.Class
import System.Directory(doesFileExist)
import System.FilePath(takeExtension,takeFileName)

main::IO ()

-- history is added by hand: multiline input goes there as one line
main = runInputT defaultSettings{autoAddHistory = False} $ loop Program{stack = [], funcs = fromList []}
	where
		{- loop function -}
		loop::Program -> InputT IO()
		loop prog = do
			input <- getInputLine "> "
			case input of
				Nothing ->
					return ()
				Just "q" ->
					outputStrLn "good bye!!!"
				Just ('l':' ':files) ->
					outputStrLn ("loading files: " ++ (unwords $ map takeFileName (fparser files))) >>
					return (fparser files) >>=
					load prog prog >>=
					loop
				Just input ->
					multiline input >>=
					(\t -> return (executeCGI (parser t) prog)) >>=
					check prog
			where
			{- just checking -}
			check::Program -> Program -> InputT IO()
			check Program{stack = oldxs, funcs = oldf} Program{stack = ((NDTYPErr err):_), funcs = _ } =
				outputStrLn err >>
				outputStrLn (showNew oldxs) >>
				loop Program{stack = oldxs, funcs = oldf}
			check _ prog =
				outputStrLn (showNew (stack prog)) >>
				loop prog
			{- get multiline -}
			multiline input =
				if ((length input == 0) || (last input /= '\\'))
					then do
						getHistory >>= putHistory . addHistoryUnlessConsecutiveDupe input
						return input
					else do
						inputnew <- getInputLine "> "
						multiline (init input ++ takemultiline inputnew)
			takemultiline Nothing = []
			takemultiline (Just input) = input
			{- loading file procedure -}
			load _ prog [] =
				return prog
			load oldprog prog (x:xs) = do
				exists <- liftIO $ doesFileExist x
				if (exists)
					then do
						if (takeExtension x == ".9")
							then do
								file <- liftIO $ readFile x
								outputStrLn ("executing file: "++ takeFileName x)
								checkl oldprog (execute (parser file) prog) xs
							else do
								outputStrLn ("error: "++ takeFileName x ++": file format not recognized")
								outputStrLn (showNew (stack prog))
								return oldprog
					else do
						outputStrLn ("error: "++ takeFileName x ++": no such file or directory")
						outputStrLn (showNew (stack prog))
						return oldprog
				where
				{- just checking for files -}
				checkl Program{stack = oldxs, funcs = oldf } Program{stack = ((NDTYPErr err):_), funcs = _ } _ =
					outputStrLn err >>
					outputStrLn (showNew oldxs) >>
					return Program{stack = oldxs, funcs = oldf }
				checkl oldprog prog files =
					outputStrLn (showNew (stack prog)) >>
					load oldprog prog files
