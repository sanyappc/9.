module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
--import System.Console.Haskeline
import System.Console.Readline
import Data.Map(fromList)
import Control.Monad.IO.Class
import System.Directory(doesFileExist)
import System.FilePath(takeExtension,takeFileName)

main::IO ()

--main = runInputT defaultSettings $ loop Program{stack = [], funcs = fromList []}
main = loop Program{stack = [], funcs = fromList []}
	where
		{- loop function -}
		--loop::Program -> InputT IO()
		loop::Program -> IO()
		loop prog = do
			--input <- getInputLine "> "
			input <- readline "> "
			case input of
				Nothing -> 
					return ()
				Just "q" -> 
					--outputStrLn "good bye!!!"
					putStrLn "good bye!!!"
				Just ('l':' ':files) ->	
					--outputStrLn ("loading files: " ++ (unwords $ map takeFileName (fparser files))) >>  
					putStrLn ("loading files: " ++ (unwords $ map takeFileName (fparser files))) >>  
					return (fparser files) >>=
					load prog prog >>=
					loop
				Just input ->	
					multiline input >>=
					(\t -> return (executeCGI (parser t) prog)) >>= 
					check prog
			where	
			{- just checking -}
			--check::Program -> InputT IO()
			check::Program -> Program -> IO()
			check Program{stack = oldxs, funcs = oldf} Program{stack = ((NDTYPErr err):_), funcs = _ } =
				--outputStrLn err >> 
				putStrLn err >> 
				--outputStrLn (showNew xs) >>
				putStrLn (showNew oldxs) >>
				loop Program{stack = oldxs, funcs = oldf}
			check _ prog =
				--outputStrLn (showNew (stack prog)) >>
				putStrLn (showNew (stack prog)) >>
				loop prog
			{- get multiline -}	
			multiline input = 
				if ((length input == 0) || (last input /= '\\'))
					then do
						addHistory input
						return input
					else do
						--inputnew <- getInputLine "> "
						inputnew <- readline "> "
						multiline (init input ++ takemultiline inputnew)
			takemultiline Nothing = []
			takemultiline (Just input) = input	
			{- loading file procedure -}
			load _ prog [] = 
				return prog
			load oldprog prog (x:xs) = do
				--exists <- liftIO $ doesFileExist x
				exists <- doesFileExist x
				if (exists) 
					then do
						if (takeExtension x == ".9") 
							then do
								--file <- liftIO $ readFile x
								file <- readFile x
								--outputStrLn ("executing file: "++ takeFileName x)
								putStrLn ("executing file: "++ takeFileName x)
								checkl oldprog (execute (parser file) prog) xs
							else do
								--outputStrLn ("error: "++ takeFileName x ++": file format not recognized")
								--outputStrLn (showNew (stack prog))
								putStrLn ("error: "++ takeFileName x ++": file format not recognized")
								putStrLn (showNew (stack prog))
								return oldprog
					else do
						--outputStrLn ("error: "++ takeFileName x ++": no such file or directory")
						--outputStrLn (showNew (stack prog))
						putStrLn ("error: "++ takeFileName x ++": no such file or directory")
						putStrLn (showNew (stack prog))
						return oldprog
				where
				{- just checking for files -}
				checkl Program{stack = oldxs, funcs = oldf } Program{stack = ((NDTYPErr err):_), funcs = _ } _ =
					--outputStrLn err >> 
					--outputStrLn (showNew xs) >>
					putStrLn err >> 
					putStrLn (showNew oldxs) >>
					return Program{stack = oldxs, funcs = oldf }	
				checkl oldprog prog files =
					--outputStrLn (showNew (stack prog)) >>
					putStrLn (showNew (stack prog)) >>
					load oldprog prog files
