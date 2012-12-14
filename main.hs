module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
import System.Console.Haskeline
import Data.Map(fromList)
import Control.Monad.IO.Class
import System.Directory(doesFileExist)
import System.FilePath(takeExtension,takeFileName)

--import Prelude hiding (catch)
--import Control.Exception hiding (catch,throwIO)
--import System.IO.Error hiding (catch)

main::IO ()

main = runInputT defaultSettings $ loop Program{stack = [], funcs = fromList []}
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
					outputStrLn ("loading files: " ++ (unwords $ words files)) >>  
					return (words files) >>=
					load prog >>=
					loop
				Just input ->	
					return (execute (parser input) prog) >>= 
					check
			where	
				{- just checking -}
				check::Program -> InputT IO()

				check Program{stack = ((NDTYPErr err):xs), funcs = f } =
					outputStrLn err >> 
					outputStrLn (showNew xs) >>
					loop Program{stack = xs, funcs = f}

				check prog =
					outputStrLn (showNew (stack prog)) >>
					loop prog
		{- loading file procedure -}
		load prog [] = 
			return prog
		load prog (x:xs) = do
			exists <- liftIO $ doesFileExist x
			if (exists) 
				then do
					if (takeExtension x == ".9") 
						then do
							file <- liftIO $ readFile x
							outputStrLn ("executing file: \""++ takeFileName x ++"\"")
							checkl (execute (parser file) prog) xs
						else do
							outputStrLn ("error: "++ takeFileName x ++": file format not recognized")
							outputStrLn (showNew (stack prog))
							return prog
				else do
					outputStrLn ("error: "++ takeFileName x ++": no such file or directory")
					outputStrLn (showNew (stack prog))
					return prog
			where
				{- just checking for files -}
				checkl Program{stack = ((NDTYPErr err):xs), funcs = f } _ =
					outputStrLn err >> 
					outputStrLn (showNew xs) >>
					load Program{stack = xs, funcs = f}	[]		
				checkl prog files =
					outputStrLn (showNew (stack prog)) >>
					load prog files
		{-
			liftIO ( readFile x `catch` handleExistance )>>=
			handleExistance' x prog >>=
			(\t -> checkl t xs)
			where 
				handleExistance err | idoDoesNotExistError err = return ""
						    | otherwise = throwIO err
				handleExistance' name prog [] = 
					outputStrLn ("error: file \""++name++"\" is empty or doesn't exist") >> 
					return prog
				handleExistance' name prog file = 
					outputStrLn ("executing file: \""++name++"\"") >>
					return (execute (parser file) prog)
		-}
