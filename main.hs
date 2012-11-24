module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
import Text.Printf 
import System.Console.Haskeline
import Data.Map
import Network.CGI

import Prelude hiding (catch)
import System.Directory
import Control.Exception hiding (catch,throwIO)
import System.IO.Error hiding (catch)

main::IO ()

main = runInputT defaultSettings $ loop Program{stack = [], funcs = fromList []}
	where
		loop::Program -> InputT IO()
		loop Program{stack = ((NDTYPErr err):xs), funcs = f } = outputStrLn err >> 
			loop Program{stack = xs, funcs = f}
		loop prog = do
			outputStrLn $ showNew (stack prog)
			input <- getInputLine "9.: "
			case input of
				Nothing -> return ()
				Just "q" ->	outputStrLn "9.: Good bye!!!!"
				Just ('l':' ':files) ->	
								outputStrLn ("9.: Loading files: " ++ (unwords $ words files)) >>  
								return (words files) >>=
								load prog >>=
								loop
--								(\t -> (outputStrLn $ showNew (stack t)) >> loop t )
				Just input ->	return (execute (parser input) prog) >>= 
								loop
--					 			(\t -> (outputStrLn $ showNew (stack t)) >> loop t )

		load prog [] = return prog
		load prog (x:xs) =	liftIO ( readFile x `catch` handleExistance )>>=
							handleExistance' x prog >>=
							(\t -> load t xs)
					where 
						  handleExistance err | isDoesNotExistError err = return ""
											  | otherwise = throwIO err
						  handleExistance' name prog [] = outputStrLn ("File \""++name++"\" is empty or doesn't exist") >> 
														  return prog
						  handleExistance' name prog file = outputStrLn ("Executing file \""++name++"\"") >>
														    return (execute (parser file) prog)
														    
