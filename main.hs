module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
import Text.Printf 
import Data.Map
import System.Console.Haskeline

main::IO ()

main = runInputT defaultSettings $loop Program{stack = [], funcs = fromList []}
	where
		loop::Program -> InputT IO()
		loop prog  = do
			input <- getInputLine "9.: "
			case input of
				Nothing -> return ()
				Just "quit" ->	outputStrLn "9.: Good bye!!!!"
				Just "load" ->	getInputLine "9.:loading " >>=  
								(\(Just t) -> return (words t)) >>=
								(\t -> return $load prog t ) >>=
								(\t -> (outputStrLn $show (stack t)) >> loop t )
				Just input ->	return (execute (parser input) prog) >>= 
					 			(\t -> (outputStrLn $show (stack t)) >> loop t )
{-
		load::Program -> [String] -> IO Program
		load prog [] = return prog
		load prog (x:xs) =	readFile x >>=
							(\t -> return $execute (parser t) prog) >>=
							(\t -> load t xs)
-}
		load::Program -> [String] -> Program
		load prog [] = prog
		load prog (x:xs) = load (execute (fparser x) prog) xs
