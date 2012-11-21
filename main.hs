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

main::IO ()

main = runInputT defaultSettings $loop Program{stack = [], funcs = fromList []}
	where
		loop::Program -> InputT IO()
		loop Program{stack = ((NDTYPErr err):xs), funcs = f } = outputStrLn err >> 
			loop Program{stack = xs, funcs = f}
		loop prog  = do
			outputStrLn $ showNew (stack prog)
			input <- getInputLine "9.: "
			case input of
				Nothing -> return ()
				Just "q" ->	outputStrLn "9.: Good bye!!!!"
				Just ('l':' ':files) ->	outputStrLn "9.:loading files" >>  
								return (words files) >>=
								(\t -> return $ load prog t ) >>=
								(\t -> liftIO t) >>=
								loop
--								(\t -> (outputStrLn $ showNew (stack t)) >> loop t )
				Just input ->	return (execute (parser input) prog) >>= 
								loop
--					 			(\t -> (outputStrLn $ showNew (stack t)) >> loop t )

		load::Program -> [String] -> IO Program
		load prog [] = return prog
		load prog (x:xs) =	readFile x >>=
							(\t -> return $execute (parser t) prog) >>=
							(\t -> load t xs)
{-
		load::Program -> [String] -> Program
		load prog [] = prog
		load prog (x:xs) = load (execute (fparser x) prog) xs
-}
