module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
import Text.Printf 
import Data.Map

main::IO ()
main =	printf "Today is a good day =)\n" >>
		mloop Program{stack = [], funcs = fromList [] } >>
		printf "Goodbye!\n" >>
		return ()
	where
	mloop prog = do
		printf ":"
		input <- getLine
		if input == "bye"
		then
			return ()
		else
			mread prog input >>=
			(\t -> print (stack t) >> mloop t) 
	
	mread::Program -> String -> IO Program
	mread stack input =
			return $execute (parser input) stack
