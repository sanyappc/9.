module Main where

import Runtime 
import NDType
import NDParse
import NDAction
import NDActionHandlers
import Text.Printf 
import NDType

main::IO ()
main =	printf "Today is a good day =)\n" >>
		mloop [] >>
		printf "Goodbye!\n" >>
		return ()
	where
	mloop stack = do
		printf ":"
		input <- getLine
		if input == "bye"
		then
			return ()
		else
			mread stack input >>=
			(\t -> print t >> mloop t) 
	
	mread::[NDTYPE] -> String -> IO [NDTYPE]
	mread stack input =
			return $execute (parser input) stack
