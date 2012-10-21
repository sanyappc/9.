module Main where

import Runtime 
import Text.Printf 
import NDType

main::IO ()

test::String
test = getLine >>= return

tst::String
tst = do
	input <- getLine
	return input

main =
	printf "Today is a good day =)\n" >>
--	return(tloop []) >>
	printf "Goodbye!\n"
--	where
--		tloop::[NDTYPE] -> [NDTYPE]
--		tloop stack = 
--			printf ":" >>
--			getLine >>= return (

