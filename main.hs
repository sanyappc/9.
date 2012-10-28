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
	mloop prog =
		printf ":" >>
		getLine >>=
		(\t -> handle prog t) >>=
		mloop
	{- старая версия повесит по принципу трёх обновлений.
	mloop prog = do
		printf ":"
		input <- getLine
		if input == ":q"
		then
			return ()
		else
			mread prog input >>=
			(\t -> print (stack t) >> mloop t) 
	-}	

	mread::Program -> String -> IO Program
	mread stack input =
			return $execute (parser input) stack

	------------------------------------------------------------------------------
	-- handle func
	------------------------------------------------------------------------------
	handle::Program -> String -> IO Program
	handle prog ":q" = error "Покедова!"
	-- сделать нормальный парсер файлов надо.
	handle prog ":l" =
		printf ":" >>
		getLine >>=
		readFile >>=
		exec
		where
			exec input =
				 mread prog input >>=
				 (\t -> print (stack t) >> return t)
	handle prog input = 
		mread prog input >>= 
            (\t -> print (stack t) >> return t)
