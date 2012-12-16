module Main where

import NDGraph(executeByStepEx)
import System.Environment(getArgs,getProgName)
import System.Directory(doesFileExist)
import System.FilePath(takeExtension,takeFileName)

main::IO ()
main = do
	args <- getArgs
	name <- getProgName
	if (length args < 1) 
		then error ("error: no input file specified!\nusage: " ++ name ++ " filename")
		else return ()
	filename <- return $ (!!) args 0
	exists <- doesFileExist filename
	if (not exists)
		then error ("error: " ++ takeFileName filename ++": no such file or directory")
		else return()
	if (takeExtension filename /= ".9")
		then error ("error: " ++ takeFileName filename ++": file format not recognized")
		else return()
	code <- readFile filename
	putStrLn (fst (executeByStepEx code)) 


--import System.Process
--import System.Exit
--import Codec.Picture.Png
--import Data.ByteString.Char8(pack)
--runGraphViz graph = do 
--	(exit,out,err) <- readProcessWithExitCode "dot" ["-Tsvg"] graph
--	writeFile "graph.svg" out
--	decodePng (pack out)
--	svgNewFromHandle svg
