module Main where

import Runtime
import NDType
import NDParse
import NDAction
import NDActionHandlers
import Data.Map(fromList)
import System.IO(hSetBuffering,stdin,BufferMode (NoBuffering))
import Data.ByteString.Char8 as C (pack,unpack,null,drop,length,breakSubstring) 
import Network.CGI(runCGI,handleErrors,getInput,liftIO,output,setHeader,getInputFilename,setCookie,newCookie,getCookie)
import Text.StringTemplate(newSTMP,toString,setManyAttrib)

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain = do
	liftIO $ hSetBuffering stdin NoBuffering
	isExec <- getInput "code_exec"
	templates <- execCode isExec
	isFileLoad <- getInput "file_load"
	newfile <- fileLoad isFileLoad templates -- just sets cookies
	isFileDelete <- getInput "file_delete"
	deletefile <- fileDelete isFileDelete newfile
	makehtml deletefile
	
fileLoad (Just _) templates = do
	fileName <- getInputFilename "file"
	file <- getInput "file"
	fileNumber <- getInput "file_load_radio"
	newfile <- makeCookie fileNumber fileName file templates
	liftIO $ return newfile
fileLoad _ templates = liftIO $ return templates

fileDelete (Just _) templates = do
	fileNumber <- getInput "file_delete_radio"
	deletefile <- unmakeCookie fileNumber templates
	liftIO $ return deletefile
fileDelete _ templates = liftIO $ return templates
	
unmakeCookie (Just fileNumber) templates = do
	setCookie (newCookie (fileNumber) (show ("","")))
	liftIO $ return (filter (\t -> (fst t /= fileNumber)) templates)
unmakeCookie _ templates = liftIO $ return templates

makeCookie (Just _) (Just "") (Just _) templates = liftIO $ return templates
makeCookie (Just fileNumber) (Just fileName) (Just file) templates = do
	setCookie (newCookie (fileNumber) (show (fileName,file)))
	liftIO $ return ((filter (\t -> (fst t /= fileNumber)) templates) ++ [(fileNumber,fileName)])
makeCookie _ _ _ templates = liftIO $ return templates

execCode (Just _) = do
	filesLoaded <- takeCookies
	code <- getInput "code"
	templates <- liftIO $ exec (map (\t-> snd t) filesLoaded) code
	liftIO $ return (("code", fst templates):("result", snd templates):(map (\(a,b) -> (a, fst b)) filesLoaded))
execCode _ = do
	filesLoaded <- takeCookies
	codeJust <- getInput "code"
	code <- liftIO (codeFromJust codeJust)
	liftIO $ return (("code",code):("result",""):(map (\(a,b) -> (a, fst b)) filesLoaded))
codeFromJust (Just code) = return code
codeFromJust _ = return ""
exec files (Just code) =
	loop Program{stack = [], funcs = fromList []} files (tokenise (C.pack "\r\n") (C.pack code)) [] []
		where 
			loop Program{stack = ((NDTYPErr err):xs)} _ codelines tcode tresult =
				return (ec codelines tcode,tresult)
			loop prog _ [] tcode tresult = return (tcode, tresult)
			loop prog (file:files) codelines [] [] = 
				return (execute (parser (snd file)) prog) >>=
				(\t -> loop t files codelines [] (erf (fst file) (stack t)))
			loop prog [] ("":codelines) tcode tresult = loop prog [] codelines tcode tresult
			loop prog [] (codeline:codelines) [] [] =
				return (execute (parser codeline) prog) >>=
				(\t -> loop t [] codelines codeline (erl (stack t) []))
			loop prog [] (codeline:codelines) tcode tresult =
				return (execute (parser codeline) prog) >>=
				(\t -> loop t [] codelines (tcode++"&#13;&#10;"++codeline) (erl (stack t) tresult))
			ec [] tcode = tcode
			ec (codeline:codelines) [] = ec codelines codeline
			ec (codeline:codelines) tcode = ec codelines (tcode++"&#13;&#10;"++codeline)
			erf filename ((NDTYPErr err):_) = "file: "++ filename ++": "++ err
			erf _ _ = ""
			erl ((NDTYPErr err):_) [] = err
			erl ((NDTYPErr err):_) tresult = tresult ++"&#13;&#10;"++err
			erl stack [] = showNew stack
			erl stack tresult = tresult++"&#13;&#10;"++showNew stack
exec _ _ = return ("","")

tokenise substring string = (C.unpack head) : if C.null tail then [] else tokenise substring (C.drop (C.length substring) tail)
		where (head,tail) = C.breakSubstring substring string

takeCookies = do
	cookie1 <- getCookie ("file1")
	value1 <- liftIO $ takeOneCookie' cookie1
	cookie2 <- getCookie ("file2")
	value2 <- liftIO $ takeOneCookie' cookie2
	cookie3 <- getCookie ("file3")
	value3 <- liftIO $ takeOneCookie' cookie3
	cookie4 <- getCookie ("file4")
	value4 <- liftIO $ takeOneCookie' cookie4
	cookie5 <- getCookie ("file5")
	value5 <- liftIO $ takeOneCookie' cookie5
	liftIO $ return [("file1",value1),("file2",value2),("file3",value3),("file4",value4),("file5",value5)]
takeOneCookie' (Just value) = return (read value :: (String,String))
takeOneCookie' _ = return ("","")

makehtml text = do
	html <- liftIO $ untemplate text
	setHeader "Content-type" "text/html"
	output html
untemplate text = do
	templateText <- readFile "template.html"
	let template = newSTMP templateText
	return (toString $ setManyAttrib text template)
