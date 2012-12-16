{-
 - Module : NDParse.hs
 - Description : Модуль для парсинга.
 - Stability : experimental
-}
module NDParse(parser,fparser) where

import Text.ParserCombinators.Parsec ( parse, Parser, manyTill,
                                       try, eof, string, char,
                                       digit, many, many1, anyChar,
                                       noneOf, skipMany, newline,
                                       tab, space, (<|>), lookAhead, choice)                                     
import Text.Parsec.Error (errorMessages, errorPos,Message(SysUnExpect),Message(UnExpect),Message(Message))
import Text.Parsec.Prim (parsecMap,getPosition)
import Text.Parsec.Pos (sourceColumn,sourceLine)

import NDType
import NDAction

parser :: String -> [NDActionPos]
parser string = case ( parse parser' "" string ) of
                     Left err -> [NDActionPos (NDPush (NDTYPErr $ makeErr (errorMessages err))) (sourceLine (errorPos err)) (sourceColumn (errorPos err)) (-1) (-1)]
                     Right xs -> xs
makeErr ((SysUnExpect err):_) = "unexpected input" ++ makeErr' err
makeErr ((UnExpect err):_) = "unexpected item" ++ makeErr' err
makeErr ((Message err):_) = "unknown error" ++ makeErr' err
makeErr (_:xs) = makeErr xs
makeErr _ = ": unknown error"
makeErr' [] = ""
makeErr' err = ": " ++ err
 
parser' :: Parser [NDActionPos]
parser' = do
          skip
          tmp <- manyTill skipper (try eof)
          return tmp
skipper :: Parser NDActionPos
skipper = do
          start <- getPosition
          tmp <- try actions <|> types
          finish <- getPosition
          skip
          return (NDActionPos tmp (sourceLine start) (sourceColumn start) (sourceLine finish) (sourceColumn finish))
types :: Parser NDAction
types = do
        tmp <- try pdigits <|> 
               try pbool <|> 
               try pchar <|> pstring
        return tmp
-- simple actions - begin 
actions :: Parser NDAction
actions = choice (map apply pactions) <|>
          try ppushf <|>
          try pcallf <|>
          try pnewf <|> pcondition
	where 
	apply (c,e) = try $ do { string c; skip1; return e}
	pactions = [("pop",NDPop)
			   ,("dswap",NDDSwap)
			   ,("swap",NDSwap)
			   ,("rotr",NDRotR)
			   ,("->",NDRotR)
			   ,("rotl",NDRotL)
			   ,("<-",NDRotL)
			   ,("dup",NDDup)
			   ,("+",NDAdd)
			   ,("-",NDSub)
			   ,("*",NDMul)
			   ,("/",DivD)
			   ,("div",Div)
			   ,("mod",Mod)
			   ,(">=",GE)
			   ,("<=",LE)
			   ,("==",E)
			   ,("<>",NE)
			   ,(">",G)
			   ,("<",L)
			   ,("~",NOT)
			   ,("&&",AND)
			   ,("||",OR)
			   ,("xor",XOR)
			   ,("9.",NDCat)
			   ,("exit",NDExit)
			   ,("@",NDSCallFunction)
			   ]
-- simple actions - end  
-- types - begin     
pbool :: Parser NDAction
pbool = do
        tmp <- string "True" <|> string "False"
        skip1
        return (NDPush (NDTYPEb ( read tmp :: Bool )))
pdigits :: Parser NDAction
pdigits = do
        tmp0 <- pdf
        tmp1 <- pds tmp0
        skip1
        return tmp1
pdf :: Parser String
pdf = try (do{ char '-'
             ; tmp <- many1 digit
             ; return ('-':tmp)
             } ) <|> 
      (do{ tmp <- many1 digit; return tmp } )
pds :: String -> Parser NDAction
pds i = try (do{ char '.'
               ; tmp <- many1 digit
               ; return (NDPush (NDTYPEd (read(i++"."++tmp)::Double)))
               }) <|>
        return (NDPush (NDTYPEi (read i::Integer)))
pchar :: Parser NDAction
pchar = do
        char '\''
        tmp <- try pchar' <|> anyChar
        char '\''
        skip1
        return (NDPush (NDTYPEc tmp))
pchar' :: Parser Char
pchar' = do
         char '\\'
         tmp <- pochar
         return tmp
pstring :: Parser NDAction
pstring = do
          char '"'
          tmp <- many ( try ( many1 $ noneOf "\\\"" ) <|> pstring' )
          char '"'
          skip1
          return (NDPush (NDTYPEs ( concat tmp )))
pstring' :: Parser String
pstring' = do
           char '\\'
           tmp <- pochar
           return [tmp]
pochar :: Parser Char
pochar = choice (map apply escapes)
			where 
				escapes = zip "\\\"\'abfnrtv" "\\\"\'\a\b\f\n\r\t\v"
				apply (c,e) = do {char c; return e}
-- types - end   
-- if statement - begin
parserelse :: Parser [NDActionPos]
parserelse = do
             string "else"
             skip1
             tmp <- manyTill skipper (try $ string "endif")
             return tmp
parserendif :: Parser [NDActionPos]
parserendif = do
              string "endif"
              skip1
              return []
pcondition :: Parser NDAction
pcondition = do
             string "then"
             skip1
             skip
             pthen <- manyTill skipper (lookAhead ( try (string "else") <|> try (string "endif")))
             pelse <- try parserelse <|> parserendif
             return (NDIf pthen pelse)
-- if statement - end
-- functions - begin
pcallf :: Parser NDAction
pcallf = do
         char '@'
         tmp <- many1 $ noneOf skipstring
         return (NDCallFunction (NDTYPEf tmp))
pnewf :: Parser NDAction
pnewf = do
        char '.'
        tmp1 <- many1 $ noneOf skipstring
        skip1
        skip
        tmp2 <- manyTill skipper (char '#')
        return (NDNewFunction (NDTYPEf tmp1) tmp2)
ppushf :: Parser NDAction
ppushf = do
         char '%'
         tmp <- many1 $ noneOf skipstring
         skip1
         return (NDPush (NDTYPEf tmp))
-- functions - end
-- filepaths parser - begin
fparser :: String -> [String]
fparser string = case ( parse fparser' "" string ) of
                     Left err -> []
                     Right xs -> xs
fparser' = many1 (try fquoted <|> fspaced)
fquoted = do
          char '"'
          tmp <- many ( try ( many1 $ noneOf "\\\"" ) <|> fspace )
          char '"'
          skip
          return (concat tmp)
fspaced :: Parser String
fspaced = do
          tmp <- many1 ( try(many1 $ noneOf "\\ ") <|> fspace)
          skip
          return (concat tmp)
fspace :: Parser String
fspace = do
	char '\\'
	tmp <- fochar
	return [tmp]
fochar :: Parser Char
fochar = choice (map apply escapes)
			where 
				escapes = "\\\"\' "
				apply c = do {char c; return c}
-- filepaths parser - end
skip = skipMany ( space <|> newline <|> tab ) 
skip1 = space <|> newline <|> tab <|> (parsecMap (\x -> 'c') eof)
skipstring = " \a\b\f\n\r\t\v"
