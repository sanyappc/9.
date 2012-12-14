{-
 - Module : NDParse.hs
 - Description : Модуль для парсинга.
 - Stability : experimental
-}
module NDParse(parser) where

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
                     -- Left err -> [NDPush (NDTYPErr $  "error: line: " ++ show (sourceLine (errorPos err)) ++ " col: " ++ show (sourceColumn (errorPos err)) ++ makeErr (errorMessages err))]
                     Left err -> [NDActionPos (NDPush (NDTYPErr $ makeErr (errorMessages err))) (sourceLine (errorPos err)) (sourceColumn (errorPos err)) (-1) (-1)]
                     Right xs -> xs
-- makeErr ((SysUnExpect err):_) = ": unexpected input" ++ makeErr' err
makeErr ((SysUnExpect err):_) = "unexpected input" ++ makeErr' err
-- makeErr ((UnExpect err):_) = ": unexpected item" ++ makeErr' err
makeErr ((UnExpect err):_) = "unexpected item" ++ makeErr' err
-- makeErr ((Message err):_) = ": unknown error" ++ makeErr' err
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
{-
actions :: Parser NDAction
actions =  do
           tmp <- try ppop <|> 
                  try pdswap <|> 
                  try pswap <|> 
                  try protr <|> 
                  try protl <|> 
                  try pdup <|> 
                  try psum <|>
                  try psub <|> 
                  try pmul <|> 
                  try pdiv <|> 
                  try pdivd <|> 
                  try pmod <|> 
                  try pge <|> 
                  try ple <|> 
                  try peq <|> 
                  try pne <|> 
                  try pgt <|> 
                  try plt <|>
                  try pnot <|> 
                  try pand <|> 
                  try por <|> 
                  try pxor <|>
                  try pcat <|>
                  try pexit <|> 
                  try ppushf <|>
                  try pscallf <|>
                  try pcallf <|>
                  try pnewf <|> pcondition
           return tmp
-- Simple actions - begin
ppop :: Parser NDAction
ppop =  do
        string "pop"
        skip1
        return NDPop
pdswap :: Parser NDAction
pdswap = do
         string "dswap"
         skip1
         return NDDSwap
pswap :: Parser NDAction
pswap = do
        string "swap"
        skip1
        return NDSwap 
protr :: Parser NDAction
protr = do
        string "rotr" <|> string "->"
        return NDRotR
protl :: Parser NDAction
protl = do
        string "rotl" <|> string "<-"
        skip1
        return NDRotL
pdup :: Parser NDAction
pdup = do
       string "dup"
       return NDDup
psum :: Parser NDAction
psum = do
       string "+"
       skip1
       return NDAdd
psub :: Parser NDAction
psub = do
       string "-"
       skip1
       return NDSub
pmul :: Parser NDAction
pmul = do
       string "*"
       skip1
       return NDMul
pdivd :: Parser NDAction
pdivd = do
        string "/"
        skip1
        return DivD
pdiv :: Parser NDAction
pdiv = do
       string "div"
       skip1
       return Div
pmod :: Parser NDAction
pmod = do
       string "mod"
       skip1
       return Mod
pge :: Parser NDAction
pge = do
      string ">="
      skip1
      return GE
ple :: Parser NDAction
ple = do
      string "<="
      skip1
      return LE
peq :: Parser NDAction
peq = do
      string "=="
      skip1
      return E
pne :: Parser NDAction
pne = do
      string "<>"
      skip1
      return NE
pgt :: Parser NDAction
pgt = do
      string ">"
      skip1
      return G
plt :: Parser NDAction
plt = do
      string "<"
      skip1
      return L
pnot :: Parser NDAction
pnot = do
       string "~"
       skip1
       return NOT 
pand :: Parser NDAction
pand = do
       string "&&"
       skip1
       return AND
por :: Parser NDAction
por = do
      string "||"
      skip1
      return OR
pxor :: Parser NDAction
pxor = do
       string "xor"
       skip1
       return XOR
pcat :: Parser NDAction
pcat = do
       string "9."
       skip1
       return NDCat
-} 
actions :: Parser NDAction
actions = choice (map apply pactions) 
	where apply (c,e) = do {string c; skip1; return e}
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
           ]
-- Simple actions - end  
-- Types - begin     
pbool :: Parser NDAction
pbool = do
        tmp <- string "True" <|> string "False"
        return (NDPush (NDTYPEb ( read tmp :: Bool )))
pdigits :: Parser NDAction
pdigits = do
        tmp0 <- pdf
        tmp1 <- pds tmp0
		--tmp1 <- (try $ do { char '.'; tmp <- many1 digit; return $ NDPush (NDTYPEd (read(tmp0++"."++tmp)::Double)) }) <|>
		--		(return $ NDPush (NDTYPEi (read tmp0::Integer)))
        --tmp2 <- try $ pdouble (tmp0) <|> pint (tmp0)
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
{-
pdouble :: String -> Parser NDAction
pdouble i = do
            char '.'
            tmp <- many1 $ digit
            return $ NDPush (NDTYPEd ( read ( i ++ "." ++ tmp) :: Double ))
pint :: String -> Parser NDAction
pint i = return (NDPush (NDTYPEi ( read i :: Integer )))
-}
pchar :: Parser NDAction
pchar = do
        char '\''
        tmp <- try pchar' <|> anyChar
        char '\''
        skip1
        return (NDPush (NDTYPEc tmp))
        -- tmp <- between (char '\'') (char '\'') (try pchar' <|> anyChar)
pchar' :: Parser Char
pchar' = do
         char '\\'
         --tmp <- try pstring1 <|> pstring2
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
           --tmp <- try pstring1 <|> pstring2
           tmp <- pochar
           return [tmp]
--pstring1 :: Parser Char
--pstring1 = do
--           tmp <- char '\\' <|> char '\"' <|> char '\''
--           return tmp
pochar :: Parser Char
pochar = choice (map apply escapes)
			where 
				escapes = zip "\\\"\'abfnrtv" "\\\"\'\a\b\f\n\r\t\v"
				apply (c,e) = do {char c; return e}
{-
pstring2 :: Parser Char
pstring2 = do
           tmp <- try pstring2N <|> 
                  try pstring2R <|> 
                  try pstring2T <|> 
                  try pstring2V <|> pstring2F
           return tmp
pstring2N :: Parser Char
pstring2N = do
            char 'n'
            return '\n'
pstring2R :: Parser Char
pstring2R = do
            char 'r'
            return '\r'
pstring2T :: Parser Char
pstring2T = do
            char 't'
            return '\t'
pstring2V :: Parser Char
pstring2V = do
            char 'v'
            return '\v'
pstring2F :: Parser Char
pstring2F = do
            char 'f'
            return '\f'
-}
-- Types - end   
-- IF statement - begin
parserelse :: Parser [NDActionPos]
parserelse = do
             string "else"
             skip1
             tmp <- manyTill skipper (try $ string "endif")
             return tmp
parserendif :: Parser [NDActionPos]
parserendif = do
              string "endif"
              return []
pcondition :: Parser NDAction
pcondition = do
             string "then"
             skip1
             skip
             pthen <- manyTill skipper (lookAhead ( try (string "else") <|> try (string "endif")))
             pelse <- try parserelse <|> parserendif
             return (NDIf pthen pelse)
-- IF statement - end
-- functions - begin
pexit :: Parser NDAction
pexit = do
        string "exit"
        skip1
        return NDExit
pscallf :: Parser NDAction
pscallf = do
         char '@'
         skip1
         return NDSCallFunction
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
skip = skipMany ( space <|> newline <|> tab ) 
skip1 = space <|> newline <|> tab <|> (parsecMap (\x -> 'c') eof)
skipstring = " \a\b\f\n\r\t\v"
