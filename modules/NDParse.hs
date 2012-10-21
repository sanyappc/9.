{-
 - Module : NDParse.hs
 - Description : Модуль для парсинга.
 - Stability : experimental
-}
module NDParse(parser) where

import Text.ParserCombinators.Parsec (parse, Parser, manyTill,
                                       try, eof, string, char,
                                       digit, many, many1, anyChar,
                                       noneOf, skipMany, newline,
                                       tab, space, (<|>), lookAhead)                                     
import Text.Parsec.Error (showErrorMessages, errorMessages)
import Text.Parsec.Prim (parsecMap)

import NDType
import NDAction

parser :: String -> [NDAction]
parser string = case ( parse parser' "" string ) of
                     Left err -> error $ showErrorMessages "or" "unknown error" "expecting:" "unexpected:" "end of input" ( errorMessages err )
                     Right xs -> xs
parser' :: Parser [NDAction]
parser' = do
          skip
          tmp <- manyTill skipper (try eof)
          return tmp
skipper :: Parser NDAction
skipper = do
          tmp <- try actions <|> 
                 try types <|> pcallf
          skip
          return tmp 
types :: Parser NDAction
types = do
        tmp <- try pdigits <|> 
               try pbool <|> 
               try pchar <|> pstring
        return tmp
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
                  try ptop <|> 
                  try pprint <|> 
                  try pexit <|> 
                  try pnewf <|> pcondition
           return tmp
-- Actions - begin
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
        string "rotr"
        return NDRotR
protl :: Parser NDAction
protl = do
        string "rotl"
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
ptop :: Parser NDAction
ptop = do
       char '@'
       skip1
       return TOP
pprint :: Parser NDAction
pprint = do
         char '?'
         skip1
         return PRINT
pexit :: Parser NDAction
pexit = do
        string "exit"
        skip1
        return NDExit
-- Actions - end  
-- Types - begin     
pbool :: Parser NDAction
pbool = do
        tmp <- string "True" <|> string "False"
        return (NDPush (NDTYPEb ( read tmp :: Bool )))
pdigits :: Parser NDAction
pdigits = do
        tmp0 <- char '-' <|> digit
        tmp1 <- many digit
        tmp2 <- try $ pdouble (tmp0 : tmp1) <|> pint (tmp0 : tmp1)
        skip1
        return tmp2
pdouble :: String -> Parser NDAction
pdouble i = do
            char '.'
            tmp <- many1 $ digit
            return $ NDPush (NDTYPEd ( read ( i ++ "." ++ tmp) :: Double ))
pint :: String -> Parser NDAction
pint i = return (NDPush (NDTYPEi ( read i :: Int )))
pchar :: Parser NDAction
pchar = do
        char '\''
        tmp <- anyChar
        char '\''
        skip1
        return (NDPush (NDTYPEc tmp))
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
           tmp <- try pstring1 <|> pstring2
           return tmp
pstring1 :: Parser String
pstring1 = do
           tmp <- char '\\' <|> char '\"'
           return [tmp]
pstring2 :: Parser String
pstring2 = do
           tmp <- try pstring2N <|> try pstring2R <|> pstring2T
           return tmp
pstring2N :: Parser String
pstring2N = do
            char 'n'
            return "\n"
pstring2R :: Parser String
pstring2R = do
            char 'r'
            return "\r"
pstring2T :: Parser String
pstring2T = do
            char 't'
            return "\t"
-- Types - end   
-- IF statement - begin
parserelse :: Parser [NDAction]
parserelse = do
             string "else"
             skip1
             tmp <- manyTill skipper (try $ string "endif")
             return tmp
parserendif :: Parser [NDAction]
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
pnewf :: Parser NDAction
pnewf = do
        char '.'
        tmp1 <- many1 $ noneOf " \n\r\t"
        skip1
        skip
        tmp2 <- manyTill skipper (char '#')
        return (NDNewFunction tmp1 tmp2)
pcallf :: Parser NDAction
pcallf = do
         tmp <- many1 $ noneOf " \n\r\t"
         skip1
         return (NDCallFunction tmp)
-- functions - end
skip = skipMany ( space <|> newline <|> tab ) 
skip1 = space <|> newline <|> tab <|> (parsecMap (\x -> 'c') eof)
