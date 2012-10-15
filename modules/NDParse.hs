{-
 - Module : NDParse.hs
 - Description : Модуль для парсинга.
 - Stability : experimental
-}
module NDParse(parser) where
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Prim (parsecMap)
import NDType
import NDAction

parser :: String -> [NDAction]
parser string = case ( parse parser' "" string ) of
                     Left err -> error $ showErrorMessages "or" "unknown error" "expecting:" "unexpected:" "end of input" ( errorMessages err )
                     Right xs -> xs
parser' :: Parser [NDAction]
parser' = do
          tmp <- many (skipper)
          return tmp
skipper :: Parser NDAction
skipper = do
          skip
          tmp <- try (actions) <|> types
          return tmp        
types :: Parser NDAction
types = do
        tmp <- try (pdigits) <|> try (pbool) <|> try (pchar) <|> pstring
        return tmp
actions :: Parser NDAction
actions =  do
           tmp <- try (ppop) <|> try (pdswap) <|> try (pswap) <|> 
                  try (pnext) <|> try (pprev) <|> try (psum) <|>
                  try (psub) <|> try (pmul) <|> try (pdiv) <|> try (pdivd) <|> 
                  try (pmod) <|> try (pge) <|> try (ple) <|> try (peq) <|> 
                  try (pne) <|> try (pgt) <|> try (plt) <|>
                  try (pnot) <|> try (pand) <|> try (por) <|> try (pxor) <|>
                  try (ptop) <|> pprint
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
         space <|> newline <|> tab
         return NDDSwap
pswap :: Parser NDAction
pswap = do
        string "swap"
        skip1
        return NDSwap 
pnext :: Parser NDAction
pnext = do
        string "next"
        return NDNext
pprev :: Parser NDAction
pprev = do
        string "prev"
        skip1
        return NDPrev
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
       string "@"
       skip1
       return TOP
pprint :: Parser NDAction
pprint = do
         string "?"
         skip1
         return PRINT
-- Actions - end  
-- Types - begin     
pbool :: Parser NDAction
pbool = do
        tmp <- string "True" <|> string "False"
        return (NDPush (NDTYPEb ( read tmp :: Bool )))
pdigits :: Parser NDAction
pdigits = do
        tmp0 <- char '-' <|> digit
        tmp1 <- many (digit)
        tmp2 <- try ( pdouble (tmp0 : tmp1) ) <|> pint (tmp0 : tmp1)
        skip1
        return tmp2
pdouble :: String -> Parser NDAction
pdouble i = do
            char '.'
            tmp <- many1 (digit)
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
           tmp <- try (pstring1) <|> pstring2
           return tmp
pstring1 :: Parser String
pstring1 = do
           tmp <- char '\\' <|> char '\"'
           return [tmp]
pstring2 :: Parser String
pstring2 = do
           tmp <- try(pstring2N) <|> try(pstring2R) <|> pstring2T
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
skip = skipMany ( space <|> newline <|> tab ) 
skip1 = space <|> newline <|> tab <|> (parsecMap (\x -> 'c') eof)

-- to make : skip for eof

