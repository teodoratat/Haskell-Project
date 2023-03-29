module Parser where

import qualified Data.List as L
import Result
import Text.Printf (printf)

data ParseError
  = UnexpectedEndOfInput
  | UnexpectedInput {gotInput :: String, expectedInput :: String}
  | Other String
  deriving (Show, Eq)

type ParseResult a = Result ParseError (a, String)

data Parser a = Parser {runParser :: String -> ParseResult a, label :: String}

instance Show (Parser a) where
  show (Parser _ label) = label

parse parser s = fst <$> runParser parser s

success = curry Success

parser input = Parser input ""

expecting :: Parser a -> String -> Parser a
expecting (Parser fn _) msg = Parser fn msg

-- Base parsers --------------------------------------------------------------------------------------------------------

satisfies :: (Char -> Bool) -> String -> Parser Char
satisfies predicate msg = Parser inner msg
  where
    inner "" = Error UnexpectedEndOfInput
    inner (first : rest) =
      if predicate first
        then success first rest
        else Error (UnexpectedInput [first] msg)

-- | Parser that always succeeds with a given value, consuming no input.
succeed :: a -> Parser a
succeed a = parser $ \input -> Success (a, input)

-- | Parser that always fails with a given message.
fail :: String -> Parser a
fail err = parser $ \input -> Error (Other err)

-- Combinators ---------------------------------------------------------------------------------------------------------

-- | Chain two parses, running the second parser with the remaining input from the first parser
--
-- >>> runParser (andThen (string "abc") number) "abc1234"
-- Success (("abc",1234),"")
--
-- >>> runParser (andThen (string "abc") number) "abcd1234"
-- Error (UnexpectedInput {gotInput = "d1234", expectedInput = "At least one digit"})
--
-- >>> runParser (andThen (string "abc") number) "abcd1234"
-- Error (UnexpectedInput {gotInput = "d1234", expectedInput = "At least one digit"})
andThen :: Parser a -> Parser b -> Parser (a, b)
andThen pa pb = parser inner
  where
    inner input =
      case runParser pa input of
        Success (a, rest) ->
          case runParser pb rest of
            Success (b, remaining) -> success (a, b) remaining
            Error err -> Error err
        Error err -> Error err

-- | Try to run the first the parser, if it succeeds, return the result, otherwise run the second parser
--
-- >>> runParser (orElse (string "abc") (string "efg")) "def"
-- Error (UnexpectedInput {gotInput = "def", expectedInput = "string \"abc\" or string \"efg\""})
orElse :: Parser a -> Parser a -> Parser a
orElse pa pb = Parser inner msg
  where
    msg = printf "%s or %s" (label pa) (label pb)
    inner input =
      case runParser pa input of
        Success (a, rest) -> success a rest
        Error _ -> case runParser pb input of
          Error _ -> Error (UnexpectedInput input msg)
          Success (ok, rest') -> success ok rest'

-- | Chain two parsers, feeding both the result and the remaining input from the first parser to the second parser.
pWith :: Parser a -> (a -> Parser b) -> Parser b
pWith pa f = parser inner
  where
    inner input =
      case runParser pa input of
        Success (a, rest) ->
          case runParser (f a) rest of
            Success (b, remaining) -> success b remaining
            Error err -> Error err
        Error err -> Error err

-- Extra combinators ---------------------------------------------------------------------------------------------------

-- | Tries a list of parsers until the first one succeeds.
-- Can be used as a convenience method for chaining multiple parsers instead of `orElse`
--
-- >>> runParser (oneOf (char 'a') [char 'b', char 'c']) "ax"
-- Success ('a',"x")
--
-- >>> runParser (oneOf (string "point") [string "vec", string "color"]) "vec"
-- Success ("vec","")
--
-- >>> runParser (oneOf (string "point") [string "vec", string "color"]) "thing"
-- Error (UnexpectedInput {gotInput = "thing", expectedInput = "string \"point\" or string \"vec\" or string \"color\""})
oneOf :: Parser a -> [Parser a] -> Parser a
oneOf p [] = p
oneOf p (next : ps) =
  p `orElse` (oneOf next ps)

-- | Run the parser while it succeeds 0 or more times, collecting the results in a list.
--
--  Never fails!
many :: Parser a -> Parser [a]
many p = parser inner
  where
    inner "" = success [] ""
    inner input =
      case runParser p input of
        Success (r, rest) ->
          -- `many` always succeeds
          case runParser (many p) rest of
            Success (rs, remaining) -> success (r : rs) remaining
        Error _ -> success [] input

-- | Run the parser while it succeeds 1 or more times, collecting the results in a list.
--
--  Fails if the parser doesn't succeed at least once.
--
-- >>> runParser (some (char 'a')) "aa"
-- Success ("aa","")
--
-- >>> runParser (some (char 'a')) ""
-- Error UnexpectedEndOfInput
--
-- >>> runParser (some (char 'a')) "bb"
-- Error (UnexpectedInput {gotInput = "bb", expectedInput = "At least one character 'a'"})
some :: Parser a -> Parser [a]
some p = Parser inner msg
  where
    msg = printf "At least one %s" (label p)
    inner "" = Error UnexpectedEndOfInput
    inner input =
      case runParser p input of
        Success (r, rest) ->
          -- `many` always succeeds
          case runParser (many p) rest of
            Success (rs, remaining) -> success (r : rs) remaining
        Error err -> Error (UnexpectedInput input msg)

-- | Run a parser, if it succeeds, it returns the result wrapped in Just, otherwise returns Nothing without consuming input.
--
--  Always succeeds.
--
-- >>> runParser (opt (char 'a')) "ab"
-- Success (Just 'a',"b")
--
-- >>> runParser (opt (char 'a')) "bb"
-- Success (Nothing,"bb")
opt :: Parser a -> Parser (Maybe a)
opt p = Parser inner (label p)
  where
    inner input =
      case runParser p input of
        Success (r, rest) -> success (Just r) rest
        Error err -> success Nothing input

-- | Transform the result of a parser.
pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = Parser inner (label p)
  where
    inner "" = Error UnexpectedEndOfInput
    inner input =
      case runParser p input of
        Success (r, rest) -> success (f r) rest
        Error err -> Error err

-- Convenience functions -----------------------------------------------------------------------------------------------

lower :: Parser Char
lower = satisfies (\c -> elem c ['a' .. 'z']) "lowercase character"

upper :: Parser Char
upper = satisfies (\c -> elem c ['A' .. 'Z']) "uppercase character"

char :: Char -> Parser Char
char c = satisfies (== c) ("character " ++ show c)

digit :: Parser Char
digit = satisfies (`elem` ['0' .. '9']) "digit"

letter :: Parser Char
letter = (lower `orElse` upper) `expecting` "letter"

-- | Parser for parsing a whitespace character
wsChar :: Parser Char
wsChar = satisfies (`elem` [' ', '\n', '\t']) "whitespace"

wsChars :: Parser Char
wsChars = satisfies (`elem` ['-']) "line"
-- | Parse a number
--
-- >>> runParser number "123"
-- Success (123,"")
--
-- >>> runParser number "asd"
-- Error (UnexpectedInput {gotInput = "asd", expectedInput = "At least one digit"})
number :: Parser Int
number = pMap read $ some digit `expecting` "number"

-- | Parse a string.
--
-- >>> runParser (string "hello") "hello"
-- Success ("hello","")
--
-- >>> runParser (string "abc") "hello"
-- Error (UnexpectedInput {gotInput = "h", expectedInput = "character 'a'"})
string :: String -> Parser String
string "" = parser (\input -> success "" input)
string (c : cs) = (pMap (\(x, xs) -> x : xs) (andThen (char c) (string cs))) `expecting` (printf "string \"%s\"" (c : cs))

-- | Parser for consuming zero or more whitespace characters.
--
-- Always succeeds.
--
-- >>> runParser ws "  \nHello"
-- Success ("  \n","Hello")
--
-- >>> runParser ws "Hello"
-- Success ("","Hello")
ws :: Parser String
ws = many wsChar

-- Extra utilities -----------------------------------------------------------------------------------------------------

-- | Chain two parses, discarding the value of the first parser
--
-- >>> runParser (pThen (char ' ') (char 'b')) " b"
-- Success ('b',"")
--
-- >>> runParser (pThen (many $ char ' ') number) "   123"
-- Success (123,"")
pThen :: Parser a -> Parser b -> Parser b
pThen pa pb = pMap snd $ andThen pa pb

-- >>> runParser (andThen3 (char 'a') (char 'b') (char 'c')) "abc"
-- Success (('a','b','c'),"")
andThen3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
andThen3 pa pb pc = pMap (\((a, b), c) -> (a, b, c)) $ pa `andThen` pb `andThen` pc

-- >>> runParser (andThen4 (char 'a') (char 'b') (char 'c') (char 'd')) "abcd"
-- Success (('a','b','c','d'),"")
andThen4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
andThen4 pa pb pc pd = pMap (\(((a, b), c), d) -> (a, b, c, d)) $ pa `andThen` pb `andThen` pc `andThen` pd

-- >>> runParser (andThen5 (char 'a') (char 'b') (char 'c') (char 'd') (char 'e')) "abcde"
-- Success (('a','b','c','d','e'),"")
andThen5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
andThen5 pa pb pc pd pe = pMap (\((((a, b), c), d), e) -> (a, b, c, d, e)) $ pa `andThen` pb `andThen` pc `andThen` pd `andThen` pe

-- >>> runParser (andThen6 (char 'a') (char 'b') (char 'c') (char 'd') (char 'e') (char 'f')) "abcdef"
-- Success (('a','b','c','d','e','f'),"")
andThen6 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser (a, b, c, d, e, f)
andThen6 pa pb pc pd pe pf = pMap (\(((((a, b), c), d), e), f) -> (a, b, c, d, e, f)) $ pa `andThen` pb `andThen` pc `andThen` pd `andThen` pe `andThen` pf

-- >>> data Point = Point Int Int deriving (Show)
-- >>> runParser (pMap2 Point number (ws `pThen` number)) "1 2"
-- Success (Point 1 2,"")
--
pMap2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pMap2 f pa pb = pMap (uncurry f) $ andThen pa pb

-- >>> data Point = Point Int Int Int deriving (Show)
-- >>> runParser (pMap3 Point number (ws `pThen` number) (ws `pThen` number)) "1 2 3"
-- Success (Point 1 2 3,"")
pMap3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pMap3 f pa pb pc = pMap (\(a, b, c) -> f a b c) (andThen3 pa pb pc)

pMap4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pMap4 f pa pb pc pd = pMap (\(a, b, c, d) -> f a b c d) (andThen4 pa pb pc pd)

pMap5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
pMap5 f pa pb pc pd pe = pMap (\(a, b, c, d, e) -> f a b c d e) (andThen5 pa pb pc pd pe)

pMap6 :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
pMap6 f pa pb pc pd pe pf = pMap (\(a, b, c, d, e, f') -> f a b c d e f') (andThen6 pa pb pc pd pe pf)

-- | Run a parser between two other parsers, discarding the result of the enclosing parsers
--
-- >>> runParser (between (char '"') (char '"') (many letter)) "\"Hello\""
-- Success ("Hello","")
-- >>> runParser (between (char '[') (char ']') number) "[1]"
-- Success (1,"")
--
-- >>> runParser (between (char '[') (char ']') number) "[1|"
-- Error (UnexpectedInput {gotInput = "|", expectedInput = "character ']'"})
between :: Parser a -> Parser b -> Parser c -> Parser c
between pHd pTl p = pMap fst $ pHd `pThen` (p `andThen` pTl)

-- >>> runParser (sepBySome (char ',') number) "1"
-- Success ([1],"")
--
-- >>> runParser (sepBySome (char ',') number) "1,2"
-- Success ([1,2],"")
--
-- >>> runParser (sepBySome (char ',') number) ""
-- Error UnexpectedEndOfInput
--
sepBySome :: Parser a -> Parser b -> Parser [b]
sepBySome sep p = pMap (\(x, xs) -> x : xs) $ p `andThen` (many (sep `pThen` p))

-- | Apply a parser n times
--
-- >>> runParser (pRepeat 2 (char 'a')) "aaa"
-- Success ("aa","a")
--
-- >>> runParser (pRepeat 3 (char 'a' `orElse` char 'b')) "abba"
-- Success ("abb","a")
pRepeat :: Int -> Parser a -> Parser [a]
pRepeat 0 _ = succeed []
pRepeat n p = pMap (\(a, as) -> a : as) $ p `andThen` (pRepeat (n -1) p)


exampleArray :: [String]
exampleArray = ["a", "b", "c", "d", "e"]

(firstHalf, secondHalf) = splitAt (length exampleArray `div` 2) exampleArray
