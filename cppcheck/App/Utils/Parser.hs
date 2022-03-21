module App.Utils.Parser (
    Input,
    ParseError,
    ParseResult,
    ParseSuccess,
    ParserType(..),
    -- parser combinators:
    anyChar,
    anyEndsBy,
    anyOf,
    anyTill,
    char,
    endl,
    eof,
    getSettings,
    identifier,
    identifier_,
    many,
    many1,
    sepBy,
    setSettings,
    string,
    try,
    (<|>),
    (<?>),
    -- useful toolkit functions:
    errorDescription,
    errorPlace_LineColumn
    ) where

import Control.Monad
import qualified Data.List as L
import Text.Format

-- Here s - is a parsing settings.
type Input s = (String,s) -- ^ Input for parser.
type ParseError s = (String,Input s) -- ^ The parse error type.
type ParseSuccess s a = (a,Input s) -- ^ The parse success type, where Input - is rest of input after parsing.
type ParseResult s a = Either (ParseError s) (ParseSuccess s a) -- ^ The parse result type.
newtype ParserType s a = ParserType { runParser :: Input s -> ParseResult s a } -- ^ The parser type.

instance Monad (ParserType s) where
    return a = ParserType $ \i -> Right (a,i)
    fail err = ParserType $ \i -> Left (err,i)
    p1 >>= pp = ParserType $ \i0 -> case runParser p1 i0 of
        Left err -> Left err
        Right (a,i1) -> runParser (pp a) i1

instance MonadPlus (ParserType s) where
    mzero = fail "Parser.MonadPlus.mzero"
    mplus p1 p2 = p1 <|> p2

anyChar :: ParserType s Char
anyChar = ParserType $ \i -> case i of
    ([],_) -> Left ("anyChar: input is empty",i)
    ((x:xs),s) -> Right (x,(xs,s))

anyEndsBy :: ParserType s a -> ParserType s String
anyEndsBy p1 = ParserType $ \i -> case runParser p1 i of
    Left _ -> flip runParser i $ do
        ch <- anyChar
        str <- anyEndsBy p1
        return $ ch:str
    Right (_,i1) -> Right ([],i1)

anyOf :: [Char] -> ParserType s Char
anyOf list = do
    ch <- anyChar
    if ch `elem` list then return ch  
        else fail $ format "anyOf: not one of '{0}'" [show list]

anyTill :: ParserType s a -> ParserType s String
anyTill p1 = ParserType $ \i -> case runParser p1 i of
    Left _ -> flip runParser i $ do
        ch <- anyChar
        str <- anyTill p1
        return $ ch:str
    Right _ -> Right ([],i)

char :: Char -> ParserType s Char
char ch = do
    ch' <- anyChar
    if ch == ch' 
        then return ch 
        else fail $ format "char: matching failed for {0}" [show ch]

endl :: ParserType s String
endl = string "\r\n" <|> (char '\n' >>= \ch -> return $ ch:[])

eof :: ParserType s String
eof = ParserType $ \i -> case i of
    ([],_) -> Right ("",i)
    _ -> Left ("eof: not end of file",i)

getSettings :: ParserType s s
getSettings = ParserType $ \i -> Right (snd i,i)

identifier :: String -> ParserType s String
identifier str = do
    i <- string str
    ch <- try $ ((endl <|> eof) >> return Nothing) <|> (anyChar >>= return . Just)
    case ch of
        Nothing -> return i
        Just ch' -> if ch' `notElem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
            then return i
            else fail $ format "identifier: matching failed for '{0}'" [str]

identifier_ :: ParserType s String
identifier_ = do
    a <- anyOf $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z']
    as <- many $ anyOf $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    return (a:as)

many :: ParserType s a -> ParserType s [a]
many p = ParserType $ \i0 -> case runParser p i0 of
    Left _ -> Right ([],i0)
    Right (a,i1) -> case runParser (many p) i1 of
        Right (bs,i2) -> Right (a:bs,i2)
        Left _ -> Left ("many: something gone wrong",i1) -- we shouldn't be here ever!

many1 :: ParserType s a -> ParserType s [a]
many1 p = ParserType $ \i0 -> case runParser p i0 of
    Left err -> Left err
    Right (a,i1) -> case runParser (many p) i1 of
        Right (bs,i2) -> Right (a:bs,i2)
        Left _ -> Left ("many1: something gone wrong",i1) -- we shouldn't be here ever!

sepBy :: ParserType s a -> ParserType s b -> ParserType s [a]
sepBy pA pB = do
    a <- pA
    as <- (pB >> sepBy pA pB) <|> return []
    return (a:as)

setSettings :: s -> ParserType s ()
setSettings s = ParserType $ \i -> Right ((),(fst i,s))

string :: String -> ParserType s String
string str = ParserType $ \(i,s) -> 
    let (x,xs) = splitAt (length str) i
    in if x == str 
        then Right (str,(xs,s))
        else Left (format "string: matching failed for '{0}'" [str],(i,s))

try :: ParserType s a -> ParserType s a
try p1 = ParserType $ \i -> case runParser p1 i of
    Left (err,_) -> Left (err,i)
    Right (a,_) -> Right (a,i)

(<|>) :: ParserType s a -> ParserType s a -> ParserType s a
(<|>) p1 p2 = ParserType $ \i0 -> case runParser p1 i0 of
    Right r1 -> Right r1
    Left _ -> runParser p2 i0

-- | In case if the left parser will fail, this combintor will replace the error
-- by providing the right error message and error place.
(<?>) :: ParserType s a -> String -> ParserType s a
(<?>) p1 errMsg = ParserType $ \i0 -> case runParser p1 i0 of
    Right r1 -> Right r1
    Left (msg1,_) -> Left (format "{0}({1})" [errMsg,msg1], i0)

--------------------------------------------------------------------------------
errorDescription :: ParseError s -> String
errorDescription (str,_) = str

errorPlace_LineColumn :: Input s -> ParseError s -> (Int,Int)
errorPlace_LineColumn (in0,_) (_,(in1,_)) = let
    inPassed = take (length in0 - length in1) in0
    passedLines = L.lines inPassed
    line = length passedLines
    col = if null passedLines then 0 else length $ last passedLines
    in (line,col)
