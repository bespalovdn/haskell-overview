module App.Data.Digit.Parser (
    digit    
    ) where

import App.Data.Parser
import Data.Char (digitToInt)

digit :: Parser Int
digit = dec <|> hex <|> oct <?> "digit: is not a digit"

dec :: Parser Int
dec = do
    a <- anyOf ['1'..'9']
    str <- many $ anyOf ['0'..'9']
    return $ strToInt 10 (a:str)

hex :: Parser Int
hex = do
    string "0x"
    str <- many1 $ anyOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
    return $ strToInt 16 str

oct :: Parser Int
oct = do
    char '0'
    str <- many1 $ anyOf ['0'..'7']
    return $ strToInt 8 str

strToInt :: Int -> String -> Int
strToInt base str = toInt 0 (reverse str)
    where toInt :: Int -> String -> Int
          toInt _ [] = 0
          toInt n (x:xs) = ((digitToInt x) * (base ^ n)) + (toInt (n + 1) xs)
