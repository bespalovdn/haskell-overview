module App.Data.Define.Parser (
    module App.Data.Define,
    define
    ) where

import App.Data.Define
import App.Data.Spaces.Parser
import App.Data.Parser

define :: Parser Define
define = do
    char '#' >> identifier "define"
    many spaces
    name <- identifier_
    params <- flip (<|>) (return Nothing) $ do
        spaces >> char '('
        ps <- sepBy param (char ',')
        char ')' >> return (Just ps)
    many $ anyOf [' ','\t']
    lines <- getLines
    result <- return $ Define name params (getBody $ concat lines)
{-
    do
        s0 <- getSettings
        defs <- return $ definesList s0
        s1 <- return $ s0 { definesList = defs ++ [result] }
        setSettings s1
-}
    return result
    where
    getBody [] = Nothing
    getBody str = Just str
    getLines = do
        line <- anyEndsBy (eol <|> eof)
        if null line then return []
            else if last line /= '\\' then return [line]
                else getLines >>= \lines -> return (line:lines)

param :: Parser String
param = do
    spaces
    p <- identifier_
    spaces
    return p
