module App.Data.Spaces.Parser (
    module App.Data.Spaces,
    spacesOnly,
    spaces
    ) where

import App.Data.Comment.Parser
import App.Data.Spaces
import App.Data.Parser

spacesOnly :: Parser Spaces
spacesOnly = do
    str <- many1 $ anyOf [' ', '\t', '\n', '\r']
    return $ Spaces str

spaces :: Parser Spaces
spaces = do
    str <- many $ do
        (spacesOnly >>= return . getSpaces) 
        <|> (comment >>= return . getComment)
    return $ Spaces $ concat str