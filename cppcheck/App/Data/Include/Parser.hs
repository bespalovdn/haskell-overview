module App.Data.Include.Parser (
    module App.Data.Include,
    include
    ) where

import App.Data.Include
import App.Data.Spaces.Parser
import App.Data.Parser

include :: Parser Include
include = do
    string "#include"
    spaces
    str <- (char '\"' >> anyEndsBy (char '\"'))
        <|> (char '<' >> anyEndsBy (char '>'))
    return $ Include str
