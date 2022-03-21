module App.Data.Using.Parser (
    module App.Data.Using,
    using
    ) where

import App.Data.Using
import App.Data.Spaces.Parser
import App.Data.Parser
import Data.Maybe (isJust)

using :: Parser Using
using = do
    identifier "using"
    spaces
    namespace <- (identifier "namespace" >>= return . Just)
        <|> return Nothing
    spaces
    what <- anyEndsBy (char ';')
    return $ Using (isJust namespace) what
