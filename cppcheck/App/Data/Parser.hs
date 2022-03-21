module App.Data.Parser (
    module Text.Parser,
    Parser,
    ParsingSettings(..),
    emptySettings
    ) where

import App.Data.Define
import Text.Parser

type Parser a = ParserType ParsingSettings a

data ParsingSettings = ParsingSettings {
    definesList::[Define] }

emptySettings :: ParsingSettings
emptySettings = ParsingSettings []
