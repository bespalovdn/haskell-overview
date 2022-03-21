module App.Data.Comment.Parser (
    module App.Data.Comment,
    comment
    ) where

import App.Data.Comment
import App.Data.Parser

comment :: Parser Comment
comment = do
    str <- comment1 <|> comment2
    return $ Comment str
    where
    comment1 = string "//" >> anyEndsBy (eol <|> eof)
    comment2 = string "/*" >> anyEndsBy (string "*/")
