module App.Data.File.Parser (
    module App.Data.File,
    file,
    namespace,
    ) where

import App.Data.File
import App.Data.Define.Parser
import App.Data.Include.Parser
import App.Data.Spaces.Parser
import App.Data.Using.Parser
import App.Data.Parser

file :: Parser File
file = do
    items <- fileItems
    return $ File items

fileItems :: Parser [FileItem]
fileItems = do
    spaces
    item <- (fileItem >>= return . Just) 
        <|> (eof >> return Nothing)
        <?> "file.fileItems: unknown file item"
    case item of
        Nothing -> return []
        Just a -> do
            as <- fileItems
            return $ a:as

fileItem :: Parser FileItem
fileItem = do
    item <- (include >>= return . ItemInclude)
        <|> (define >>= return . ItemDefine)
        <|> (using >>= return . ItemUsing)
        <|> (namespace >>= return . ItemNamespace)
    return item

--------------------------------------------------------------------------------
namespace :: Parser Namespace
namespace = do
    identifier "namespace"
    spaces
    name <- flip (<|>) (char '{' >> return "") $ do
        a <- identifier_
        spaces >> char '{'
        return a
    items <- many (spaces >> fileItem)
    spaces
    char '}'
    return $ Namespace name items
