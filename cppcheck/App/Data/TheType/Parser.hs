module App.Data.TheType.Parser (
    module App.Data.TheType,
    theType
    ) where

import App.Data.Digit.Parser
import App.Data.Spaces.Parser
import App.Data.TheType
import App.Data.TheType.Parser.BuiltinType
import App.Data.Parser
import Data.Maybe (fromJust, isJust)

theType :: Parser TheType
theType = do
    (_const,cot) <- builtinType <|> userType
    -- if type is not defined but it's const type, then userType was not worked out yet, 
    -- so we try to do it right now - maybe it's a constant user type. if not - it's definitely const int.
    cot <- if (not $ isJust cot) && (isJust _const) 
        then (userType >>= return . snd) <|> (return $ Just $ COT_BT $ BuiltinType BT_int Nothing Nothing)
        else return cot
    if isJust cot then return () else fail "theType: ClassOfType is not recognized"
    at <- (spaces >> accessType_ >>= return . Just) <|> return Nothing
    arr <- (spaces >> array >>= return . Just) <|> return Nothing
    return $ TheType _const (fromJust cot) at arr

accessType_ :: Parser AccessType
accessType_ = do
    at <- ptr <|> ref
    return at
    where
    ptr = do
        char '*'
        n <- return . length =<< (many $ spaces >> identifier "const")
        ptr' <- (spaces >> ptr >>= return . Just) <|> return Nothing
        return $ AT_ptr (if n == 0 then Nothing else Just n) ptr'
    ref = do
        char '&'
        n <- return . length =<< (many $ spaces >> identifier "const")
        return $ AT_ref (if n == 0 then Nothing else Just n)

array :: Parser Array
array = do
    char '['
    n <- (spaces >> digit >>= return . Just) <|> (return Nothing)
    spaces >> char ']'
    sub <- (spaces >> array >>= return . Just) <|> (return Nothing)
    return $ Array n sub

userType :: Parser (Maybe Int, Maybe ClassOfType)
userType = fail "theType.userType: is not implemented yet"
