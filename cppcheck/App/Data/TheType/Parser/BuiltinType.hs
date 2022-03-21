module App.Data.TheType.Parser.BuiltinType (
    builtinType
    ) where

import App.Data.TheType
import App.Data.Spaces.Parser
import App.Data.Parser
import Data.Maybe (fromJust, isJust)
import Text.Format (format)

data ResultType = ResultOk
        (Maybe Int) -- ^ const count
        (Maybe (BT_Sign,Int)) -- ^ sign count
        (Maybe BT_Length) -- ^ long/short
        (Maybe BT_Type)
    | ResultFail String

builtinType :: Parser (Maybe Int, Maybe ClassOfType) --(const count,Just BuiltinType | Nothing)
builtinType = do
    (ResultOk a b c d) <- parsePhrase =<< parseIdentifier (ResultOk Nothing Nothing Nothing Nothing)
    case d of
        Just t -> return (a, Just $ COT_BT $ BuiltinType t b c)
        Nothing -> case b of
            Just _ -> return (a, Just $ COT_BT $ BuiltinType BT_int b c)
            Nothing -> case c of
                Just _ -> return (a, Just $ COT_BT $ BuiltinType BT_int b c)
                Nothing -> return (a, Nothing)

parsePhrase :: ResultType -> Parser ResultType
parsePhrase res = do
    res' <- (spaces >> parseIdentifier res >>= return . Just) <|> return Nothing
    case res' of
        Nothing -> return res
        Just (ResultFail e) -> fail $ format "theType.builtinType.parsePhrase: {0}" [e]
        Just a@(ResultOk _ _ _ _) -> parsePhrase a

parseIdentifier :: ResultType -> Parser ResultType
parseIdentifier res = type_ res >>> const_ >>> unsigned_ >>> signed_ >>> long_ >>> short_
    where
    (>>>) :: Parser ResultType -> (ResultType -> Parser ResultType) -> Parser ResultType
    (>>>) p1 p2 = ParserType $ \i0 -> case runParser p1 i0 of
        Left _ -> runParser (p2 res) i0
        Right a -> Right a
        
const_ :: ResultType -> Parser ResultType
const_ (ResultFail a) = return $ ResultFail a
const_ (ResultOk a b c d) = do
    identifier "const"
    x <- return $ if isJust $ a 
        then Just $ 1 + (fromJust a) 
        else Just 1
    return $ ResultOk x b c d

signed_ :: ResultType -> Parser ResultType
signed_ (ResultFail a) = return $ ResultFail a
signed_ (ResultOk a b c d) = do
    identifier "signed"
    return $ if not $ isJust b 
        then ResultOk a (Just (BT_signed,1)) c d
        else case fromJust b of
            (BT_signed,n) -> ResultOk a (Just (BT_signed,n+1)) c d
            (BT_unsigned,_) -> ResultFail "signed/unsigned keywords mutually exclusive"

unsigned_ :: ResultType -> Parser ResultType
unsigned_ (ResultFail a) = return $ ResultFail a
unsigned_ (ResultOk a b c d) = do
    identifier "unsigned"
    return $ if not $ isJust b 
        then ResultOk a (Just (BT_unsigned,1)) c d
        else case fromJust b of
            (BT_unsigned,n) -> ResultOk a (Just (BT_unsigned,n+1)) c d
            (BT_signed,_) -> ResultFail "signed/unsigned keywords mutually exclusive"

long_ :: ResultType -> Parser ResultType
long_ (ResultFail a) = return $ ResultFail a
long_ (ResultOk a b c d) = do
    identifier "long"
    case c of
        Nothing -> return $ ResultOk a b (Just BT_long) d
        Just BT_long -> return $ ResultOk a b (Just BT_long) d
        Just BT_short -> return $ ResultFail $ format "{0} followed by {1} is illegal" [show BT_short, show BT_long]

short_ :: ResultType -> Parser ResultType
short_ (ResultFail a) = return $ ResultFail a
short_ (ResultOk a b c d) = do
    identifier "short"
    case c of
        Nothing -> return $ ResultOk a b (Just BT_short) d
        Just BT_short -> return $ ResultOk a b (Just BT_short) d
        Just BT_long -> return $ ResultFail $ format "{0} followed by {1} is illegal" [show BT_long, show BT_short]

type_ :: ResultType -> Parser ResultType
type_ (ResultFail a) = return $ ResultFail a
type_ (ResultOk a b c d) = do
    bt <- btype
    return $ if not $ isJust d
        then ResultOk a b c (Just bt)
        else ResultFail $ format "{0} followed by {1} is illegal" [show bt, show $ fromJust d]
    
btype :: Parser BT_Type
btype = (identifier "bool" >> return BT_bool)
        <|> (identifier "char" >> return BT_char)
        <|> (identifier "double" >> return BT_double)
        <|> (identifier "float" >> return BT_float)
        <|> (identifier "int" >> return BT_int)
        <|> (identifier "__int8" >> return BT_int8)
        <|> (identifier "__int16" >> return BT_int16)
        <|> (identifier "__int32" >> return BT_int32)
        <|> (identifier "__int64" >> return BT_int64)
