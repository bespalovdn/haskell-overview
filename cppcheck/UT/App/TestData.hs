module App.TestData (
    TestItem(..),
    TestObject(..),
    readObject
    ) where

import App.Data.Comment
import App.Data.Define
import Text.Format

data TestItem = TestItem {
    testObject::String,
    testInput::String,
    testOutput::Maybe TestObject } deriving (Read, Show)

data TestObject = 
    TO_Comment Comment |
    TO_Define Define
    deriving (Eq, Read, Show)

-- Note: can throw an exception in case if "read" function failed.
readObject :: String -> String -> Maybe TestObject
readObject obj str = case obj of
    "Comment" -> TO_Comment <*> (doRead str :: Maybe Comment)
    "Define" -> TO_Define <*> (doRead str :: Maybe Define)
    a -> error $ format "TestData.readObject: unexpected object: '{0}'" [a]
    where
    (<*>) :: (a -> b) -> Maybe a -> Maybe b
    (<*>) _ Nothing = Nothing
    (<*>) f (Just a) = Just $ f a
    doRead :: Read a => String -> Maybe a
    doRead str = let
        res0 = reads str :: (Read a => [(a,String)])
        in if null res0 then Nothing
            else let 
                res1 = head res0
                in if not . null . snd $ res1 then Nothing
                    else Just $ fst res1
