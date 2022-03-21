module App.Data.Comment (
    Comment(..)
    ) where

data Comment = Comment { getComment::String } deriving (Eq, Read, Show)
