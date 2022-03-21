module App.Data.Define (
    Define(..)
    ) where

data Define = Define { 
    defineName::String,
    defineParams::Maybe [String],
    defineBody::Maybe String } deriving (Eq, Read, Show)
