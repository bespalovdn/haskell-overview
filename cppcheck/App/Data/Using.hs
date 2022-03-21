module App.Data.Using (
    Using(..)
    ) where

data Using = Using { usingNamespace::Bool, usingWhat::String }
