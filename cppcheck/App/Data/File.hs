module App.Data.File (
    File(..),
    FileItem(..),
    Namespace(..)
    ) where

import App.Data.Define
import App.Data.Include
import App.Data.Using

data File = File { getFileData::[FileItem] }

data FileItem = ItemDefine Define
    | ItemInclude Include
    | ItemUsing Using
    | ItemNamespace Namespace

data Namespace = Namespace {
    namespaceName::String,
    namespaceData::[FileItem] }

