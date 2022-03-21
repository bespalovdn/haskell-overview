module MainForm.State (
   module Control.Monad.State.Strict,
   module Data.IORef,
   ItemType(..),
   ListRow(..),
   ListModel,
   MainFormState(..),
   MFState,
   emptyMainFormState
   ) where

import Control.Monad.State.Strict
import Data.IORef
import Graphics.UI.Gtk

data ListRow = ListRow { lrName::String, lrType::ItemType }
type ListModel = ListStore ListRow

data ItemType = FileItem | DirItem deriving (Eq)

data MainFormState = MainFormState {
      mfsTreeView :: TreeView,
      mfsListModel :: ListModel,
      mfsCurrentDir :: IORef FilePath,
      mfsSearchSymbols :: IORef [Char] }

type MFState m a = StateT MainFormState m a

emptyMainFormState :: IO MainFormState
emptyMainFormState = do
   view <- treeViewNew
   model <- listStoreNew []
   curDir <- newIORef "."
   searchSyms <- newIORef ""
   return $ MainFormState view model curDir searchSyms
