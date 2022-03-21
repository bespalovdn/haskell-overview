module MainForm.ListView.Actions (
   module Control.Monad.Trans.Maybe,
   selectItem
   ) where

import MainForm.State

import Control.Monad.Trans.Maybe
import Graphics.UI.Gtk

selectItem :: ListModel -> TreeView -> String -> MaybeT IO ()
selectItem model view item = liftIO $ do
   size <- listStoreGetSize model
   forM_ [0..size-1] $ \i -> do
      (ListRow name _) <- listStoreGetValue model i
      when (name == item) $ do
         selection <- treeViewGetSelection view
         mIter <- treeModelGetIter model [i]
         case mIter of
            Nothing -> return ()
            (Just iter) -> do
               treeSelectionSelectIter selection iter
               Just column <- treeViewGetColumn view 0
               treeViewScrollToCell view [i] column Nothing

