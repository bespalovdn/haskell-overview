module MainForm.ListView (
   initListView
   ) where

import MainForm.ListView.Events
import MainForm.State

import Data.Char (toLower)
import Graphics.UI.Gtk

initListView :: MFState IO ()
initListView = do
   view <- gets mfsTreeView
   model <- gets mfsListModel
   searchSymbols <- gets mfsSearchSymbols
   liftIO $ do
      treeViewSetHeadersVisible view True
      
      col1 <- treeViewColumnNew
      treeViewColumnSetTitle col1 "name"

      renderer1 <- cellRendererTextNew
      cellLayoutPackStart col1 renderer1 True
      cellLayoutSetAttributeFunc col1 renderer1 model $ 
         setAttributes (markupName searchSymbols) renderer1 model

      treeViewAppendColumn view col1
   setEvents

setAttributes :: (String -> IO String) -> CellRendererText -> ListModel -> TreeIter -> IO ()
setAttributes nameToMarkup renderer model iter = do
   let i = listStoreIterToIndex iter
   (ListRow n t) <- listStoreGetValue model i
   markupText <- nameToMarkup n
   set renderer [
      cellTextMarkup := Just markupText,
      cellTextForeground := if t == DirItem then "green" else "blue"]

markupName :: IORef [Char] -> String -> IO String
markupName searchSyms fileName =  do
   syms <- readIORef searchSyms
   if null syms
      then return fileName
      else case toMarkup syms fileName (False, "") of
         Nothing -> return fileName
         Just str -> return str
   where
   toMarkup :: String -> String -> (Bool, String) -> Maybe String -- searchStr -> fileName -> (boldIsSet, result)
   toMarkup [] rest (bold, result) = if bold then Just $ result ++ "</b>" ++ rest else Just $ result ++ rest
   toMarkup _ [] _ = Nothing
   toMarkup aa@(a:as) (b:bs) (bold, result)
      | toLower a == toLower b = if bold 
            then toMarkup as bs (True, result ++ [b])
            else toMarkup as bs (True, result ++ "<b>" ++ [b])
      | otherwise = if bold
            then toMarkup aa bs (False, result ++ "</b>" ++ [b])
            else toMarkup aa bs (False, result ++ [b])
