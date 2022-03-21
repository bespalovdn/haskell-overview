module MainForm.ListView.Events (
   setEvents
   ) where

import MainForm.Actions
import MainForm.ListView.Actions
import MainForm.State
import Utils.FilePath

import qualified Control.Exception as E
import Data.Char (toLower)
import Graphics.UI.Gtk

setEvents :: MFState IO ()
setEvents = do
   setKeyReturn
   setKeyBackspace
   setSearchSymbols

setKeyReturn :: MFState IO ()
setKeyReturn = do
   view <- gets mfsTreeView
   model <- gets mfsListModel
   currDirRef <- gets mfsCurrentDir
   searchSyms <- gets mfsSearchSymbols
   flip (>>) (return ()) $ liftIO $ do
      view `on` keyPressEvent $ tryEvent $ do
         "Return" <- eventKeyName
         liftIO $ do
            modifyIORef searchSyms $ const ""
            widgetQueueDraw view
            s <- treeViewGetSelection view
            Just iter <- treeSelectionGetSelected s
            (ListRow a t) <- listStoreGetValue model (listStoreIterToIndex iter)
            case t of
               FileItem -> return ()
               DirItem -> (\fn -> runMaybeT fn >> return ()) $ changeDir model view currDirRef a
   where
   changeDir model view currDirRef dir = do
      currDir <- liftIO $ readIORef currDirRef
      let newDir = if dir == ".." then takeDirectory currDir else combine currDir dir
      tryChangeDirectory model newDir
      when (dir == "..") $ selectItem model view $ takeFileName currDir
      liftIO $ do
         writeIORef currDirRef newDir
         widgetGrabFocus view

setKeyBackspace :: MFState IO ()
setKeyBackspace = do
   view <- gets mfsTreeView
   model <- gets mfsListModel
   currDirRef <- gets mfsCurrentDir
   searchSyms <- gets mfsSearchSymbols
   flip (>>) (return ()) $ liftIO $ do
      view `on` keyPressEvent $ tryEvent $ do
         "BackSpace" <- eventKeyName
         liftIO $ do
            syms <- readIORef searchSyms
            when (not $ null syms) $ do
               modifyIORef searchSyms $ const ""
               widgetQueueDraw view
               eventBreak
            currDir <- readIORef currDirRef
            if isDrive currDir then return () else (\fn -> runMaybeT fn >> return ()) $ do
               let newDir = takeDirectory currDir
               tryChangeDirectory model newDir
               selectItem model view $ takeFileName currDir
               liftIO $ do
                  writeIORef currDirRef newDir
                  widgetGrabFocus view

setSearchSymbols :: MFState IO ()
setSearchSymbols = do
   view <- gets mfsTreeView
   searchSyms <- gets mfsSearchSymbols
   flip (>>) (return ()) $ liftIO $ do
      view `on` keyPressEvent $ tryEvent $ do
         keyStr <- eventKeyName
         when (null keyStr || length keyStr > 1) eventBreak
         let key = toLower $ head keyStr
         when (key `notElem` symbols) eventBreak
         liftIO $ do
            modifyIORef searchSyms $ \a -> a ++ [key]
            widgetQueueDraw view
   where
   symbols = "abcdefghijklmnopqrstuvwxyz" ++
             "0123456789" ++
             " ._"

eventBreak :: a
eventBreak = E.throw $ E.PatternMatchFail ""
