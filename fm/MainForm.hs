module MainForm (
   showMainForm
   ) where

import MainForm.Actions
import MainForm.ListView
import MainForm.MenuBar
import MainForm.State

import Graphics.UI.Gtk
import System.Directory

mainFormTitle :: String
mainFormTitle = "FM"

showMainForm :: FilePath -> IO ()
showMainForm currDir = do
   emptyMainFormState >>= \s -> flip runStateT s $ do
      view <- gets mfsListModel >>= liftIO . treeViewNewWithModel
      modify $ \a -> a { mfsTreeView = view }
      gets mfsCurrentDir >>= liftIO . flip writeIORef currDir
      wnd <- liftIO $ do
         wnd <- windowNew
         set wnd [
            windowDefaultWidth := 800, 
            windowDefaultHeight := 600,
            windowWindowPosition := WinPosCenter, 
            windowTitle := mainFormTitle]
         onDestroy wnd onQuit
         return wnd
      addControls wnd
      liftIO $ do
         widgetShowAll wnd
         widgetGrabFocus view
   return ()

addControls :: Window -> MFState IO ()
addControls wnd = do
   view <- gets mfsTreeView
   model <- gets mfsListModel
   initListView
   liftIO $ do
      mb <- getMenuBar
      sv <- do
         sv <- scrolledWindowNew Nothing Nothing
         scrolledWindowSetPolicy sv PolicyAutomatic PolicyAutomatic
         containerAdd sv view
         return sv
      vbox <- vBoxNew False 0
      boxPackStart vbox mb PackNatural 0
      boxPackStart vbox sv PackGrow 0
      set wnd [containerChild := vbox]
   liftIO $ runMaybeT $ tryChangeDirectory model =<< liftIO getCurrentDirectory
   return ()
