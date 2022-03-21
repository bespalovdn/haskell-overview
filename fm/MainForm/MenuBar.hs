module MainForm.MenuBar (
   getMenuBar
   ) where

import qualified MainForm.Actions as A

import Graphics.UI.Gtk

getMenuBar :: IO MenuBar
getMenuBar = do
   mb <- menuBarNew

   mItemFile <- menuItemNewWithLabel "File"
   menuShellAppend mb mItemFile

   mFile <- menuNew
   menuItemSetSubmenu mItemFile mFile
   mItemQuit <- menuItemNewWithLabel "Quit"
   onActivateLeaf mItemQuit A.onQuit
   menuShellAppend mFile mItemQuit

   return mb
