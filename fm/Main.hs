import MainForm

import Graphics.UI.Gtk
import System.Directory

main :: IO ()
main = do
   initGUI
   --timeoutAddFull (yield >> return True) priorityDefaultIdle 10
   showMainForm =<< getCurrentDirectory
   mainGUI
