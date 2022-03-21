module MainForm.Actions (
   module Control.Monad.Trans.Maybe,
   onQuit,
   tryChangeDirectory
   ) where

import MainForm.State
import Utils.FilePath

import Control.Exception
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import Data.List (sortBy)
import Graphics.UI.Gtk
import System.Directory
import System.IO.Error

onQuit :: IO ()
onQuit = mainQuit

tryChangeDirectory :: ListModel -> FilePath -> MaybeT IO ()
tryChangeDirectory model path = (\fn -> liftIO (tryJust predicate fn) >>= toMaybeT) $ do
   items <- getDirectoryContents path >>= return . map (\a -> (a, combine path a))
   dirs <- filterM (\(_, a) -> doesDirectoryExist a) items >>= 
      return . filter (\(a,_) -> a `notElem` [".", ".."]) >>= 
      return . sortNoCase . fst . unzip
   dirs <- return $ if isDrive path then dirs else (".." : dirs)
   files <- filterM (\(_, a) -> doesFileExist a) items >>= return . sortNoCase . fst . unzip
   listStoreClear model
   forM_ dirs $ \a -> listStoreAppend model (ListRow a DirItem)
   forM_ files $ \a -> listStoreAppend model (ListRow a FileItem)
   where
   predicate e = if isPermissionError e then Just () else Nothing
   toMaybeT :: Either a b -> MaybeT IO b
   toMaybeT (Left _) = fail ""
   toMaybeT (Right a) = return a

sortNoCase :: [String] -> [String]
sortNoCase = sortBy (\a b -> compare (map toLower a) (map toLower b))
