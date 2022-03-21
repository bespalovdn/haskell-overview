{-# LANGUAGE CPP #-}
module Utils.FilePath (
#ifdef WINDOWS
   module System.FilePath.Windows
#else
   module System.FilePath.Posix
#endif
   ) where

#ifdef WINDOWS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
