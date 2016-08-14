module Paths_JsonXml (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/haskell/JsonXml/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin"
libdir     = "/usr/haskell/JsonXml/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/lib/x86_64-linux-ghc-7.10.3/JsonXml-0.1.0.0-CYX1ZKz0trn5l4pYnt4JFa"
datadir    = "/usr/haskell/JsonXml/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/share/x86_64-linux-ghc-7.10.3/JsonXml-0.1.0.0"
libexecdir = "/usr/haskell/JsonXml/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/libexec"
sysconfdir = "/usr/haskell/JsonXml/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "JsonXml_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "JsonXml_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "JsonXml_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "JsonXml_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "JsonXml_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
