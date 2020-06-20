{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_wellbot (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/darick/wellbot/.stack-work/install/x86_64-linux/b99bff683fcbf123a9e6ab1120ed9bbf109c1e85b780217631e540e900d917e5/8.6.5/bin"
libdir     = "/home/darick/wellbot/.stack-work/install/x86_64-linux/b99bff683fcbf123a9e6ab1120ed9bbf109c1e85b780217631e540e900d917e5/8.6.5/lib/x86_64-linux-ghc-8.6.5/wellbot-0.1.0.0-BkMtpLb75OBL0euBJNNUcO"
dynlibdir  = "/home/darick/wellbot/.stack-work/install/x86_64-linux/b99bff683fcbf123a9e6ab1120ed9bbf109c1e85b780217631e540e900d917e5/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/darick/wellbot/.stack-work/install/x86_64-linux/b99bff683fcbf123a9e6ab1120ed9bbf109c1e85b780217631e540e900d917e5/8.6.5/share/x86_64-linux-ghc-8.6.5/wellbot-0.1.0.0"
libexecdir = "/home/darick/wellbot/.stack-work/install/x86_64-linux/b99bff683fcbf123a9e6ab1120ed9bbf109c1e85b780217631e540e900d917e5/8.6.5/libexec/x86_64-linux-ghc-8.6.5/wellbot-0.1.0.0"
sysconfdir = "/home/darick/wellbot/.stack-work/install/x86_64-linux/b99bff683fcbf123a9e6ab1120ed9bbf109c1e85b780217631e540e900d917e5/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wellbot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wellbot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wellbot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wellbot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wellbot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wellbot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
