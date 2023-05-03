{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_quilt (
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

bindir     = "/home/peanut-brother/school/CSCI360/module12/.stack-work/install/x86_64-linux/150ad291b0fee8859c67036335e81b871bb1b547d7296c42d460e7d008c50021/8.10.4/bin"
libdir     = "/home/peanut-brother/school/CSCI360/module12/.stack-work/install/x86_64-linux/150ad291b0fee8859c67036335e81b871bb1b547d7296c42d460e7d008c50021/8.10.4/lib/x86_64-linux-ghc-8.10.4/quilt-0.1.0.0-1J7G8safrBsGEnBXkursDJ"
dynlibdir  = "/home/peanut-brother/school/CSCI360/module12/.stack-work/install/x86_64-linux/150ad291b0fee8859c67036335e81b871bb1b547d7296c42d460e7d008c50021/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/peanut-brother/school/CSCI360/module12/.stack-work/install/x86_64-linux/150ad291b0fee8859c67036335e81b871bb1b547d7296c42d460e7d008c50021/8.10.4/share/x86_64-linux-ghc-8.10.4/quilt-0.1.0.0"
libexecdir = "/home/peanut-brother/school/CSCI360/module12/.stack-work/install/x86_64-linux/150ad291b0fee8859c67036335e81b871bb1b547d7296c42d460e7d008c50021/8.10.4/libexec/x86_64-linux-ghc-8.10.4/quilt-0.1.0.0"
sysconfdir = "/home/peanut-brother/school/CSCI360/module12/.stack-work/install/x86_64-linux/150ad291b0fee8859c67036335e81b871bb1b547d7296c42d460e7d008c50021/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quilt_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quilt_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quilt_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quilt_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quilt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quilt_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
