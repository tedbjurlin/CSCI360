{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_quilt (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/peanut-brother/school/CSCI360/module13/.stack-work/install/x86_64-linux/569d56480c837cec306a7809e898d4918bdf73ba4bfa6c65dd3652f221722ecd/9.2.7/bin"
libdir     = "/home/peanut-brother/school/CSCI360/module13/.stack-work/install/x86_64-linux/569d56480c837cec306a7809e898d4918bdf73ba4bfa6c65dd3652f221722ecd/9.2.7/lib/x86_64-linux-ghc-9.2.7/quilt-0.1.0.0-3oyMIraszFS44Jj7gokR9t"
dynlibdir  = "/home/peanut-brother/school/CSCI360/module13/.stack-work/install/x86_64-linux/569d56480c837cec306a7809e898d4918bdf73ba4bfa6c65dd3652f221722ecd/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/peanut-brother/school/CSCI360/module13/.stack-work/install/x86_64-linux/569d56480c837cec306a7809e898d4918bdf73ba4bfa6c65dd3652f221722ecd/9.2.7/share/x86_64-linux-ghc-9.2.7/quilt-0.1.0.0"
libexecdir = "/home/peanut-brother/school/CSCI360/module13/.stack-work/install/x86_64-linux/569d56480c837cec306a7809e898d4918bdf73ba4bfa6c65dd3652f221722ecd/9.2.7/libexec/x86_64-linux-ghc-9.2.7/quilt-0.1.0.0"
sysconfdir = "/home/peanut-brother/school/CSCI360/module13/.stack-work/install/x86_64-linux/569d56480c837cec306a7809e898d4918bdf73ba4bfa6c65dd3652f221722ecd/9.2.7/etc"

getBinDir     = catchIO (getEnv "quilt_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "quilt_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "quilt_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "quilt_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quilt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quilt_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
