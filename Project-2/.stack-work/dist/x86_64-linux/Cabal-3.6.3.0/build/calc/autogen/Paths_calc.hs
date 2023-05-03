{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_calc (
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
version = Version [1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/peanut-brother/school/CSCI360/Project-2/.stack-work/install/x86_64-linux/5abdf098f159018d3210367bbca981114917596bb4a86ce900177ff1a873e8a0/9.2.6/bin"
libdir     = "/home/peanut-brother/school/CSCI360/Project-2/.stack-work/install/x86_64-linux/5abdf098f159018d3210367bbca981114917596bb4a86ce900177ff1a873e8a0/9.2.6/lib/x86_64-linux-ghc-9.2.6/calc-1.0-6NyXp5rDYy42denbH1LzPG-calc"
dynlibdir  = "/home/peanut-brother/school/CSCI360/Project-2/.stack-work/install/x86_64-linux/5abdf098f159018d3210367bbca981114917596bb4a86ce900177ff1a873e8a0/9.2.6/lib/x86_64-linux-ghc-9.2.6"
datadir    = "/home/peanut-brother/school/CSCI360/Project-2/.stack-work/install/x86_64-linux/5abdf098f159018d3210367bbca981114917596bb4a86ce900177ff1a873e8a0/9.2.6/share/x86_64-linux-ghc-9.2.6/calc-1.0"
libexecdir = "/home/peanut-brother/school/CSCI360/Project-2/.stack-work/install/x86_64-linux/5abdf098f159018d3210367bbca981114917596bb4a86ce900177ff1a873e8a0/9.2.6/libexec/x86_64-linux-ghc-9.2.6/calc-1.0"
sysconfdir = "/home/peanut-brother/school/CSCI360/Project-2/.stack-work/install/x86_64-linux/5abdf098f159018d3210367bbca981114917596bb4a86ce900177ff1a873e8a0/9.2.6/etc"

getBinDir     = catchIO (getEnv "calc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "calc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "calc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "calc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calc_sysconfdir") (\_ -> return sysconfdir)




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
