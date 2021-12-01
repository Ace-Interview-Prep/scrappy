{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cereal (
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
version = Version [0,5,8,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lazylambda/.cabal/store/ghc-8.8.4/cereal-0.5.8.1-618e09ec745c370621f9cc2bbb8b0e509fbbd88619c97ab65301ea6fb60aa785/bin"
libdir     = "/home/lazylambda/.cabal/store/ghc-8.8.4/cereal-0.5.8.1-618e09ec745c370621f9cc2bbb8b0e509fbbd88619c97ab65301ea6fb60aa785/lib"
dynlibdir  = "/home/lazylambda/.cabal/store/ghc-8.8.4/cereal-0.5.8.1-618e09ec745c370621f9cc2bbb8b0e509fbbd88619c97ab65301ea6fb60aa785/lib"
datadir    = "/home/lazylambda/.cabal/store/ghc-8.8.4/cereal-0.5.8.1-618e09ec745c370621f9cc2bbb8b0e509fbbd88619c97ab65301ea6fb60aa785/share"
libexecdir = "/home/lazylambda/.cabal/store/ghc-8.8.4/cereal-0.5.8.1-618e09ec745c370621f9cc2bbb8b0e509fbbd88619c97ab65301ea6fb60aa785/libexec"
sysconfdir = "/home/lazylambda/.cabal/store/ghc-8.8.4/cereal-0.5.8.1-618e09ec745c370621f9cc2bbb8b0e509fbbd88619c97ab65301ea6fb60aa785/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cereal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cereal_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cereal_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cereal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cereal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cereal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
