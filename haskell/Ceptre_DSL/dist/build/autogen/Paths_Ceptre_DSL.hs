{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Ceptre_DSL (
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

bindir     = "/Users/se/.cabal/bin"
libdir     = "/Users/se/.cabal/lib/x86_64-osx-ghc-8.2.2/Ceptre-DSL-0.1.0.0-BgViz9k50NzIKY87Fs1g8q"
dynlibdir  = "/Users/se/.cabal/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/se/.cabal/share/x86_64-osx-ghc-8.2.2/Ceptre-DSL-0.1.0.0"
libexecdir = "/Users/se/.cabal/libexec/x86_64-osx-ghc-8.2.2/Ceptre-DSL-0.1.0.0"
sysconfdir = "/Users/se/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ceptre_DSL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ceptre_DSL_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Ceptre_DSL_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Ceptre_DSL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ceptre_DSL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ceptre_DSL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
