{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_model_ebpf (
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

bindir     = "/home/poranacu/.cabal/bin"
libdir     = "/home/poranacu/.cabal/lib/x86_64-linux-ghc-9.0.2/model-ebpf-0.1.0.0-inplace-model-ebpf"
dynlibdir  = "/home/poranacu/.cabal/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/poranacu/.cabal/share/x86_64-linux-ghc-9.0.2/model-ebpf-0.1.0.0"
libexecdir = "/home/poranacu/.cabal/libexec/x86_64-linux-ghc-9.0.2/model-ebpf-0.1.0.0"
sysconfdir = "/home/poranacu/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "model_ebpf_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "model_ebpf_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "model_ebpf_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "model_ebpf_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "model_ebpf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "model_ebpf_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
