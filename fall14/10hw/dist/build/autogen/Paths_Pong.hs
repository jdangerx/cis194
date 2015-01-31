module Paths_Pong (
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

bindir     = "/home/john/cis194/10hw/.cabal-sandbox/bin"
libdir     = "/home/john/cis194/10hw/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/Pong-0.1.0.0"
datadir    = "/home/john/cis194/10hw/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/Pong-0.1.0.0"
libexecdir = "/home/john/cis194/10hw/.cabal-sandbox/libexec"
sysconfdir = "/home/john/cis194/10hw/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Pong_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Pong_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Pong_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Pong_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Pong_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
