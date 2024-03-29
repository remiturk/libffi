{-# LANGUAGE CPP #-}
-- | Some utility code that papers over differences in how various operating
-- systems perform dynamic loading.
module Common
  ( crtPath
  , dllext
  , DynLib
  , dynLibSym
  , withDynLib
  ) where

import Foreign.Ptr (FunPtr)

#if defined(MIN_VERSION_Win32)
import Control.Exception (bracket)
import Foreign.Ptr (castPtrToFunPtr)
import System.Win32.DLL
import System.Win32.Types (HMODULE)
#else
import System.Posix.DynamicLinker
#endif

-- | The dynamic library containing the C runtime.
crtPath :: FilePath
#if defined(mingw32_HOST_OS)
crtPath = "msvcrt.dll"
#elif defined(darwin_HOST_OS)
crtPath = "libc.dylib"
#else
-- On other OSes, it suffices to use the name of the current
-- executable, as the dyanmic loader can chase dependencies until it
-- finds the C runtime.
crtPath = ""
#endif

-- | The file extension used for dynamic libraries.
dllext :: String
#if defined(mingw32_HOST_OS)
dllext = "dll"
#elif defined(darwin_HOST_OS)
dllext = "dylib"
#else
dllext = "so"
#endif

-- | The Haskell representation of a loaded dynamic library.
#if defined(MIN_VERSION_Win32)
type DynLib = HMODULE
#else
type DynLib = DL
#endif

-- | Return the address of a function symbol contained in a dynamic library.
dynLibSym :: DynLib -> String -> IO (FunPtr a)
#if defined(MIN_VERSION_Win32)
dynLibSym source symbol = do
  addr <- getProcAddress source symbol
  return $ castPtrToFunPtr addr
#else
dynLibSym = dlsym
#endif

-- | Load a dynamic library, perform some action on it, and then free it.
withDynLib :: FilePath -> (DynLib -> IO a) -> IO a
#if defined(MIN_VERSION_Win32)
withDynLib file f = bracket (loadLibrary file) freeLibrary f
#else
withDynLib file = withDL file [RTLD_NOW]
#endif
