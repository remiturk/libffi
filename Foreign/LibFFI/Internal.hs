{-# INCLUDE <ffi.h> #-}
{-# LINE 1 "Foreign/LibFFI/Internal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "Foreign/LibFFI/Internal.hsc" #-}
module Foreign.LibFFI.Internal where


{-# LINE 5 "Foreign/LibFFI/Internal.hsc" #-}

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data CValue
data CType
data CIF

type C_ffi_status   = (Word32)
{-# LINE 17 "Foreign/LibFFI/Internal.hsc" #-}
type C_ffi_abi      = (Word32)
{-# LINE 18 "Foreign/LibFFI/Internal.hsc" #-}

ffi_default_abi :: C_ffi_abi
ffi_default_abi = 2
{-# LINE 21 "Foreign/LibFFI/Internal.hsc" #-}

ffi_ok          :: C_ffi_status
ffi_ok          = 0
{-# LINE 24 "Foreign/LibFFI/Internal.hsc" #-}

sizeOf_cif :: Int
sizeOf_cif = (32)
{-# LINE 27 "Foreign/LibFFI/Internal.hsc" #-}

foreign import ccall unsafe ffi_prep_cif
    :: Ptr CIF -> C_ffi_abi -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO C_ffi_status

foreign import ccall unsafe ffi_call
    :: Ptr CIF -> FunPtr a -> Ptr CValue -> Ptr (Ptr CValue) -> IO ()
