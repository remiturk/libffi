{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module ForeignFFI where

#include <ffi.h>

import Control.Applicative
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Utils

data CValue
data CType
data CIF

ffi_default_abi :: CInt
ffi_default_abi = #const FFI_DEFAULT_ABI

cif_size :: Int
cif_size = #size ffi_cif

foreign import ccall unsafe "&" ffi_type_void :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint8 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint8 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint32 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint32 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_pointer :: Ptr CType

foreign import ccall unsafe ffi_prep_cif
    :: Ptr CIF -> CInt -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO CInt

foreign import ccall unsafe ffi_call
    :: Ptr CIF -> FunPtr a -> Ptr CValue -> Ptr (Ptr CValue) -> IO ()
