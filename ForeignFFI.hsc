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
foreign import ccall unsafe "&" ffi_type_uint16 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint16 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint32 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint32 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint64 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint64 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_double :: Ptr CType
foreign import ccall unsafe "&" ffi_type_pointer :: Ptr CType

ffi_type_uchar  = ffi_type_uint8
ffi_type_schar  = ffi_type_sint8

ffi_type_uint   = case sizeOf (undefined :: CUInt) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_uint of unsupported size"

ffi_type_sint   = case sizeOf (undefined :: CInt) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_sint of unsupported size"

ffi_type_ulong  = case sizeOf (undefined :: CULong) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_ulong of unsupported size"

ffi_type_slong  = case sizeOf (undefined :: CLong) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_slong of unsupported size"

foreign import ccall unsafe ffi_prep_cif
    :: Ptr CIF -> CInt -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO CInt

foreign import ccall unsafe ffi_call
    :: Ptr CIF -> FunPtr a -> Ptr CValue -> Ptr (Ptr CValue) -> IO ()
