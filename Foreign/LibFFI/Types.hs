module Foreign.LibFFI.Types where

import Control.Monad
import Data.List
import Data.Char
import Data.Int
import Data.Word

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Foreign.LibFFI.Base
import Foreign.LibFFI.FFITypes

argCInt     = mkStorableArg ffi_type_sint   :: CInt -> Arg
argCUInt    = mkStorableArg ffi_type_uint   :: CUInt -> Arg
argCLong    = mkStorableArg ffi_type_slong  :: CLong -> Arg
argCULong   = mkStorableArg ffi_type_ulong  :: CULong -> Arg

argInt      = mkStorableArg ffi_type_hs_int :: Int -> Arg
argInt8     = mkStorableArg ffi_type_sint8  :: Int8 -> Arg
argInt16    = mkStorableArg ffi_type_sint16 :: Int16 -> Arg
argInt32    = mkStorableArg ffi_type_sint32 :: Int32 -> Arg
argInt64    = mkStorableArg ffi_type_sint64 :: Int64 -> Arg

argWord     = mkStorableArg ffi_type_hs_word:: Word -> Arg
argWord8    = mkStorableArg ffi_type_uint8  :: Word8 -> Arg
argWord16   = mkStorableArg ffi_type_uint16 :: Word16 -> Arg
argWord32   = mkStorableArg ffi_type_uint32 :: Word32 -> Arg
argWord64   = mkStorableArg ffi_type_uint64 :: Word64 -> Arg

argCFloat   = mkStorableArg ffi_type_float  :: CFloat -> Arg
argCDouble  = mkStorableArg ffi_type_double :: CDouble -> Arg

argCSize    = mkStorableArg ffi_type_size   :: CSize -> Arg

argCChar    = mkStorableArg ffi_type_schar  :: CChar -> Arg
argCUChar   = mkStorableArg ffi_type_uchar  :: CUChar -> Arg

argCWchar   = mkStorableArg ffi_type_schar  :: CWchar -> Arg

argPtr      :: Ptr a -> Arg
argPtr      = mkStorableArg ffi_type_pointer

argString   :: String -> Arg
argString   = customPointerArg newCString free

argConstByteString  :: BS.ByteString -> Arg
argConstByteString  = customPointerArg (flip BSU.unsafeUseAsCString return) (const $ return ())

retVoid     :: RetType ()
retVoid     = RetType ffi_type_void (\write -> write nullPtr >> return ())

retCInt     = mkStorableRetType ffi_type_sint   :: RetType CInt
retCUInt    = mkStorableRetType ffi_type_uint   :: RetType CUInt
retCLong    = mkStorableRetType ffi_type_slong  :: RetType CLong
retCULong   = mkStorableRetType ffi_type_ulong  :: RetType CULong

retInt      = mkStorableRetType ffi_type_hs_int :: RetType Int
retInt8     = mkStorableRetType ffi_type_sint8  :: RetType Int8
retInt16    = mkStorableRetType ffi_type_sint16 :: RetType Int16
retInt32    = mkStorableRetType ffi_type_sint32 :: RetType Int32
retInt64    = mkStorableRetType ffi_type_sint64 :: RetType Int64

retWord     = mkStorableRetType ffi_type_hs_word :: RetType Word
retWord8    = mkStorableRetType ffi_type_uint8  :: RetType Word8
retWord16   = mkStorableRetType ffi_type_uint16 :: RetType Word16
retWord32   = mkStorableRetType ffi_type_uint32 :: RetType Word32
retWord64   = mkStorableRetType ffi_type_uint64 :: RetType Word64

retCFloat   = mkStorableRetType ffi_type_float  :: RetType CFloat
retCDouble  = mkStorableRetType ffi_type_double :: RetType CDouble

retCSize    = mkStorableRetType ffi_type_size   :: RetType CSize

retCChar    = mkStorableRetType ffi_type_schar  :: RetType CChar
retCUChar   = mkStorableRetType ffi_type_uchar  :: RetType CUChar

retCWchar   = mkStorableRetType ffi_type_schar  :: RetType CWchar

retPtr      :: RetType a -> RetType (Ptr a)
retPtr _    = mkStorableRetType ffi_type_pointer

retCString          :: RetType CString
retCString          = retPtr retCChar

retString           :: RetType String
retString           = withRetType peekCString (retPtr retCChar)

retByteString       :: RetType BS.ByteString
retByteString       = withRetType BS.packCString (retPtr retCChar)

retMallocByteString :: RetType BS.ByteString
retMallocByteString = withRetType BSU.unsafePackMallocCString (retPtr retCChar)
