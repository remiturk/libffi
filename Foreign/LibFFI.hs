module Foreign.LibFFI where

import Control.Monad
import Data.List
import Data.Char
import Data.Int
import Data.Word
import System.Posix.DynamicLinker

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Foreign.LibFFI.Internal

ffi_type_hs_int = case sizeOf (undefined :: Int) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_hs_int: unsupported sizeOf (_ :: Int)"

ffi_type_hs_word = case sizeOf (undefined :: Word) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_hs_word: unsupported sizeOf (_ :: Word)"

newtype Arg = Arg { unArg :: IO (Ptr CType, Ptr CValue, IO ()) }

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

argPtr      :: Ptr a -> Arg
argPtr      = mkStorableArg ffi_type_pointer

argString   :: String -> Arg
argString   = customPointerArg newCString free

argConstByteString  :: BS.ByteString -> Arg
argConstByteString  = customPointerArg (flip BSU.unsafeUseAsCString return) (const $ return ())

customPointerArg :: (a -> IO (Ptr b)) -> (Ptr b -> IO ()) -> a -> Arg
customPointerArg newA freeA a = Arg $ do
    p <- newA a
    pp <- new p
    return $ (ffi_type_pointer, castPtr pp, free pp >> freeA p)

mkStorableArg :: Storable a => Ptr CType -> a -> Arg
mkStorableArg cType a = Arg $ do
    p <- malloc
    poke p a
    return $ (cType, castPtr p, free p)

data RetType a = RetType (Ptr CType) ((Ptr CValue -> IO ()) -> IO a)

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

retPtr      :: RetType a -> RetType (Ptr a)
retPtr _    = mkStorableRetType ffi_type_pointer

retString   :: RetType String
retString   = RetType ffi_type_pointer
                (\write -> alloca $ \ptr -> write (castPtr ptr) >> peek ptr >>= peekCString)

retByteString :: RetType BS.ByteString
retByteString = RetType ffi_type_pointer
                (\write -> alloca $ \ptr -> write (castPtr ptr) >> peek ptr >>= BS.packCString)

retMallocByteString :: RetType BS.ByteString
retMallocByteString = RetType ffi_type_pointer
                        (\write -> alloca $ \ptr -> write (castPtr ptr) >> peek ptr >>= BSU.unsafePackMallocCString)

mkStorableRetType :: Storable a => Ptr CType -> RetType a
mkStorableRetType cType
            = RetType cType
                (\write -> alloca $ \ptr -> write (castPtr ptr) >> peek ptr)

callFFI :: FunPtr a -> RetType b -> [Arg] -> IO b
callFFI funPtr (RetType cRetType withRet) args
    = allocaBytes sizeOf_cif $ \cif -> do
        (cTypes, cValues, frees) <- unzip3 `liftM` mapM unArg args
        withArray cTypes $ \cTypesPtr -> do
            status <- ffi_prep_cif cif ffi_default_abi (genericLength args) cRetType cTypesPtr
            unless (status == ffi_ok) $
                error "callFFI: ffi_prep_cif failed"
            withArray cValues $ \cValuesPtr -> do
                ret <- withRet (\cRet -> ffi_call cif funPtr cRet cValuesPtr)
                sequence_ frees
                return ret

{-
dl <- dlopen "" [RTLD_NOW]
p <- callFFI dl "calloc" (retPtr retWord8) [argInt 1, argInt 41]
callFFI dl "memset" retVoid [argPtr p, argInt 98, argInt 40]
callFFI dl "memset" retVoid [argPtr p, argInt 97, argInt 20]
callFFI dl "strfry" retString [argPtr p]
callFFI dl "free" retVoid [argPtr p]

dl <- dlopen "" [RTLD_LAZY]
callFFI dl "printf" retCInt [argString "CLong %d %ld;  CInt %d %ld;  Int %d %ld\n", argCLong (10^10), argCLong (10^10), argCInt (10^10), argCInt (10^10), argInt (10^10), argInt (10^10)]
-}
