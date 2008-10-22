module FFI where

import Control.Monad
import Data.List
import Data.Char
import Data.Int
import Data.Word
import System.Posix.DynamicLinker

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal

import ForeignFFI

ffi_type_hs_int = case sizeOf (undefined :: Int) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_hs_int: unsupported sizeOf (_ :: Int)"

ffi_type_hs_word = case sizeOf (undefined :: Word) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_hs_word: unsupported sizeOf (_ :: Word)"

data Arg    = Arg (Ptr CType) (ForeignPtr CValue)

argCInt     = mkStorableArg ffi_type_sint   :: CInt -> IO Arg
argCUInt    = mkStorableArg ffi_type_uint   :: CUInt -> IO Arg
argCLong    = mkStorableArg ffi_type_slong  :: CLong -> IO Arg
argCULong   = mkStorableArg ffi_type_ulong  :: CULong -> IO Arg

argInt      = mkStorableArg ffi_type_hs_int :: Int -> IO Arg
argInt8     = mkStorableArg ffi_type_sint8  :: Int8 -> IO Arg
argInt16    = mkStorableArg ffi_type_sint16 :: Int16 -> IO Arg
argInt32    = mkStorableArg ffi_type_sint32 :: Int32 -> IO Arg
argInt64    = mkStorableArg ffi_type_sint64 :: Int64 -> IO Arg

argWord     = mkStorableArg ffi_type_hs_word:: Word -> IO Arg
argWord8    = mkStorableArg ffi_type_uint8  :: Word8 -> IO Arg
argWord16   = mkStorableArg ffi_type_uint16 :: Word16 -> IO Arg
argWord32   = mkStorableArg ffi_type_uint32 :: Word32 -> IO Arg
argWord64   = mkStorableArg ffi_type_uint64 :: Word64 -> IO Arg

argCFloat   = mkStorableArg ffi_type_float  :: CFloat -> IO Arg
argCDouble  = mkStorableArg ffi_type_double :: CDouble -> IO Arg

argPtr      :: Ptr a -> IO Arg
argPtr      = mkStorableArg ffi_type_pointer

argString   :: String -> IO Arg
argString   = customPointerArg newCString free

customPointerArg :: (a -> IO (Ptr b)) -> (Ptr b -> IO ()) -> a -> IO Arg
customPointerArg new free a = do
    pA <- new a
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ptr -> poke ptr pA
    finalizer <- mkFinalizerPtr (const $ free pA)
    addForeignPtrFinalizer finalizer fp
    return (Arg ffi_type_pointer (castForeignPtr fp))

mkStorableArg :: Storable a => Ptr CType -> a -> IO Arg
mkStorableArg cType a = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ptr -> poke ptr a
    return (Arg cType (castForeignPtr fp))

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

mkStorableRetType :: Storable a => Ptr CType -> RetType a
mkStorableRetType cType
            = RetType cType
                (\write -> alloca $ \ptr -> write (castPtr ptr) >> peek ptr)

cTypeOfArg :: Arg -> Ptr CType
cTypeOfArg (Arg cType _) = cType

cValueOfArg :: Arg -> ForeignPtr CValue
cValueOfArg (Arg _ cValue) = cValue

callFFI :: DL -> String -> RetType a -> [IO Arg] -> IO a
callFFI dl sym retType args = do
    funPtr <- dlsym dl sym
    args' <- sequence args
    callFFI' funPtr retType args'

callFFI' funPtr (RetType cRetType withRet) args
    = allocaBytes cif_size $ \cif -> do
        withArray (map cTypeOfArg args) $ \cTypes -> do
            status <- ffi_prep_cif cif ffi_default_abi (genericLength args) cRetType cTypes
            unless (status == ffi_ok) $
                error "callFFI: ffi_prep_cif failed"
            withMany withForeignPtr (map cValueOfArg args) $ \argPtrs -> do
                withArray argPtrs $ \cArgPtrs -> do
                withRet $ \cRet -> ffi_call cif funPtr cRet cArgPtrs

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
