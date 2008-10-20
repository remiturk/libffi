{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module FFI where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Traversable hiding (mapM, sequence)
import Data.List
import Data.Char
import Data.Int
import Data.Word
import System.Environment
import System.IO
import System.Mem
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

data Type a where
    Type    :: Ptr CType -> Int -> (Ptr CValue -> IO a) -> Type a
    TyVoid  :: Type ()

argCInt      = storableFFIArg ffi_type_sint    :: CInt -> IO Arg
argCUInt     = storableFFIArg ffi_type_uint    :: CUInt -> IO Arg
argCLong     = storableFFIArg ffi_type_slong   :: CLong -> IO Arg
argCULong    = storableFFIArg ffi_type_ulong   :: CULong -> IO Arg

argInt       = storableFFIArg ffi_type_hs_int  :: Int -> IO Arg
argInt8      = storableFFIArg ffi_type_sint8   :: Int8 -> IO Arg
argInt16     = storableFFIArg ffi_type_sint16  :: Int16 -> IO Arg
argInt32     = storableFFIArg ffi_type_sint32  :: Int32 -> IO Arg
argInt64     = storableFFIArg ffi_type_sint64  :: Int64 -> IO Arg

argWord      = storableFFIArg ffi_type_hs_word :: Word -> IO Arg
argWord8     = storableFFIArg ffi_type_uint8   :: Word8 -> IO Arg
argWord16    = storableFFIArg ffi_type_uint16  :: Word16 -> IO Arg
argWord32    = storableFFIArg ffi_type_uint32  :: Word32 -> IO Arg
argWord64    = storableFFIArg ffi_type_uint64  :: Word64 -> IO Arg

argCFloat   = storableFFIArg ffi_type_float  :: CFloat -> IO Arg
argCDouble   = storableFFIArg ffi_type_double  :: CDouble -> IO Arg

argPtr       :: Ptr a -> IO Arg
argPtr       = storableFFIArg ffi_type_pointer

argString    :: String -> IO Arg
argString    = customPointerArg newCString free

customPointerArg :: (a -> IO (Ptr b)) -> (Ptr b -> IO ()) -> a -> IO Arg
customPointerArg new free a = do
    pA <- new a
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ptr -> poke ptr pA
    finalizer <- mkFinalizerPtr (const $ free pA)
    addForeignPtrFinalizer finalizer fp
    return (Arg ffi_type_pointer (castForeignPtr fp))

storableFFIArg :: forall a. Storable a => Ptr CType -> a -> IO Arg
storableFFIArg cType a = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ptr -> poke ptr a
    return (Arg cType (castForeignPtr fp))

tyCInt  = storableFFIType ffi_type_sint (undefined :: CInt)
tyCUInt = storableFFIType ffi_type_uint (undefined :: CUInt)
tyCLong = storableFFIType ffi_type_slong (undefined :: CLong)
tyCULong= storableFFIType ffi_type_ulong (undefined :: CULong)

tyInt   = storableFFIType ffi_type_hs_int (undefined :: Int)
tyInt8  = storableFFIType ffi_type_sint8 (undefined :: Int8)
tyInt16 = storableFFIType ffi_type_sint16 (undefined :: Int16)
tyInt32 = storableFFIType ffi_type_sint32 (undefined :: Int32)
tyInt64 = storableFFIType ffi_type_sint64 (undefined :: Int64)

tyWord  = storableFFIType ffi_type_hs_word (undefined :: Word)
tyWord8  = storableFFIType ffi_type_uint8 (undefined :: Word8)
tyWord16 = storableFFIType ffi_type_uint16 (undefined :: Word16)
tyWord32 = storableFFIType ffi_type_uint32 (undefined :: Word32)
tyWord64 = storableFFIType ffi_type_uint64 (undefined :: Word64)

tyCFloat= storableFFIType ffi_type_float (undefined :: CFloat)
tyCDouble= storableFFIType ffi_type_double (undefined :: CDouble)

tyPtr   :: forall a. Type a -> Type (Ptr a)
tyPtr _ = storableFFIType ffi_type_pointer (undefined :: Ptr a)

tyString    :: Type String
tyString    = Type ffi_type_pointer
                (sizeOf (undefined :: Ptr CString))
                (peekCString <=< peek . castPtr)

storableFFIType         :: forall a. Storable a => Ptr CType -> a -> Type a
storableFFIType cType _ = Type cType
                            (sizeOf (undefined :: a))
                            (peek . castPtr :: Ptr CValue -> IO a)

cTypeOfArg :: Arg -> Ptr CType
cTypeOfArg (Arg cType _) = cType

cValueOfArg :: Arg -> ForeignPtr CValue
cValueOfArg (Arg _ cValue) = cValue

callFFI :: DL -> String -> Type a -> [IO Arg] -> IO a
callFFI dl sym mbRetType args = do
    funPtr <- dlsym dl sym
    args' <- sequence args
    callFFI' funPtr mbRetType args'

callFFI' :: forall a t. FunPtr t -> Type a -> [Arg] -> IO a
callFFI' funPtr retType args
    = allocaBytes cif_size $ \cif -> do
        withArray (map cTypeOfArg args) $ \cTypes -> do
            status <- ffi_prep_cif cif ffi_default_abi (genericLength args) cRetType cTypes
            unless (status == ffi_ok) $
                error "callFFI: ffi_prep_cif failed"
            withMany withForeignPtr (map cValueOfArg args) $ \argPtrs -> do
                withArray argPtrs $ \cArgPtrs -> do
                withRet $ \cRet -> ffi_call cif funPtr cRet cArgPtrs
    where
        -- cannot bind a pair here due to Type being a GADT
        cRetType    = fst unType
        withRet     = snd unType
        unType      :: (Ptr CType, (Ptr CValue -> IO ()) -> IO a)
        unType      = case retType of
                            Type cRetType size peek -> (cRetType, \f -> allocaBytes size $ \cRet -> f cRet >> peek cRet)
                            TyVoid  -> (ffi_type_void, \f -> f nullPtr >> return ())

{-
dl <- dlopen "/lib/libc.so.6" [RTLD_NOW]
p <- callFFI dl "calloc" (tyPtr tyWord8) [argInt 1, argInt 41]
callFFI dl "memset" TyVoid [argPtr p, argInt 98, argInt 40]
callFFI dl "memset" TyVoid [argPtr p, argInt 97, argInt 20]
callFFI dl "strfry" tyString [argPtr p]
callFFI dl "free" TyVoid [argPtr p]

dl <- dlopen "" [RTLD_LAZY]
callFFI dl "printf" tyCInt [argString "CLong %d %ld;  CInt %d %ld;  Int %d %ld\n", argCLong 42, argCLong 42, argCInt 42, argCInt 42, argInt 42, argInt 42]
-}
