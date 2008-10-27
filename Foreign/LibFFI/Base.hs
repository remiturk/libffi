module Foreign.LibFFI.Base where

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

import Foreign.LibFFI.Internal
import Foreign.LibFFI.FFITypes

newtype Arg = Arg { unArg :: IO (Ptr CType, Ptr CValue, IO ()) }

customPointerArg :: (a -> IO (Ptr b)) -> (Ptr b -> IO ()) -> a -> Arg
customPointerArg newA freeA a = Arg $ do
    p <- newA a
    pp <- new p
    return (ffi_type_pointer, castPtr pp, free pp >> freeA p)

mkStorableArg :: Storable a => Ptr CType -> a -> Arg
mkStorableArg cType a = Arg $ do
    p <- malloc
    poke p a
    return (cType, castPtr p, free p)

data RetType a = RetType (Ptr CType) ((Ptr CValue -> IO ()) -> IO a)

instance Functor RetType where
    fmap f (RetType cType withPoke) = RetType cType (fmap f . withPoke)

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
