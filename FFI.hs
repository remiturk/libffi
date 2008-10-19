{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Traversable hiding (mapM)
import Data.List
import Data.Char
import System.Environment
import System.IO
import System.Posix.DynamicLinker

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal

import ForeignFFI

data Call = Call String (Maybe Type) [Value]
    deriving (Eq, Show)

data Type = TInt | TCInt | TString | TPointer
    deriving (Eq, Show, Read)

data Value = VInt Int | VCInt CInt | VString String | VPointer (Ptr ())
    deriving (Eq, Show, Read)

instance Read (Ptr a) where
    readsPrec p = map (first $ plusPtr nullPtr) . readsPrec p

-- FIXME: 32bitness!

ffi_type_hs_int = case sizeOf (undefined :: Int) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_hs_int: unsupported sizeOf (_ :: Int)"

valueToFFI :: Value -> IO (Ptr CType, Ptr CValue, IO ())
valueToFFI (VInt n) = do
    ptr <- new n
    return (ffi_type_hs_int, castPtr ptr, free ptr)
valueToFFI (VCInt n) = do
    ptr <- new n
    return (ffi_type_sint, castPtr ptr, free ptr)
valueToFFI (VString s) = do
    ptr <- new =<< newCString s
    return (ffi_type_pointer, castPtr ptr, peek (castPtr ptr) >>= free >> free ptr)
valueToFFI (VPointer p) = do
    ptr <- new p
    return (ffi_type_pointer, castPtr ptr, free ptr)

typeToFFI           :: Type -> (Ptr CType, (Ptr a -> IO b) -> IO b)
typeToFFI TInt      = (ffi_type_hs_int, allocaBytes $ sizeOf (undefined :: Int))
typeToFFI TCInt     = (ffi_type_sint, allocaBytes $ sizeOf (undefined :: CInt))
typeToFFI TString   = (ffi_type_pointer, allocaBytes $ sizeOf (undefined :: CString))
typeToFFI TPointer  = (ffi_type_pointer, allocaBytes $ sizeOf (undefined :: Ptr ()))

valueFromFFI :: Ptr CValue -> Type -> IO Value
valueFromFFI ptr TInt
    = VInt <$> peek (castPtr ptr)
valueFromFFI ptr TCInt
    = VCInt <$> peek (castPtr ptr)
valueFromFFI ptr TString
    = VString <$> (peekCString =<< peek (castPtr ptr))
valueFromFFI ptr TPointer
    = VPointer <$> peek (castPtr ptr)

call :: DL -> Call -> IO (Maybe Value)
call dl (Call sym retType args) = do
    funPtr <- dlsym dl sym
    callFunPtr funPtr retType args

callFunPtr :: FunPtr a -> Maybe Type -> [Value] -> IO (Maybe Value)
callFunPtr funPtr mbRetType args = allocaBytes cif_size $ \cif -> do
    (types, values, frees) <- unzip3 <$> mapM valueToFFI args
    withArray types $ \ctypes -> do
        ffi_prep_cif cif ffi_default_abi (genericLength args) cRetType ctypes
        withArray values $ \cvalues -> do
            allocaRet $ \cRet -> do
                ffi_call cif funPtr cRet cvalues
                sequence_ frees
                traverse (valueFromFFI cRet) mbRetType
    where
        (cRetType, allocaRet) = maybe (ffi_type_void, ($ nullPtr)) typeToFFI mbRetType

main = do
    lib:sym:ret:args <- getArgs
    dl <- dlopen lib [RTLD_NOW]
    call dl (Call sym (read ret) [ if isDigit (head arg) then VInt (read arg) else VString arg | arg <- args ])
        >>= print

{-
dl <- dlopen "/lib/libc.so.6" [RTLD_NOW]
Just p <- call dl $ Call "calloc" (Just TPointer) [VInt 1, VInt 41]
call dl $ Call "memset" Nothing [p, VInt 98, VInt 40]
call dl $ Call "memset" Nothing [p, VInt 97, VInt 20]
call dl $ Call "strfry" (Just TString) [p]
call dl $ Call "free" Nothing [p]
-}
