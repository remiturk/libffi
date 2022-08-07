## 0.2 [????.??.??]
* The `libffi` library now uses `bracket` internally and should now be
  exception-safe.
* The definition of `Arg` has changed:

  ```diff
  -newtype Arg = Arg { unArg :: IO (Ptr CType, Ptr CValue, IO ()) }
  +newtype Arg = Arg { unArg :: forall a. (Ptr CType -> Ptr CValue -> IO a) -> IO a }
  ```
* The definition of `RetType` has changed:
  ```diff
  -data RetType a = RetType (Ptr CType) ((Ptr CValue -> IO ()) -> IO a)
  +newtype RetType a = RetType { unRetType :: (Ptr CType -> Ptr CValue -> IO ()) -> IO a }
  ```

## 0.1 [2009.03.17]
* Initial release.
