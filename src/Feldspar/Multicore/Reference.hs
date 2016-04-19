module Feldspar.Multicore.Reference where

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype LocalRef a = LocalRef { unLocalRef :: LocalArr a }


allocRef :: PrimType a => CoreId -> Multicore (LocalRef a)
allocRef coreId = LocalRef <$> allocLArr coreId 1


class (MonadComp m, ArrayAccess LocalArr m) => LocalRefAccess m
  where
    getLocalRef :: PrimType a => LocalRef a -> m (Data a)
    setLocalRef :: PrimType a => LocalRef a -> Data a -> m ()


instance LocalRefAccess Host
  where
    getLocalRef (LocalRef arr) = do
        tmp <- newArr 1
        readArr arr (0,0) tmp
        getArr 0 tmp

    setLocalRef (LocalRef arr) value = do
        tmp <- newArr 1
        setArr 0 value tmp
        writeArr arr (0,0) tmp


instance LocalRefAccess CoreComp
  where
    getLocalRef (LocalRef arr) = getArr 0 -< arr
    setLocalRef (LocalRef arr) value = setArr 0 value -< arr
