module Feldspar.Multicore.Reference where

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype LocalRef a = LocalRef { unLocalRef :: LocalArr a }


allocRef :: SmallType a => CoreId -> Multicore (LocalRef a)
allocRef coreId = LocalRef <$> allocLArr coreId 1


class (MonadComp m, ArrayAccess LocalArr m) => LocalRefAccess m
  where
    getLocalRef :: SmallType a => LocalRef a -> m (Data a)
    getLocalRef (LocalRef arr) = do
        tmp <- newArr 1
        readArr arr (0,0) tmp
        getArr 0 tmp

    setLocalRef :: SmallType a => LocalRef a -> Data a -> m ()
    setLocalRef (LocalRef arr) value = do
        tmp <- newArr 1
        setArr 0 value tmp
        writeArr arr (0,0) tmp

instance LocalRefAccess Host
instance LocalRefAccess CoreComp
