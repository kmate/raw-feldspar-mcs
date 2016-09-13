module Feldspar.Multicore.Reference where

import Feldspar
import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype LRef a = LRef { unLocalRef :: LArr a }

type DLRef a = LRef (Data a)

allocRef :: PrimType a => CoreId -> Multicore (DLRef a)
allocRef coreId = LRef <$> allocLArr coreId 1


class (MonadComp m, ArrayAccess LArr m) => LocalRefAccess m
  where
    getLocalRef :: PrimType a => DLRef a -> m (Data a)
    setLocalRef :: PrimType a => DLRef a -> Data a -> m ()


instance LocalRefAccess Host
  where
    getLocalRef (LRef arr) = do
        tmp <- newArr 1
        readArr arr (0,0) tmp
        getArr tmp 0

    setLocalRef (LRef arr) value = do
        tmp <- newArr 1
        setArr tmp 0 value
        writeArr arr (0,0) tmp


instance LocalRefAccess CoreComp
  where
    getLocalRef (LRef arr) = getLArr arr 0
    setLocalRef (LRef arr) value = setLArr arr 0 value
