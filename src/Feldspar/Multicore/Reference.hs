module Feldspar.Multicore.Reference where

import Feldspar
import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype LRef a = LRef { unLRef :: LArr a }

type DLRef a = LRef (Data a)

allocLRef :: PrimType a => CoreId -> Multicore (DLRef a)
allocLRef coreId = LRef <$> allocLArr coreId 1


class (MonadComp m, ArrayAccess LArr m) => LocalRefAccess m
  where
    getLRef :: PrimType a => DLRef a -> m (Data a)
    setLRef :: PrimType a => DLRef a -> Data a -> m ()


instance LocalRefAccess Host
  where
    getLRef (LRef arr) = do
        tmp <- newArr 1
        readArr arr (0,0) tmp
        getArr tmp 0

    setLRef (LRef arr) value = do
        tmp <- newArr 1
        setArr tmp 0 value
        writeArr arr (0,0) tmp


instance LocalRefAccess CoreComp
  where
    getLRef (LRef arr) = getLArr arr 0
    setLRef (LRef arr) value = setLArr arr 0 value
