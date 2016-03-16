module Feldspar.Multicore.Reference where

import GHC.TypeLits

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype LocalRef coreId a = LocalRef { unLocalRef :: LocalArr coreId a }


allocRef :: (KnownNat coreId, SmallType a) => Multicore (LocalRef coreId a)
allocRef = LocalRef <$> allocArr 1


readRef :: (KnownNat coreId, SmallType a) => LocalRef coreId a -> Host (Data a)
readRef (LocalRef arr) = do
    tmp <- newArr 1
    readArr arr (0,0) tmp
    getArr 0 tmp

writeRef :: (KnownNat coreId, SmallType a) => LocalRef coreId a -> Data a -> Host ()
writeRef (LocalRef arr) value = do
    tmp <- newArr 1
    setArr 0 value tmp
    writeArr arr (0,0) tmp


getLocalRef :: (KnownNat ca, KnownNat cb, SmallType a)
            => LocalRef ca a -> CoreComp cb (Data a)
getLocalRef = getArr 0 <=< local . unLocalRef

setLocalRef :: (KnownNat ca, KnownNat cb, SmallType a)
            => LocalRef ca a-> Data a -> CoreComp cb ()
setLocalRef (LocalRef arr) value = setArr 0 value -< arr
