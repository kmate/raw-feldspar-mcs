module Feldspar.Multicore.Reference where

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype LocalRef a = LocalRef { unLocalRef :: LocalArr a }

allocRef :: SmallType a => CoreId -> Multicore (LocalRef a)
allocRef coreId = LocalRef <$> allocArr coreId 1

writeRef :: SmallType a => LocalRef a -> Data a -> Host ()
writeRef (LocalRef arr) value = do
    tmp <- newArr 1
    setArr 0 value tmp
    writeArr arr (0,0) tmp

readRef :: SmallType a => LocalRef a -> Host (Data a)
readRef (LocalRef arr) = do
    tmp <- newArr 1
    readArr arr (0,0) tmp
    getArr 0 tmp

getLocalRef :: SmallType a => LocalRef a -> CoreComp (Data a)
getLocalRef = getArr 0 <=< local . unLocalRef

setLocalRef :: SmallType a => Data a -> LocalRef a-> CoreComp ()
setLocalRef value = setArr 0 value <=< local . unLocalRef
