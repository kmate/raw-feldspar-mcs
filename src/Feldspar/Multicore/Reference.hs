module Feldspar.Multicore.Reference where

import Feldspar
import Feldspar.Multicore.Frontend
import Feldspar.Multicore.Representation


newtype SpmRef a = SpmRef { unSpmRef :: LocalArr a }

allocSpmRef :: SmallType a => CoreId -> Multicore (SpmRef a)
allocSpmRef coreId = SpmRef <$> allocArr coreId 1

fetchSpmRef :: SmallType a => SpmRef a -> Data a -> Host ()
fetchSpmRef spmRef value = do
    tmp <- newArr 1
    setArr 0 value tmp
    writeArr (unSpmRef spmRef) (0,0) tmp

flushSpmRef :: SmallType a => SpmRef a -> Host (Data a)
flushSpmRef spmRef = do
    tmp <- newArr 1
    readArr (unSpmRef spmRef) (0,0) tmp
    getArr 0 tmp

getSpmRef :: SmallType a => SpmRef a -> CoreComp (Data a)
getSpmRef = getArr 0 <=< local . unSpmRef

setSpmRef :: SmallType a => Data a -> SpmRef a-> CoreComp ()
setSpmRef value = setArr 0 value <=< local . unSpmRef
