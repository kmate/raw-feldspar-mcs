{-# LANGUAGE QuasiQuotes #-}
module Feldspar.Multicore.Compile.Parallella.Esdk where

import Feldspar.Multicore.Compile.Parallella.Imports
import Feldspar.Multicore.Compile.Parallella.Util (isCTypeOf)

import qualified Language.C.Monad as C (addGlobal, CGen)
import Language.C.Quote.C as C
import qualified Language.C.Syntax as C (Type)


--------------------------------------------------------------------------------
-- Hardware and (e)SDK specific utilities
--------------------------------------------------------------------------------

type CoreCoords = (Word32, Word32)

groupCoord :: CoreId -> CoreCoords
groupCoord coreId
    | isValidCoreId coreId = coreId `divMod` 4
    | otherwise = error $ "invalid core id: " ++ show coreId

isValidCoreId :: CoreId -> Bool
isValidCoreId = flip elem [0..15]

systemCoord :: CoreId -> CoreCoords
systemCoord coreId = let (gr, gc) = groupCoord coreId in (gr + 32, gc + 8)

addCoreSpecification :: CoreId -> C.CGen ()
addCoreSpecification coreId = do
    let (sr, sc) = systemCoord coreId
        setRow   = "asm(\".set __CORE_ROW_," ++ show sr ++ "\");"
        setCol   = "asm(\".set __CORE_COL_," ++ show sc ++ "\");"
    C.addGlobal [cedecl| extern int _CORE_ROW_; |]
    C.addGlobal [cedecl| $esc:("asm(\".global __CORE_ROW_\");") |]
    C.addGlobal [cedecl| $esc:setRow |]
    C.addGlobal [cedecl| extern int _CORE_COL_; |]
    C.addGlobal [cedecl| $esc:("asm(\".global __CORE_COL_\");") |]
    C.addGlobal [cedecl| $esc:setCol |]


type LocalAddress = Word32
type GlobalAddress = Word32

toGlobal :: LocalAddress -> CoreId -> GlobalAddress
toGlobal addr coreId
    -- external memory addresses are relative to a fixed base pointer
    | coreId == sharedId = addr
    | otherwise = let (sr, sc) = systemCoord coreId
                  -- 6 bit row number, 6 bit column number, 20 bit local address
                  in (sr `shift` 26) .|. (sc `shift` 20) .|. addr

aligned :: LocalAddress -> LocalAddress
aligned addr
    | m == 0 = addr
    | otherwise = addr - m + dmaAlign
    where m = addr `mod` dmaAlign

dmaAlign :: Word32
dmaAlign = 16

bank1Base :: LocalAddress  -- local in the address space of a core
bank1Base = 0x2000

sharedBase :: LocalAddress  -- relative to external memory base
sharedBase = 0x1000000


sizeOf :: C.Type -> Length
sizeOf (isCTypeOf (Proxy :: Proxy Bool)             -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Int8)             -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Int16)            -> True) = 2
sizeOf (isCTypeOf (Proxy :: Proxy Int32)            -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Int64)            -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy Word8)            -> True) = 1
sizeOf (isCTypeOf (Proxy :: Proxy Word16)           -> True) = 2
sizeOf (isCTypeOf (Proxy :: Proxy Word32)           -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Word64)           -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy Float)            -> True) = 4
sizeOf (isCTypeOf (Proxy :: Proxy Double)           -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy (Complex Float))  -> True) = 8
sizeOf (isCTypeOf (Proxy :: Proxy (Complex Double)) -> True) = 16
sizeOf cty = error $ "size of C type is unknown: " ++ show cty
