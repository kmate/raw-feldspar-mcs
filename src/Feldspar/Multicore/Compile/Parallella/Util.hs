module Feldspar.Multicore.Compile.Parallella.Util where

import Data.TypedStruct

import Feldspar.Multicore.Compile.Parallella.Imports

import Language.C.Monad as C (CEnv, CGen, Flags (..), defaultCEnv, runCGen)
import Language.C.Syntax as C (Type)
import Language.Embedded.Expression (VarId)
import qualified Language.Embedded.Imperative.CMD as Imp


--------------------------------------------------------------------------------
-- Access lower Feldspar data representation layer
--------------------------------------------------------------------------------

mkArrayRef :: (ArrayWrapper arr, PrimType a) => VarId -> Length -> arr (Data a)
mkArrayRef n l = wrapArr $ Arr 0 (value l) $ Single $ Imp.ArrComp n

arrayRefName :: ArrayWrapper arr => arr (Data a) -> VarId
arrayRefName (unwrapArr -> (Arr _ _ (Single (Imp.ArrComp name)))) = name


--------------------------------------------------------------------------------
-- Access to C code generator and types
--------------------------------------------------------------------------------

cGen :: C.CGen a -> (a, C.CEnv)
cGen = flip C.runCGen (C.defaultCEnv C.Flags)

isCTypeOf :: PrimType' a => proxy a -> C.Type -> Bool
isCTypeOf ty cty = cty == cTypeOf ty

cTypeOf :: PrimType' a => proxy a -> C.Type
cTypeOf = fst . cGen . compType (Proxy :: Proxy PrimType')

