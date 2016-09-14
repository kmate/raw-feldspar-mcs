module Feldspar.Multicore.Compile.Parallella.Access where


--------------------------------------------------------------------------------
-- Utility functions to access lower layers
--------------------------------------------------------------------------------

cGen :: C.CGen a -> (a, C.CEnv)
cGen = flip C.runCGen (C.defaultCEnv C.Flags)

mkArrayRef :: (ArrayWrapper arr, PrimType a) => VarId -> Length -> arr (Data a)
mkArrayRef n l = wrapArr $ Arr 0 (value l) $ Single $ Imp.ArrComp n

arrayRefName :: ArrayWrapper arr => arr (Data a) -> VarId
arrayRefName (unwrapArr -> (Arr _ _ (Single (Imp.ArrComp name)))) = name

