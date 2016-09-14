{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Feldspar.Multicore.Compile.Parallella.Host where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Feldspar.Multicore.Compile.Parallella.Channel
import Feldspar.Multicore.Compile.Parallella.Core
import Feldspar.Multicore.Compile.Parallella.Esdk
import Feldspar.Multicore.Compile.Parallella.Imports
import Feldspar.Multicore.Compile.Parallella.State
import Feldspar.Multicore.Compile.Parallella.Util

import Feldspar.Run.Concurrent (forkWithId)
import Feldspar.Run.Compile (env0, ProgC, translate, TargetCMD)

import qualified Language.C.Monad as C (_funUsedVars, _globals, _includes, CEnv, cenvToCUnit, wrapMain)
import Language.C.Quote.C
import qualified Language.C.Syntax as C (Id (..))
import qualified Language.Embedded.Concurrent.CMD as Imp
import qualified Language.Embedded.Imperative.CMD as Imp


--------------------------------------------------------------------------------
-- Forking threads
--------------------------------------------------------------------------------

compThreadCMD :: Imp.ThreadCMD (Param3 RunGen Data PrimType') a -> RunGen a
compThreadCMD (Imp.ForkWithId p) = do
    s <- genState
    let p' = evalGen s . p
    fromRun $ forkWithId p'

instance Interp Imp.ThreadCMD RunGen (Param2 Data PrimType')
  where interp = compThreadCMD


--------------------------------------------------------------------------------
-- Core-local array fetch and flush
--------------------------------------------------------------------------------

compLocalBulkArrCMD :: (BulkArrCMD LArr) (Param3 RunGen exp pred) a -> RunGen a
compLocalBulkArrCMD (WriteArr offset spm range ram) =
    compLocalCopy "host_write_local"  spm ram offset range
compLocalBulkArrCMD (ReadArr  offset spm range ram) =
    compLocalCopy "host_read_local"   spm ram offset range

instance Interp (BulkArrCMD LArr) RunGen (Param2 exp pred)
  where interp = compLocalBulkArrCMD

compLocalCopy :: PrimType a => String
              -> DLArr a -> Arr (Data a)
              -> Data Index -> IndexRange -> RunGen ()
compLocalCopy op spm ram offset (lower, upper) = do
    groupAddr <- gets group
    (r, c) <- gets $ groupCoordsForName (arrayRefName spm)
    lift $ addInclude "<feldspar-parallella.h>"
    lift $ callProc op
        [ groupAddr
        , valArg $ value r
        , valArg $ value c
        , arrArg (unwrapArr spm)
        , arrArg ram
        , valArg offset
        , valArg lower
        , valArg upper
        ]


--------------------------------------------------------------------------------
-- Shared array fetch and flush
--------------------------------------------------------------------------------

compSharedBulkArrCMD :: (BulkArrCMD SArr) (Param3 RunGen exp pred) a -> RunGen a
compSharedBulkArrCMD (WriteArr offset spm range ram) =
    compSharedCopy "host_write_shared" spm ram offset range
compSharedBulkArrCMD (ReadArr  offset spm range ram) =
    compSharedCopy "host_read_shared"  spm ram offset range

instance Interp (BulkArrCMD SArr) RunGen (Param2 exp pred)
  where interp = compSharedBulkArrCMD

compSharedCopy :: PrimType a => String
               -> DSArr a -> Arr (Data a)
               -> Data Index -> IndexRange -> RunGen ()
compSharedCopy op spm ram offset (lower, upper) = do
    shmRef <- gets $ shmRefForName $ arrayRefName spm
    lift $ addInclude "<feldspar-parallella.h>"
    lift $ callProc op
        [ shmRef
        , arrArg ram
        , valArg offset
        , valArg lower
        , valArg upper
        ]


--------------------------------------------------------------------------------
-- Halting cores
--------------------------------------------------------------------------------

compHostHaltCMD :: CoreHaltCMD (Param3 RunGen exp pred) a -> RunGen a
compHostHaltCMD (HaltCore (CoreRefComp coreId)) = do
    groupAddr <- gets group
    let (r, c) = groupCoord coreId
    lift $ callProc "e_halt" [ groupAddr, valArg $ value r, valArg $ value c ]

instance Interp CoreHaltCMD RunGen (Param2 exp pred)
  where interp = compHostHaltCMD


--------------------------------------------------------------------------------
-- Spawning core programs
--------------------------------------------------------------------------------

moduleName :: CoreId -> String
moduleName = ("core" ++) . show

compMulticoreCMD :: MulticoreCMD (Param3 RunGen exp pred) a -> RunGen a
compMulticoreCMD (OnCore coreId comp) = do
    s <- genState
    let coreRef = CoreRefComp coreId
    compCore coreId
        $ evalGen s
        $ interpretT ((lift :: Run a -> CoreGen a) . liftRun)
        $ unCoreComp
        $ comp coreRef
    groupAddr <- gets group
    let (r, c) = groupCoord coreId
    lift $ do
        addInclude "<e-loader.h>"
        callProc "e_load"
            [ strArg $ moduleName coreId ++ ".srec"
            , groupAddr
            , valArg $ value r
            , valArg $ value c
            , valArg (value 0 :: Data Int32) {- E_FALSE -}
            ]
    -- TODO:
    --  * insert all core data initializer code here
    --  * possibily aggregate those in RunGen state on host command compilation
    --    * when an instruction has a core id, and the core is not started
    --    * then do not emit the instruction, just collect it
    --    * then emit the collected code for the current core here!
        callProc "e_start"
            [ groupAddr
            , valArg $ value r
            , valArg $ value c
            ]
    return coreRef

instance Interp MulticoreCMD RunGen (Param2 exp pred)
  where interp = compMulticoreCMD


--------------------------------------------------------------------------------
-- Core program alignment transformation
--------------------------------------------------------------------------------

type TargetPrams = '(Program TargetCMD (Param2 Prim PrimType'), Param2 Prim PrimType')

alignArrays :: ProgC a -> ProgC a
alignArrays = go . view
  where
    go :: ProgramView TargetCMD (Param2 Prim PrimType') a -> ProgC a
    go (Return x) = unview (Return x)
    go (x :>>= y) = unview (align (traverse x) :>>= \r -> go (view (y r)))

    traverse :: forall a. TargetCMD TargetPrams a -> TargetCMD TargetPrams a
    traverse x = case (prj x :: Maybe (Imp.ControlCMD TargetPrams a)) of
        Just (Imp.If cond t f)     -> inj $ Imp.If cond (alignArrays t) (alignArrays f)
        Just (Imp.For range body)  -> inj $ Imp.For range (\i -> alignArrays (body i))
        Just (Imp.While cond body) -> inj $ Imp.While (alignArrays cond) (alignArrays body)
        _ -> x

    align :: forall a. TargetCMD TargetPrams a -> TargetCMD TargetPrams a
    align x = case (prj x :: Maybe (Imp.ArrCMD TargetPrams a)) of
        Just (Imp.NewArr   base len) -> inj $ Imp.NewCArr   base al len
        Just (Imp.ConstArr base as)  -> inj $ Imp.ConstCArr base al as
        _ -> x
      where
        al :: forall i. Integral i => Maybe i
        al = Just $ fromIntegral dmaAlign


--------------------------------------------------------------------------------
-- Core program main wrapper and declaration generator
--------------------------------------------------------------------------------

compCore :: CoreId -> Run () -> RunGen ()
compCore coreId comp = do
    -- compile the core program to C and collect the resulting environment
    let (_, env) = cGen $ do
            addCoreSpecification coreId
            C.wrapMain $ interpret $ alignArrays $ translate env0 comp

    -- collect pre-allocated local and shared arrays used by core main
    arrayDecls <- mkArrayDecls coreId (mainUsedVars env)

    -- merge type includes and array definitions
    inclMap <- gets inclMap
    let coreTypeIncludes   = fromMaybe Set.empty (Map.lookup coreId   inclMap)
        sharedTypeIncludes = fromMaybe Set.empty (Map.lookup sharedId inclMap)
        env' = env { C._includes = C._includes env
                         `Set.union` coreTypeIncludes
                         `Set.union` sharedTypeIncludes
                   -- cenvToCUnit will reverse the order of definitions
                   , C._globals  = C._globals env ++ reverse arrayDecls }

    -- merge contents to the core module
    lift $ inModule (moduleName coreId)
         $ mapM_ addDefinition (C.cenvToCUnit env')

mainUsedVars :: C.CEnv -> [Name]
mainUsedVars
    = map (\(C.Id name _) -> name)
    . maybe [] Set.toList
    . Map.lookup "main"
    . C._funUsedVars

mkArrayDecls :: CoreId -> [Name] -> RunGen [Definition]
mkArrayDecls coreId usedVars = do
    nameMap <- gets nameMap
    let arrayVars = filter (isJust . flip Map.lookup nameMap) usedVars
    forM arrayVars $ mkArrayDecl coreId

mkArrayDecl :: CoreId -> Name -> RunGen Definition
mkArrayDecl coreId name = do
    typeMap <- gets typeMap
    nameMap <- gets nameMap
    let Just ty = Map.lookup name typeMap
        Just (coreId', addr) = Map.lookup name nameMap
     -- convert address to global when the given array is on another core
        addr'
            | coreId' == coreId = addr
            | otherwise = addr `toGlobal` coreId'
    return $ if coreId' == sharedId
       then [cedecl| volatile void * const $id:name = (void *)$addr; |]
       else [cedecl| volatile $ty:ty * const $id:name = ($ty:ty *)$addr'; |]
