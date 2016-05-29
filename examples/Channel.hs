module Channel where

import qualified Prelude
import Feldspar.Data.Vector
import Feldspar.Multicore

primitives :: Multicore ()
primitives = do
    c0 <- newChan hostId 0      one
    c1 <- newChan 0      1      one
    c2 <- newChan 1      hostId one
    onHost $ do
        onCore 0 (f c0 c1)
        onCore 1 (g c1 c2)

        while (return $ true) $ do
            item <- lift $ fget stdin
            writeChan c0 item

            slot <- newSlot c2
            readChan c2 slot
            item :: Data Int32 <- getSlot slot
            printf "> %d\n" item

        closeChan c1
        closeChan c2


f :: CoreChan (Data Int32) -> CoreChan (Data Int32) -> CoreComp ()
f input output = forever $ do
    slot <- newSlot input
    readChan input slot
    elem <- getSlot slot
    void $ writeChan output (elem + 1)

g :: CoreChan (Data Int32) -> CoreChan (Data Int32) -> CoreComp ()
g input output = forever $ do
    slot <- newSlot input
    readChan input slot
    elem <- getSlot slot
    void $ writeChan output (elem * 2)


------------------------------------------------------------

vectors :: Length -> Multicore ()
vectors vecSize = do
    let chanSize = Prelude.fromIntegral vecSize
    c0 <- newChan hostId 0      chanSize
    c1 <- newChan 0      1      chanSize
    c2 <- newChan 1      hostId chanSize
    onHost $ do
        onCore 0 (inc   c0 c1)
        onCore 1 (twice c1 c2)

        while (return $ true) $ do
            arr <- newArr $ value vecSize
            for (0, 1, Excl $ value vecSize) $ \i -> do
                item <- lift $ fget stdin
                setArr i item arr
            lenRef <- initRef (value vecSize :: Data Length)
            let input = Store (lenRef, arr)
            writeChan c0 input

            slot <- newSlot c2
            readChan c2 slot
            store :: Store (DPull Int32) <- getSlot slot
            output <- unsafeFreezeStore store
            for (0, 1, Excl $ value vecSize) $ \i -> do
                let item :: Data Int32 = output ! i
                printf "> %d\n" item

        closeChan c1
        closeChan c2


inc :: CoreChan (Store (DPull Int32)) -> CoreChan (Store (DPull Int32)) -> CoreComp ()
inc inp out = forever $ do
    slot <- newSlot inp
    readChan inp slot
    store :: Store (DPull Int32) <- getSlot slot
    v <- unsafeFreezeStore store
    v' <- initStore $ fmap (+1) v
    void $ writeChan out v'

twice :: CoreChan (Store (DPull Int32)) -> CoreChan (Store (DPull Int32)) -> CoreComp ()
twice inp out = forever $ do
    slot <- newSlot inp
    readChan inp slot
    store :: Store (DPull Int32) <- getSlot slot
    v <- unsafeFreezeStore store
    v' <- initStore $ fmap (*2) v
    void $ writeChan out v'


------------------------------------------------------------

test = primitives

testAll = do
    icompileAll `onParallella` test
    let modules = compileAll `onParallella` test
    forM_ modules $ \(name, contents) -> do
        let name' = if name Prelude.== "main" then "host" else name
        writeFile (name' Prelude.++ ".c") contents

runTestCompiled = runCompiled' opts test
  where
    opts = defaultExtCompilerOpts
        { externalFlagsPre  = [ "-I../imperative-edsl/include"
                              , "../imperative-edsl/csrc/chan.c"]
        , externalFlagsPost = [ "-lpthread" ]
        }
