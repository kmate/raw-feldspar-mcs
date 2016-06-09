module Zeldspar.Multicore.Frontend where

import Control.Monad.Trans as C (lift)

import Feldspar
import Feldspar.Run.Representation
import Feldspar.Multicore.CoreId
import Feldspar.Multicore.Channel.Frontend
import Feldspar.Multicore.Representation hiding (OnCore)
import Zeldspar.Multicore.Representation

import Ziria (liftIn, liftOut)


on :: ( CoreTransferable minp, CoreTransferType CoreComp cinp minp
      , CoreTransferable mout, CoreTransferType CoreComp cout mout )
   => CoreZ cinp cout a
   -> CoreId
   -> MulticoreZ minp mout a
on = OnCore . liftOut toTransfer . liftIn fromTransfer


-- | Parallel composition of Ziria programs. It should be used in conjunction
--   with (>>|) to create a mixfix operator with a channel size specifier in the
--   middle, for example 'a |>>n>>| b' composes 'a' and 'b' through a channel of
--   size 'n'.
(|>>) :: CoreTransferable mid
      => MulticoreZ inp mid a
      -> SizeSpec mid
      -> (MulticoreZ mid out b -> MulticoreZ inp out ())
(|>>) = flip Connect

(>>|) :: (MulticoreZ mid out a -> MulticoreZ inp out ())
      -> MulticoreZ mid out a
      -> MulticoreZ inp out ()
(>>|) = ($)

infixl 1 |>>
infixl 1 >>|


(|>>>|) :: PrimType mid
        => MulticoreZ inp (Data mid) a
        -> MulticoreZ (Data mid) out b
        -> MulticoreZ inp out ()
a |>>>| b = a |>>one>>| b

infixl 1 |>>>|


liftHost :: Run a -> Host a
liftHost = C.lift
