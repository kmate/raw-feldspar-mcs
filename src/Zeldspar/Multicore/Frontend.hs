module Zeldspar.Multicore.Frontend where

import Control.Monad.Trans as C (lift)

import Feldspar.Run.Representation
import Feldspar.Multicore.Channel
import Feldspar.Multicore.Representation hiding (OnCore)
import Zeldspar.Multicore.Representation

import Ziria (liftIn, liftOut)


on :: ( Transferable minp, TransferType CoreComp cinp minp
      , Transferable mout, TransferType CoreComp cout mout )
   => CoreZ cinp cout a
   -> CoreId
   -> MulticoreZ minp mout a
on = OnCore . liftOut toTransfer . liftIn fromTransfer


-- | Parallel composition of Ziria programs. It should be used in conjunction
--   with (>>|) to create a mixfix operator with a channel size specifier in the
--   middle, for example 'a |>>n>>| b' composes 'a' and 'b' through a channel of
--   size 'n'.
(|>>) :: Transferable mid
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

liftHost :: Run a -> Host a
liftHost = C.lift
