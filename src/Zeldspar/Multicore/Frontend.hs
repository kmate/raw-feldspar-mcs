module Zeldspar.Multicore.Frontend where

import Control.Monad.Trans as C (lift)

import Feldspar.Run.Representation
import Feldspar.Multicore.Channel
import Feldspar.Multicore.Representation hiding (OnCore)
import Zeldspar.Multicore.Representation


on :: CoreZ inp out -> CoreId -> MulticoreZ inp out
on = OnCore

-- | Parallel composition of Ziria programs. It should be used in conjunction
--   with (>>|) to create a mixfix operator with a channel size specifier in the
--   middle, for example 'a |>>n>>| b' composes 'a' and 'b' through a channel of
--   size 'n'.
(|>>) :: Transferable mid
      => MulticoreZ inp mid
      -> SizeSpec mid
      -> (MulticoreZ mid out -> MulticoreZ inp out)
l |>> len = Connect len l

(>>|) :: (MulticoreZ mid out -> MulticoreZ inp out)
      -> MulticoreZ mid out
      -> MulticoreZ inp out
connP >>| r = connP r

infixl 1 |>>
infixl 1 >>|

liftHost :: Run a -> Host a
liftHost = C.lift
