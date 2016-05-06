module Zeldspar.Multicore.Representation where

import Feldspar.Multicore
import Ziria

--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

type CoreZ inp out = Z inp out CoreComp ()

data MulticoreZ inp out
  = OnCore (CoreZ inp out) CoreId
    -- TODO: restrict `mid` and replace `Length`
  | forall mid. Connect Length (MulticoreZ inp mid) (MulticoreZ mid out)


--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

on :: CoreZ inp out -> CoreId -> MulticoreZ inp out
on = OnCore

-- | Parallel composition of Ziria programs. It should be used in conjunction
--   with (>>|) to create a mixfix operator with a channel size specifier in the
--   middle, for example 'a |>>n>>| b' composes 'a' and 'b' through a channel of
--   size 'n'.
(|>>) :: MulticoreZ inp mid
      -> Length  -- replace to a size spec
      -> (MulticoreZ mid out -> MulticoreZ inp out)
l |>> len = Connect len l

(>>|) :: (MulticoreZ mid out -> MulticoreZ inp out)
      -> MulticoreZ mid out
      -> MulticoreZ inp out
connP >>| r = connP r

infixl 1 |>>
infixl 1 >>|
