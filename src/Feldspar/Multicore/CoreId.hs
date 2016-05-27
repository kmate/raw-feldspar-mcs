module Feldspar.Multicore.CoreId where

import Data.Word


type CoreId = Word32

-- | Id used for identifying host in channels
hostId :: CoreId
hostId = maxBound

-- | Id used to declare arrays in shared memory
sharedId :: CoreId
sharedId = hostId
