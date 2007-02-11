{-# OPTIONS -fglasgow-exts #-}

{- |
   Module      :  Interface.TV.Tangible
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  experimental
   Portability :  portable

-}

module Interface.TV.Tangible
  (
  -- * Tangible values
   TV, tv, unTv, RunTV, runTV
  ) where

import Control.Arrow
import Control.Monad.Identity

import Data.Tupler
import Interface.TV.Output
import Interface.TV.Present
import Interface.TV.Misc

-- import Control.Arrow.DeepArrow
-- import Data.FunArr
-- import Interface.TV.OFun 

-- | Tangible values (TVs).
type TV (~>) a = Pair1 (Output (~>)) Identity a

-- To do: use a newtype for TV, for friendlier messages.  Requires TVFun
-- and FunArr instance below.  Unfortunately, GHC will not automatically
-- derive the instances I'll need.

-- -- | 'DeepArrow' corresponding to 'TV'
-- newtype TVFun (~>) a b = TVFun (Pair2 (OFun (~>)) (->) a b) deriving DeepArrow

-- instance FunArr (~>) => FunArr (TVFun (~>)) (TV (~>))

-- | Make a 'TV'
tv :: Output (~>) a -> a -> TV (~>) a
tv o a = Pair1 (o, return a)

-- | Dissect a 'TV'
unTv :: TV (~>) a -> (Output (~>) a, a)
unTv (Pair1 (o, ida)) = (o, runIdentity ida)

-- | To define disambiguating type-specializations
type RunTV (~>) = forall a. TV (~>) a -> IO ()

-- | Run a 'TV'
runTV :: (ToIO (~>), Present (~>)) => RunTV (~>)
runTV teevee = toIO (pure (const a) >>> present o)
  where (o,a) = unTv teevee
