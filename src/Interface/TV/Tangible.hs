{-# LANGUAGE Rank2Types, TypeOperators #-}
----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Tangible
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  Rank2Types
-- 
-- Tangible values -- interface (output) and value, combined & separable
----------------------------------------------------------------------

module Interface.TV.Tangible
  (
  -- * Tangible values
   TV, TVFun, tv, unTv, RunTV, runTV
  ) where

import Control.Compose (Id(..),(:*:)(..),(::*::)(..),Flip(..),ToOI(..))
import Data.Pair
import Data.Lambda
import Data.Title

-- import Data.Tupler
import Interface.TV.Output
import Interface.TV.OFun

-- import Control.Arrow.DeepArrow
-- import Data.FunArr
-- import Interface.TV.OFun 

-- | Tangible values (TVs).
type TV src snk = Output src snk :*: Id

-- | Arrow on 'TV's
type TVFun src snk = OFun src snk ::*:: (->)


-- To do: use a newtype for TV, for friendlier messages.  Requires TVFun
-- and FunArr instance below.  Unfortunately, GHC will not automatically
-- derive the instances I'll need.

-- -- | 'DeepArrow' corresponding to 'TV'
-- newtype TVFun src snk a b = TVFun (Pair2 (OFun src snk) (->) a b) deriving DeepArrow

-- instance FunArr src snk => FunArr (TVFun src snk) (TV src snk)

-- | Make a 'TV'
tv :: Output src snk a -> a -> TV src snk a
tv o a = Prod (o, Id a)

-- | Dissect a 'TV'
unTv :: TV src snk a -> (Output src snk a, a)
unTv (Prod (o, ida)) = (o, unId ida)

-- | Useful to define disambiguating type-specializations of 'runTV'
type RunTV src snk = forall a. TV src snk a -> IO ()

-- | Run a 'TV'
runTV :: ( Title_f snk, Title_f src
         , Lambda src snk, Pair snk, Pair src
         , ToOI snk) => RunTV src snk
runTV teevee = unFlip (toOI (output o)) a
  where (o,a) = unTv teevee
