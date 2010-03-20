{-# LANGUAGE Rank2Types, TypeOperators #-}
-- For the TV & TVFun newtypes:
-- {-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
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

-- -- For the TV & TVFun newtypes:
-- 
-- import Control.Category (Category)
-- import Control.Arrow (Arrow)
-- import Control.Arrow.DeepArrow (DeepArrow)
-- import Data.FunArr (FunArr(..))

-- | Tangible values (TVs).
type TV src snk = Output src snk :*: Id

-- | Arrow on 'TV's
type TVFun src snk = OFun src snk ::*:: (->)

-- | Make a 'TV'
tv :: Output src snk a -> a -> TV src snk a
tv o a = Prod (o, Id a)

-- | Dissect a 'TV'
unTv :: TV src snk a -> (Output src snk a, a)
unTv (Prod (o, Id a)) = (o, a)

-- | Useful to define disambiguating type-specializations of 'runTV'
type RunTV src snk = forall a. TV src snk a -> IO ()

-- | Run a 'TV'
runTV :: ( Title_f snk, Title_f src
         , Lambda src snk, Pair snk, Pair src
         , ToOI snk) => RunTV src snk
runTV tval = unFlip (toOI (output o)) a
  where (o,a) = unTv tval


{-

{--------------------------------------------------------------------
    TV and TVFun newtypes
--------------------------------------------------------------------}

-- To do: use a newtype for TV, for friendlier messages, as follows.
-- Type-checks as of 2010-03-20.

newtype TV' src snk a = TV' (TV src snk a)

-- | 'DeepArrow' corresponding to 'TV'
newtype TVFun' src snk a b = TVFun' ((OFun src snk ::*:: (->)) a b)
  deriving (Category, Arrow, DeepArrow)

-- GHC isn't up for:
-- 
--     deriving instance FunArr (TVFun src snk) (TV src snk)
-- 
-- So give a manual definition:

instance FunArr (TVFun' src snk) (TV' src snk) where
  toArr (TV' tval) = TVFun' (toArr tval)
  TVFun' f $$ TV' wa = TV' (f $$ wa)

-- Then names (TV/TV' & TVFun/TVFun')

-}
