{-# LANGUAGE GADTs, KindSignatures #-}
----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Input
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  GADTs
-- 
-- Inputs -- means of obtaining values
----------------------------------------------------------------------

module Interface.TV.Input
  (
  -- * Input data type
    Input(..), input
  -- * Input functions
  -- ** Builders
  , iPrim, iPair, iTitle
  -- ** Canonicalizers
  , asIPair
  ) where

import Data.Pair  (Pair(..))
import Data.Title (Title_f(..))

{----------------------------------------------------------
    Input data type
----------------------------------------------------------}

-- | An /Input/ describes a way to obtain a functional value from a user.
-- Used in Output for making function visualizations.
data Input src :: * -> * where
  -- | Input primitive
  IPrim :: src a -> Input src a
  -- | Input a pair
  IPair :: Input src a -> Input src b -> Input src (a,b)
  -- | Massage via an arrow value (generalizes fmap)
  -- ICompose :: Input src a -> src (a -> b) -> Input src b
  -- | Title/label an input
  ITitle :: String -> Input src a -> Input src a

instance Title_f (Input src) where title_f = iTitle

-- See 'OEmpty' for note about eliminating OEmpty.

instance Show (Input src a) where
  -- show IEmpty          = "IEmpty"
  show (IPrim _)       = "(IPrim _)"
  show (IPair a b)     = "(IPair "++show a++" "++show b++")"
  -- show (ICompose a _)  = "(ICompose "++show a++" _)"
  show (ITitle str i)  = "(ITitle "++show str++" "++show i++")"

input :: (Pair src, Title_f src) => Input src t -> src t

input (IPrim src)    = src
input (IPair a b)    = pair (input a) (input b)
input (ITitle str t) = title_f str (input t)


{----------------------------------------------------------
    Input functions
----------------------------------------------------------}

-- The rest just rename the constructors.  Maybe eliminate.
-- Keep for now, since Haddock can't digest the constructor declarations.

-- Alternatively, eliminate IEmpty and define

-- | Input primitive
iPrim :: src a -> Input src a
iPrim = IPrim

-- | Input a pair
iPair :: Input src a -> Input src b -> Input src (a,b)
iPair = IPair

instance Pair (Input src) where pair = iPair

-- | Massage via an arrow value (generalizes fmap)
fmapO :: Functor src => (a -> b) -> Input src a -> Input src b
fmapO f (IPrim fa)     = IPrim (fmap f fa)
fmapO f (ITitle str a) = ITitle str (fmapO f a)
fmapO _ i              = error ("fmap given non-IPrim: "++show i)

-- Not sure about the ITitle choice.  Maybe mention fmap.

instance Functor src => Functor (Input src) where fmap = fmapO

-- | Title (label) an input
iTitle :: String -> Input src a -> Input src a
iTitle = ITitle



{----------------------------------------------------------
    Canonicalizers
----------------------------------------------------------}

-- | Dissect a pair-valued input into two inputs.  Loses outer 'iTitle's.
-- Must be a (possibly titled) pair-style input.
asIPair :: Input src (a,b) -> (Input src a, Input src b)
asIPair (IPair  a b ) = (a,b)
asIPair (ITitle _ ab) = asIPair ab
asIPair i             = error ("asIPair of non-IPair "++show i)
-- asIPair _             = (iEmpty, iEmpty)

-- Alternatively, transform titles
-- asIPair (ITitle s ab) = ( ITitle ("first of " ++s) a
--                         , ITitle ("second of "++s) b )
--  where
--    (a,b) = asIPair ab
