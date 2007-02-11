{-# OPTIONS -fglasgow-exts -cpp #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Input
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module defines an 'Input' type constructor, for obtaining typed
-- values from a user.
----------------------------------------------------------------------

module Interface.TV.Input
  (
  -- * Input data type
    Input(..)
  -- * Canonicalizers
  , asIPair
  {-, iEmpty-}, iPrim, iPair, iCompose, iTitle
  ) where

import Control.Arrow

{----------------------------------------------------------
    Input data type
----------------------------------------------------------}

-- | An /Input/ describes a way to obtain a functional value from a user.
-- Used in Output for making function visualizations.
#ifdef __HADDOCK__
data Input (~>) a
#else
data Input (~>) :: * -> * where
  -- | When we don't know what input to use.   I might remove this constructor.
  -- IEmpty :: Input (~>) a
  -- | Input primitive
  IPrim :: () ~> a -> Input (~>) a
  -- | Input a pair
  IPair :: Input (~>) a -> Input (~>) b -> Input (~>) (a,b)
  -- | Massage via an arrow value (generalizes fmap)
  -- ICompose :: Input (~>) a -> a ~> b -> Input (~>) b
  -- | Title/label an input
  ITitle :: String -> Input (~>) a -> Input (~>) a
#endif

-- See 'OEmpty' for note about eliminating OEmpty.

instance Show (Input (~>) a) where
  -- show IEmpty          = "IEmpty"
  show (IPrim _)       = "(IPrim _)"
  show (IPair a b)     = "(IPair "++show a++" "++show b++")"
  -- show (ICompose a _)  = "(ICompose "++show a++" _)"
  show (ITitle str i)  = "(ITitle "++show str++" "++show i++")"

{----------------------------------------------------------
    Canonicalizers
----------------------------------------------------------}

-- | Dissect a pair-valued input into two inputs.  Loses outer 'iTitle's.
-- Yields empty inputs when not a (possibly titled) pair-style input.
asIPair :: Input (~>) (a,b) -> (Input (~>) a, Input (~>) b)
asIPair (IPair  a b ) = (a,b)
asIPair (ITitle _ ab) = asIPair ab
asIPair i             = error ("asIPair of non-IPair "++show i)
-- asIPair _             = (iEmpty, iEmpty)

-- Alternatively, transform titles
-- asIPair (ITitle s ab) = ( ITitle ("first of " ++s) a
--                         , ITitle ("second of "++s) b )
--  where
--    (a,b) = asIPair ab


{----------------------------------------------------------
    Input functions
----------------------------------------------------------}

-- The rest just rename the constructors.  Maybe eliminate.
-- Keep for now, since Haddock can't digest the constructor declarations.

{-
-- | An empty (invisible) input for when we don't know what else to do.
-- Careful: this one probably yields bottom when used.
iEmpty :: Input (~>) a
iEmpty = IEmpty
-}

-- Alternatively, eliminate IEmpty and define

-- iEmpty = iPrim $ (~>) $ const $ error "cannot get value from empty input"

-- | Input primitive
iPrim :: () ~> a -> Input (~>) a
iPrim = IPrim

-- | Input a pair
iPair :: Input (~>) a -> Input (~>) b -> Input (~>) (a,b)
iPair = IPair

-- | Massage via an arrow value (generalizes fmap)
iCompose :: Arrow (~>) => Input (~>) a -> a ~> b -> Input (~>) b
IPrim put `iCompose` arr = IPrim (put >>> arr)
i         `iCompose` _   = error ("iCompose given non-IPrim: "++show i)

-- iCompose = ICompose

-- | Title (label) an input
iTitle :: String -> Input (~>) a -> Input (~>) a
iTitle = ITitle


-- | Handy specialization of 'iCompose'
instance Arrow (~>) => Functor (Input (~>)) where
  fmap f input = input `iCompose` pure f
