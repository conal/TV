{-# OPTIONS -fglasgow-exts #-}

{- Module      :  Graphics.UI.TV.Input
   Copyright   :  (c) Conal Elliott 2006
   License     :  LGPL

   Maintainer  :  conal@conal.net
   Stability   :  provisional
   Portability :  portable
-}

-- | This module defines an 'Input' type constructor, for obtaining typed
-- values from a user.
module Graphics.UI.TV.Input
  (
  -- * Input data type
    Input(..)
  -- * Output functions
  -- ** General
  , iPrim, iPair
  ) where

import Control.Arrow


{----------------------------------------------------------
    Output data type
----------------------------------------------------------}

-- | An /Input/ describes a way to obtain a functional value from a user.
-- Used in 'Output' for making function visualizations.
data Input arr :: * -> * where
  -- | Input primitive
  IPrim  :: arr () a -> Input arr a
  -- | Input a pair
  IPair  :: Input arr a -> Input arr b -> Input arr (a,b)

instance Show (Input arr a) where
  show (IPrim p)   = "(IPrim _)"
  show (IPair a b)  = "(IPair "++show a++" "++show b++")"


---- Canonicalizers

-- asIPair :: Input arr (a,b) -> Maybe (Input arr a, Input arr b)
-- asIPair (IPair a b) = Just (a,b)
-- asIPair _           = Nothing


{----------------------------------------------------------
    Input functions
----------------------------------------------------------}

-- | Input primitive
iPrim :: arr () a -> Input arr a
iPrim = IPrim

-- | Input a pair
iPair :: Input arr a -> Input arr b -> Input arr (a,b)
iPair = IPair
