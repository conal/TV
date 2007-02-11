{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Defaults
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Default inputs and outputs
-- 
-- TODO: Provide @[a]@ instances for DefaultIn and DefaultOut using the
-- trick for Show @[a]@.  See "Interface.TV.DefaultsList" for a first
-- attempt.
----------------------------------------------------------------------

module Interface.TV.Defaults
  (
  -- * Inputs
   DefaultIn(..)
  -- * Outputs
  , DefaultOut(..)
  ) where

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.Common

{----------------------------------------------------------
    Inputs
----------------------------------------------------------}

-- | Class of types that provide a default input
class DefaultIn a where
  -- | The default input for a type
  defaultIn :: CInput a

instance DefaultIn Bool   where defaultIn = boolIn False
instance DefaultIn Int    where defaultIn = readIn 0
instance DefaultIn Double where defaultIn = readIn 0
instance DefaultIn Float  where defaultIn = readIn 0
instance DefaultIn String where defaultIn = stringIn

instance (DefaultIn a, DefaultIn b) => DefaultIn (a,b) where
  defaultIn = IPair defaultIn defaultIn


{----------------------------------------------------------
    Outputs
----------------------------------------------------------}

-- | Class of types that provide a default output
class DefaultOut a where
  -- | The default output for a type
  defaultOut :: COutput a

instance DefaultOut Bool   where defaultOut = boolOut
instance DefaultOut Int    where defaultOut = showOut
instance DefaultOut Double where defaultOut = showOut
instance DefaultOut Float  where defaultOut = showOut
instance DefaultOut String where defaultOut = stringOut

instance (DefaultOut a, DefaultOut b) => DefaultOut (a,b) where
  defaultOut = OPair defaultOut defaultOut

instance (DefaultIn a, DefaultOut b) => DefaultOut (a->b) where
  defaultOut = OLambda defaultIn defaultOut
