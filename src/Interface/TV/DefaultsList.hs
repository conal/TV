{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.DefaultsList
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Default inputs and outputs.  Experimental variant of
-- "Interface.TV.DefaultsList".
-- 
-- This version extend the 'DefaultIn' and 'DefaultOut' type classes to
-- handle lists in a general way, but still handle 'String' (@['Char']@)
-- in the most familiar way.  The 'Read' and 'Show' classes have a special
-- trick.  Can I adapt it?  This module is an incomplete first attempt.
-- See @error@ calls in the code for the gaps.

----------------------------------------------------------------------

module Interface.TV.DefaultsList
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
  -- | The default input for lists
  defaultInList :: CInput [a]

readInL :: Read a => CInput [a]
readInL = readIn []

instance DefaultIn Bool where
  defaultIn     = boolIn False
  defaultInList = readInL

instance DefaultIn Char where
  defaultIn     = readIn 'Q'
  defaultInList = stringIn

instance DefaultIn Int where
  defaultIn     = readIn 0
  defaultInList = readInL

instance DefaultIn Double where
  defaultIn     = readIn 0
  defaultInList = readInL

instance DefaultIn Float where
  defaultIn     = readIn 0
  defaultInList = readInL

-- instance DefaultIn String where
--   defaultIn     = stringIn
--   defaultInList = readInL

instance DefaultIn a => DefaultIn [a] where
  defaultIn     = defaultInList
  defaultInList = error "defaultInList: not yet defined for [a]"

instance (DefaultIn a, DefaultIn b) => DefaultIn (a,b) where
  defaultIn     = IPair defaultIn defaultIn
  defaultInList = error "defaultInList: not yet defined for (a,b)"

{----------------------------------------------------------
    Outputs
----------------------------------------------------------}

-- | Class of types that provide a default output
class DefaultOut a where
  -- | The default output for a type
  defaultOut :: COutput a
  -- | The default output for lists
  defaultOutList :: COutput [a]

instance DefaultOut Bool where
  defaultOut     = boolOut
  defaultOutList = showOut

instance DefaultOut Char where
  defaultOut     = showOut
  defaultOutList = stringOut

instance DefaultOut Int where
  defaultOut     = showOut
  defaultOutList = showOut

instance DefaultOut Double where
  defaultOut     = showOut
  defaultOutList = showOut

instance DefaultOut Float where
  defaultOut     = showOut
  defaultOutList = showOut

-- instance DefaultOut String where
--   defaultOut     = stringOut
--   defaultOutList = showOut

instance DefaultOut a => DefaultOut [a] where
  defaultOut     = defaultOutList
  defaultOutList = error "defaultOutList: not yet defined for [a]"

instance (DefaultOut a, DefaultOut b) => DefaultOut (a,b) where
  defaultOut     = OPair defaultOut defaultOut
  defaultOutList = error "defaultOutList: not yet defined for (a,b)"

instance (DefaultIn a, DefaultOut b) => DefaultOut (a->b) where
  defaultOut     = OLambda defaultIn defaultOut
  defaultOutList = error "defaultOutList: not yet defined for (a,b)"

