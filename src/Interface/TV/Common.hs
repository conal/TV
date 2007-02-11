{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Common
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Some common interaction vocabulary 
----------------------------------------------------------------------

module Interface.TV.Common
  (
  -- * Type class
   CommonInsOuts(..), Common, CInput, COutput, CTV
  -- * Inputs
  , stringIn, boolIn, readIn  -- , intIn
  -- * Outputs
  , stringOut, boolOut, showOut, interactLine, interactRSOut
  ) where

import Control.Arrow

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.Tangible (TV)

import Interface.TV.Misc (readD,Cofunctor(..))

-- | This class captures some useful operations available in some arrows
-- and allows definition of some \"'Common'\" 'Input's, 'Output's, and
-- TVs.
class Arrow (~>) => CommonInsOuts (~>) where
  -- | Output a string
  putString :: String ~> ()
  -- | Input a string
  getString :: () ~> String
  -- | Output a bool
  putBool :: Bool ~> ()
  putBool = pure show >>> putString
  -- | Input a bool
  getBool :: Bool               -- ^ default
          -> () ~> Bool
  getBool dflt = getString >>> pure (readD dflt)
{-
  -- | Input an int with default & bounds
  -- TODO: add getDouble or generalize
  getInt :: Int                 -- ^ default
         -> (Int,Int)           -- ^ bounds
         -> () ~> Int
-}

-- | For operations over all 'CommonInsOuts' arrows.
type Common f a = forall (~>). CommonInsOuts (~>) => f (~>) a

-- | Inputs that work over all 'CommonInsOuts' arrows.
type CInput a = Common Input a

-- | Outputs that work over all 'CommonInsOuts' arrows.
type COutput a = Common Output a

-- | Convenient type synonym for TVs that work over all 'CommonInsOuts' arrows.
type CTV a = Common TV a

-- | String input
stringIn :: CInput String
stringIn = iPrim getString

-- | Bool input
boolIn :: Bool                          -- ^ default
       -> CInput Bool
boolIn dflt = iPrim (getBool dflt)

-- -- | Int input, with default and bounds
-- intIn :: Int -> (Int,Int) -> CInput Int
-- intIn dflt bounds = iPrim (getInt dflt bounds)

-- | Input a readable value.  Use default when read fails.
readIn :: Read a => a                   -- ^ default
       -> CInput a
readIn dflt = fmap (readD dflt) stringIn


-- | Output a string
stringOut :: COutput String
stringOut = oPrim putString

-- | Output a bool
boolOut :: COutput Bool
boolOut = oPrim putBool

-- | Output a showable value
showOut :: Show a => COutput a
showOut = cofmap show stringOut

-- | 'Output' version of 'interact'.  Well, not quite, since the IO
-- version uses 'getLine' instead of 'getContents'.  See also
-- 'Interface.TV.interactOut'
interactLine :: COutput (String -> String)
interactLine = oLambda stringIn stringOut

-- | Read+Show of 'interact'
interactRSOut :: (Read a, Show b)
  => a     -- ^ default, if read fails
  -> COutput (a -> b)
interactRSOut dflt = oLambda (readIn dflt) showOut

-- The following definition is more elegant but loses the oLambda structure.
-- cofmap (wrapF show (readD dflt)) interactLine
