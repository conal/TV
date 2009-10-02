{-# LANGUAGE Rank2Types, TypeOperators, TypeSynonymInstances,
    PatternGuards, FlexibleInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Common
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  Rank2Types
-- 
-- Some common interaction vocabulary 
----------------------------------------------------------------------

module Interface.TV.Common
  (
  -- * Type class
   CommonIns(..), readD, getReadF, CommonOuts(..), putShowC
  , CInput, CInputI, COutput, CTV
  -- * Inputs
  , stringIn, boolIn, readIn  -- , intIn
  -- * Outputs
  , stringOut, boolOut, showOut, interactLine, readShow, interactLineRS
  ) where

-- import Control.Arrow
-- import Control.Applicative

import Control.Compose (OI,Flip(..),Cofunctor(..))

import Interface.TV.Input
import Interface.TV.Output
import Interface.TV.OFun (wrapO)
import Interface.TV.Tangible (TV)

-- import Interface.TV.Misc (readD)


-- | This class captures some useful operations available in some input
-- types, and allows definition of some \"'Common'\" 'Input's
class CommonIns src where
  -- | Input a string (with default)
  getString :: String -> src String
  -- | Read-based input.  Initial value is also used as a default for
  -- failed parse.  Define as 'getReadF' when @src@ is a 'Functor'.
  -- Requires 'Show' as well as 'Read', for displaying the initial value.
  getRead :: (Show a, Read a) => a -> src a
  -- | Input a bool
  getBool :: Bool -> src Bool
  getBool = getRead
{-
  -- | Input an int with default & bounds
  -- TODO: add getDouble or generalize
  getInt :: Int                 -- ^ default
         -> (Int,Int)           -- ^ bounds
         -> src Int
-}


-- | Read with default value.  If the input doesn't parse as a value of
-- the expected type, or it's ambiguous, yield the default value.
readD :: Read a => a -> String -> a
readD dflt str | [(a,"")] <- reads str = a
               | otherwise             = dflt

-- | 'getRead' for 'Functor's
getReadF :: (CommonIns src, Functor src, Show a, Read a) => a -> src a
getReadF dflt = fmap (readD dflt) (getString (show dflt))


instance CommonIns IO where { getString = const getLine; getRead = getReadF }

instance CommonOuts OI where { putString = Flip putStrLn; putShow = putShowC }

-- Hm.  putStrLn vs putStr above?

-- | This class captures some useful operations available in some arrows
-- and allows definition of some \"'Common'\" 'Input's, 'Output's, and
-- TVs.
class CommonOuts snk where
  -- | Output a string
  putString :: snk String
  -- | Shows based outout.  Define as 'putReadC' when @snk@ is a
  -- 'Cofunctor'
  putShow :: Show a => snk a
  -- | Output a bool
  putBool :: snk Bool
  putBool = putShow

putShowC :: (CommonOuts snk, Cofunctor snk, Show a) => snk a
putShowC = cofmap show putString

-- | Inputs that work over all 'CommonInsOuts' typecons.
type CInput a = forall src. (CommonIns src) => Input src a

-- | 'CInput' with initial value
type CInputI a = a -> CInput a

-- | Outputs that work over all 'CommonOuts' typecons.
type COutput a =
  forall src snk. (CommonIns src, CommonOuts snk) => Output src snk a

-- | Convenient type synonym for TVs that work over all 'CommonInsOuts' typecons.
type CTV a = forall src snk. (CommonIns src, CommonOuts snk) => TV src snk a

-- | String input with default
stringIn :: CInputI String
stringIn s = iPrim (getString s)

-- | Bool input with default
boolIn :: CInputI Bool
boolIn b = iPrim (getBool b)

-- -- | Int input, with default and bounds
-- intIn :: Int -> (Int,Int) -> CInput Int
-- intIn dflt bounds = iPrim (getInt dflt bounds)

-- | Input a readable value.  Use default when read fails.
readIn :: (Read a, Show a) => CInputI a
readIn a = iPrim (getRead a)


-- | Output a string
stringOut :: COutput String
stringOut = oPrim putString

-- | Output a bool
boolOut :: COutput Bool
boolOut = oPrim putBool

-- | Output a showable value
showOut :: Show a => COutput a
showOut = oPrim putShow -- cofmap show stringOut

-- | 'Output' version of 'interact'.  Well, not quite, since the IO
-- version uses 'getLine' instead of 'getContents'.  See also
-- 'Interface.TV.interactOut'
interactLine :: String -> COutput (String -> String)
interactLine s = oLambda (stringIn s) stringOut

-- | Handy Read+Show wrapper
readShow :: ( Read a, Show b, CommonIns src, CommonOuts snk
            , Functor src, Cofunctor snk )
         => Output src snk (String->String) -- ^ base output
         -> a                            -- ^ default, when read fails
         -> Output src snk (a -> b)

readShow o dflt = wrapO show (readD dflt) o

-- Tempting to give the following terse type spec:
-- 
-- readShow :: (Read a, Show b) =>
--             CFOutput (String->String) -> a -> CFOutput (a -> b)
--
-- However, the universality requirement on the first argument is too strong.



-- | Read+Show of 'interactLine'
interactLineRS :: ( Read a, Show a, Show b, CommonIns src, CommonOuts snk )
               => a                     -- ^ default, if read fails
               -> Output src snk (a -> b)

interactLineRS dflt = oLambda (readIn dflt) showOut

-- This version requires Functor src & Cofunctor snk
--  interactLineRS a = readShow (interactLine (show a)) a

