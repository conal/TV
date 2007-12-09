{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances
           , TypeSynonymInstances, FlexibleInstances, UndecidableInstances
           , IncoherentInstances #-}
-- For ghc 6.6 compatibility
-- {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

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
class DefaultIn src a where
  -- | The default input for a type
  defaultIn :: Input src a

instance CommonIns src => DefaultIn src Bool   where defaultIn = boolIn False
instance CommonIns src => DefaultIn src Int    where defaultIn = readIn 0
instance CommonIns src => DefaultIn src Double where defaultIn = readIn 0
instance CommonIns src => DefaultIn src Float  where defaultIn = readIn 0
instance CommonIns src => DefaultIn src String where defaultIn = stringIn ""

instance (DefaultIn src a, DefaultIn src b) => DefaultIn src (a,b) where
  defaultIn = IPair defaultIn defaultIn

instance (Read a, Show a, CommonIns src, DefaultIn src a) => DefaultIn src [a] where
  defaultIn = readIn []


{----------------------------------------------------------
    Outputs
----------------------------------------------------------}

-- | Class of types that provide a default output
class DefaultOut src snk a where
  -- | The default output for a type
  defaultOut :: Output src snk a

instance (CommonIns src, CommonOuts snk) => DefaultOut src snk Bool   where defaultOut = boolOut
instance (CommonIns src, CommonOuts snk) => DefaultOut src snk Int    where defaultOut = showOut
instance (CommonIns src, CommonOuts snk) => DefaultOut src snk Double where defaultOut = showOut
instance (CommonIns src, CommonOuts snk) => DefaultOut src snk Float  where defaultOut = showOut
instance (CommonIns src, CommonOuts snk) => DefaultOut src snk String where defaultOut = stringOut

-- The OverlappingInstances, UndecidableInstances, and IncoherentInstances
-- are all for the overlap of String and lists.

instance (DefaultOut src snk a, DefaultOut src snk b) => DefaultOut src snk (a,b) where
  defaultOut = OPair defaultOut defaultOut

instance (DefaultIn src a, DefaultOut src snk b) => DefaultOut src snk (a->b) where
  defaultOut = OLambda defaultIn defaultOut

instance (Show a, CommonIns src, CommonOuts snk, DefaultOut src snk a) =>
    DefaultOut src snk [a] where
  defaultOut = showOut
