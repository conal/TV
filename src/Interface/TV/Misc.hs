{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Misc
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Misc helpers
----------------------------------------------------------------------

module Interface.TV.Misc where

import Control.Arrow (Arrow)

-- | Read with default value.  If the input doesn't parse as a value of
-- the expected type, or it's ambiguous, yield the default value.
readD :: Read a => a -> String -> a
readD dflt str | [(a,"")] <- reads str = a
               | otherwise             = dflt

-- | Often useful for \"acceptors\" of values.
class Cofunctor acceptor where
  cofmap :: (a -> b) -> (acceptor b -> acceptor a)

-- | Arrows that convert to IO actions.
class Arrow (~>) => ToIO (~>) where
  -- Result type is restricted to () to allow arr types that yield more
  -- (or fewer) than one value.
  toIO :: () ~> () -> IO ()

-- | Handy wrapping pattern.  For instance, @wrapF show read@ turns a
-- string function into value function.
wrapF :: (c->d) -> (a->b) -> ((b->c) -> (a->d))
wrapF after before f = after . f . before


-- Just a haddock test.  I added this infix support.

-- foodle :: Arrow (~>) => a~>b -> b~>c -> a~>c
-- foodle = undefined

-- doodle :: Arrow arr => a `arr` b
-- doodle = undefined
