{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Kleisli
-- Copyright   :  (c) Conal Elliott 2006
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Support for monadic TVs, via Kleisli arrows
----------------------------------------------------------------------

module Interface.TV.Kleisli
  (
  -- * Monadic variants
    PresentM(..), ToIOM(..)
  -- * 'Input' and 'Output' makers
  , kIn, kOut
  ) where

import Control.Arrow

import Interface.TV.Input (Input,iPrim)
import Interface.TV.Output (Output,oPrim)
import Interface.TV.Present (Present(..))
import Interface.TV.Misc (ToIO(..))


-- | Monadic variant of 'Present'
class Monad m => PresentM m where
  presentPairM, presentLambdaM :: m a -> m a
  presentTitleM :: String -> m a -> m a
  presentPairM   = id
  presentLambdaM = id

-- | Monadic variant of 'ToIOM'
class Monad m => ToIOM m where
  toIOM :: m a -> IO a

instance PresentM m => Present (Kleisli m) where
  presentPair      = liftK presentPairM
  presentLambda    = liftK presentLambdaM
  presentTitle str = liftK (presentTitleM str)

liftK :: (m b -> m b) -> (Kleisli m a b -> Kleisli m a b)
liftK mf (Kleisli f) = Kleisli (mf . f)

instance ToIOM m => ToIO (Kleisli m) where
  toIO (Kleisli f) = toIOM (f ())


-- | Make an 'Input' from a monadic value
kIn :: m a -> Input (Kleisli m) a
kIn m = iPrim (Kleisli (const m))

-- | Make an 'Output' from a monadic function
kOut :: (b -> m ()) -> Output (Kleisli m) b
kOut f = oPrim (Kleisli f)
