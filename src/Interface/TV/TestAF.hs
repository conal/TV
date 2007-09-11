{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------
-- |
-- Module      :  TestAF
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  LGPL
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- Portability :  GADT
-- 
-- Experiments in (applicative) functors for TV
----------------------------------------------------------------------

module TestAF
  (
  
  ) where


import Control.Applicative
import Control.Monad (liftM2)
import Data.Monoid
import Control.Arrow hiding (pure)
import Control.Compose
-- import Control.Instances

import qualified Interface.TV.Present as P (Present(..))

-- | An /Input/ describes a way to obtain a functional value from a user.
-- Used in Output for making function visualizations.
data Input :: (* -> *) -> * -> * where
  -- | Input primitive
  IPrim :: f a -> Input f a
  -- | Input a pair
  IPair :: Input f a -> Input f b -> Input f (a,b)
  -- | Title/label an input
  ITitle :: String -> Input f a -> Input f a

-- | An /Output/ describes a way to present a functional value, perhaps
-- interactively.  It is the interface half of a tangible value.
data Output :: (* -> *) -> * -> * -> * where
  -- | Output primitive
  OPrim :: f (a -> o) -> Output f o a
  -- | Visualize a function.  Akin to /lambda/
  OLambda :: Input f a -> Output f o b -> Output f o (a->b)
  -- | Visualize a pair
  OPair :: Output f o a -> Output f o b -> Output f o (a,b)
  -- | Title/label an output
  OTitle :: String -> Output f o a -> Output f o a



ipairA :: Applicative f => f a -> f b -> f (a,b)
ipairA = liftA2 (,)

olambdaA :: Applicative f => f a -> f (b -> o) -> f ((a->b) -> o)
olambdaA = liftA2 (\ a snkb -> \ f -> snkb (f a))

opairA :: (Applicative f, Monoid o) =>
  f (a -> o) -> f (b -> o) -> f ((a,b) -> o)
opairA = liftA2 sinkPair
 where
   sinkPair :: Monoid o => (a -> o) -> (b -> o) -> ((a,b) -> o)
   sinkPair ka kb = \ (a,b) -> ka a `mappend` kb b

-- | 'present' requires a bit of arrow-specific help.  For instance, in
-- UIs, pairs are presented horizontally, lambdas vertically, and titles
-- with a labeled box.  I'm not really comfortable with this part of the
-- design, especially the specificity of addressing titles here.  All of
-- the methods here have do-nothing defaults, so you can simply say
-- @instance Present arr@ for your arrow @arr@.
class Present f where
  presentPair     :: f a -> f a
  presentPair     =  id
  presentLambda   :: f a -> f a
  presentLambda   =  id
  presentTitle    :: String -> f a -> f a
  presentTitle _  =  id

-- | Convert an 'Input' into an arrow value.
accept :: (Applicative f, Present f) => Input f b -> f b

accept (IPrim p)       = p
accept (IPair ia ib)   = presentPair $ ipairA (accept ia) (accept ib)
accept (ITitle str i)  = presentTitle str $ accept i


-- | Convert an 'Output' into an arrow value
present :: (Applicative f, Monoid o, Present f) => Output f o a -> f (a -> o)

present (OPrim p)       = p
present (OPair oa ob)   = presentPair $ opairA (present oa) (present ob)
present (OLambda i o)   = presentLambda $ olambdaA (accept i) (present o)
present (OTitle str o)  = presentTitle str $ present o


---- Next, generalize from the particular style of consumer (@f (a -> o)@)

class Pair cof where
  pair :: cof a -> cof b -> cof (a,b)

class Lambda f cof where
  lambda :: f a -> cof b -> cof (a->b)

-- class Cof f cof | cof -> f where
--   lambda  ::   f a -> cof b -> cof (a->b)
--   pair :: cof a -> cof b -> cof (a, b)


---- Monads

olambdaM :: Monad m => m a -> (b -> m o) -> ((a->b) -> m o)
olambdaM ioa oib = \ f -> ioa >>= oib . f

opairM :: (Monad m, Monoid o) =>
  (a -> m o) -> (b -> m o) -> ((a,b) -> m o)
opairM coa cob = \ (a,b) -> liftM2 mappend (coa a) (cob b)

-- Repackage 'olambdaA', 'opairA' for use in a 'Lambda' instance.

type CoM m o = Flip (->) (m o)

lamM :: Monad m => m a -> CoM m o b -> CoM m o (a->b)
lamM ioa = inFlip (olambdaM ioa)

pairM :: (Monad m, Monoid o) =>
  CoM m o a -> CoM m o b -> CoM m o (a,b)
pairM = inFlip2 opairM

instance Present f => Present (CoM f o) where
  presentPair      = inFlip (presentPair .)
  presentLambda    = inFlip (presentLambda .)
  presentTitle str = inFlip (presentTitle str .)

-- Standard instance: replace IO with any monad
instance Monoid o => Pair (CoM IO o) where
  pair = pairM
instance Lambda IO (CoM IO o) where
  lambda = lamM

-- instance Monoid o => Cof IO (CoM IO o) where
--   lambda  = lamM
--   pair = pairM


---- Applicative functors

-- Repackage 'olambdaA', 'opairA' for use in a 'Lambda' instance.

type CoA f o = O f (Flip (->) o)

coA :: Functor f => f (b -> o) -> CoA f o b
coA = O . fmap Flip

unCoA :: Functor f => CoA f o b -> f (b -> o)
unCoA = fmap unFlip . unO

inCoA :: (Functor f, Functor g)
  => (f (a -> o) -> g (b -> p))
  -> (CoA f o a  -> CoA g p b)
inCoA f = coA . f . unCoA

inCoA2 :: (Functor f, Functor g, Functor h)
  => (f (a -> o) -> g (b -> p) -> h (c -> q))
  -> (CoA f o a  -> CoA g p b  -> CoA h q c)
inCoA2 f coa coa' = coA (f (unCoA coa) (unCoA coa'))

--   or
-- inCoA2 f coa = inCoA (f (unCoA coa))


lamA :: Applicative f => f a -> CoA f o b -> CoA f o (a->b)
lamA fa = inCoA (olambdaA fa)

pairA :: (Applicative f, Monoid o) =>
  CoA f o a -> CoA f o b -> CoA f o (a,b)
pairA = inCoA2 opairA


instance (Functor f, Present f) => Present (CoA f o) where
  presentPair      = inCoA presentPair
  presentLambda    = inCoA presentLambda
  presentTitle str = inCoA (presentTitle str)

-- Standard instance: replace [] with any AF, particularly non-monads.
instance Monoid o => Pair (CoA [] o) where
  pair = pairA
instance Lambda [] (CoA [] o) where
  lambda  = lamA

-- instance Monoid o => Cof [] (CoA [] o) where
--   lambda  = lamA
--   pair = pairA


---- Arrows

olambdaAr :: (Monoid i, Arrow (~>)) => (i ~> a) -> (b ~> o) -> ((a->b) ~> o)
olambdaAr ia ob = 
  arr (\ f -> (f, mempty))  >>>
  second ia                 >>>
  arr (uncurry ($))         >>>
  ob

opairAr :: (Arrow (~>), Monoid o) => (a ~> o) -> (b ~> o) -> ((a,b) ~> o)
opairAr oa ob = (oa *** ob) >>> arr (uncurry mappend)


-- Repackage 'olambdaAr', 'opairAr' for use in a 'Lambda' instance.

lamAr :: (Monoid i, Arrow (~>)) => (i ~> a) -> Flip (~>) o b -> Flip (~>) o (a->b)
lamAr ia = inFlip (olambdaAr ia)

pairAr :: (Monoid o, Arrow (~>)) =>
  Flip (~>) o a -> Flip (~>) o b -> Flip (~>) o (a,b)
pairAr = inFlip2 opairAr


instance P.Present (~>) => Present (Flip (~>) o) where
  presentPair      = inFlip P.presentPair
  presentLambda    = inFlip P.presentLambda
  presentTitle str = inFlip (P.presentTitle str)


-- Standard instances: replace (->) with any arrow and () and [a] with any monoid
instance Pair (Flip (->) [a]) where
  pair = pairAr
instance Lambda ((->) ()) (Flip (->) [a]) where
  lambda  = lamAr

-- instance Cof ((->) ()) (Flip (->) [a]) where
--   lambda  = lamAr
--   pair = pairAr


-- The type packaging used in CoM & CoA (Flip and O) complicate the
-- definitions.  Maybe type-indexed synonyms (not yet implemented) will
-- eliminate them.



-- | Alternative to 'Output'
data Output' :: (* -> *) -> (* -> *) -> (* -> *) where
  -- | Output primitive
  OPrim' :: cof a -> Output' f cof a
  -- | Visualize a function.  Akin to /lambda/
  OLambda' :: Input f a -> Output' f cof b -> Output' f cof (a->b)
  -- | Visualize a pair
  OPair' :: Output' f cof a -> Output' f cof b -> Output' f cof (a,b)
  -- | Title/label an output
  OTitle' :: String -> Output' f cof a -> Output' f cof a

-- | Convert an 'Output' into an arrow value
present' :: (Applicative f, Pair cof, Lambda f cof, Present f, Present cof) =>
  Output' f cof a -> cof a

present' (OPrim' p)       = p
present' (OPair' oa ob)   = presentPair $ pair (present' oa) (present' ob)
present' (OLambda' i o)   = presentLambda $ lambda (accept i) (present' o)
present' (OTitle' str o)  = presentTitle str $ present' o

