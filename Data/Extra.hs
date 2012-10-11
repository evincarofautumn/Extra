module Data.Extra
  ( Extra(..)
  , catOnes
  , catOthers
  , extra
  , extraToList
  , fromExtra
  , isBoth
  , isOne
  , justOther
  , listToExtra
  , mapExtra
  , mapOne
  , mapOther
  , one
  , otherToList
  , toExtra
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Bind
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Semigroup
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Traversable
import Prelude hiding (foldr)

data Extra a b
  = One a
  | Both a b
  deriving (Eq, Ord, Read, Show)

instance (Monoid a) => Applicative (Extra a) where
  pure = Both mempty
  (<*>) = (<.>)

instance (Monoid a) => Apply (Extra a) where
  One  a1    <.> One  a2    = One  (mappend a1 a2)
  Both a1 _  <.> One  a2    = One  (mappend a1 a2)
  One  a1    <.> Both a2 _  = One  (mappend a1 a2)
  Both a1 b1 <.> Both a2 b2 = Both (mappend a1 a2) (b1 b2)

instance Bifoldable Extra where
  bifold = extra id mappend
  bifoldr f g z = extra (flip f z) $ \ x y ->      f x $ g y z
  bifoldl f g z = extra (f z)      $ \ x y -> flip g y $ f z x

instance Bifoldable1 Extra where
  bifold1 = extra id (<>)

instance Bifunctor Extra where
  bimap  = mapExtra
  first  = mapOne
  second = mapOther

instance Foldable (Extra a) where
  foldr f z = foldr f z . justOther

instance Functor (Extra a) where
  fmap _ (One a)    = One  a
  fmap f (Both a b) = Both a (f b)

instance (Monoid a, Semigroup a, Semigroup b) => Monoid (Extra a b) where
  mempty = One mempty
  mappend = (<>)

instance (Monoid a) => Bind (Extra a) where
  One  a     >>- _ = One a
  Both a1 b1 >>- f = case f b1 of
    One  a2    -> One  (mappend a1 a2)
    Both a2 b2 -> Both (mappend a1 a2) b2

instance (Monoid a) => Monad (Extra a) where
  return = pure
  (>>=) = (>>-)

instance (Monoid a) => MonadPlus (Extra a) where
  mzero = One mempty
  One  a1    `mplus` One  a2    = One  (mappend a1 a2)
  One  a1    `mplus` Both a2 b2 = Both (mappend a1 a2) b2
  Both a1 b1 `mplus` One  a2    = Both (mappend a1 a2) b1
  Both a1 b1 `mplus` Both a2 _  = Both (mappend a1 a2) b1

instance (Semigroup a, Semigroup b) => Semigroup (Extra a b) where
  One  a1    <> One  a2    = One  (a1 <> a2)
  Both a1 b1 <> One  a2    = Both (a1 <> a2) b1
  One  a1    <> Both a2 b2 = Both (a1 <> a2) b2
  Both a1 b1 <> Both a2 b2 = Both (a1 <> a2) (b1 <> b2)

instance Traversable (Extra a) where
  traverse _ (One a)    = pure $ One a
  traverse f (Both a x) = Both a <$> f x

  sequenceA (One a)    = pure $ One a
  sequenceA (Both a x) = Both a <$> x

catOnes :: [Extra a b] -> [a]
catOnes = map one

catOthers :: [Extra a b] -> [b]
catOthers = mapMaybe justOther

extra :: (a -> c) -> (a -> b -> c) -> Extra a b -> c
extra f _ (One  a)   = f a
extra _ g (Both a b) = g a b

extraToList :: Extra a a -> [a]
extraToList (One a) = [a]
extraToList (Both a b) = [a, b]

fromExtra :: b -> Extra a b -> (a, b)
fromExtra b (One  a)   = (a, b)
fromExtra _ (Both a b) = (a, b)

isBoth :: Extra a b -> Bool
isBoth (Both _ _) = True
isBoth _ = False

isOne :: Extra a b -> Bool
isOne (One _) = True
isOne _ = False

justOther :: Extra a b -> Maybe b
justOther (Both _ b) = Just b
justOther (One  _)   = Nothing

listToExtra :: (Monoid a) => [a] -> Extra a b
listToExtra (x:_) = One x
listToExtra [] = One mempty

mapExtra :: (a -> c) -> (b -> d) -> Extra a b -> Extra c d
mapExtra f g (Both a b) = Both (f a) (g b)
mapExtra f _ (One  a)   = One (f a)

mapOne :: (a -> c) -> Extra a b -> Extra c b
mapOne f = mapExtra f id

mapOther :: (b -> c) -> Extra a b -> Extra a c
mapOther f = mapExtra id f

one :: Extra a b -> a
one (One  a)   = a
one (Both a _) = a

otherToList :: Extra a b -> [b]
otherToList (Both _ b) = [b]
otherToList _ = []

toExtra :: a -> Maybe b -> Extra a b
toExtra a Nothing  = One a
toExtra a (Just b) = Both a b
