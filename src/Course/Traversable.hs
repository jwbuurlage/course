{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Functor
import Course.Applicative
import Course.List

-- | All instances of the `Traversable` type-class must satisfy two (three?) laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse Id x ≅ Id x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse g xs = foldRight (\a c -> ((:.) <$> g a) <*> c) (pure Nil) xs

  -- foldRight (a -> c -> c) -> c -> [a] -> c
  -- here a is as a
  -- while c has type f (List b)
  -- given an `f List b` and an `a`, obtain an f List b, we have a function from a to f b
  -- g a = f b
  -- f b -> List (f b) -> List (f b)
  --
  -- OBTAIN: a -> f (List b) -> f (List b)
  -- HAVE: (:.) :: b -> (List b -> List b)
  --       (<$>) (:.) :: f b -> f (List b -> List b)
  --       (<$>) (:.) (g a) :: f (List b -> List b)
  --       (<*>) (<$>) (:.) (g a) :: f List b -> f List b
