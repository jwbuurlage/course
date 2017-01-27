{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
    --(<$>) :: (a -> b) -> Compose f g a -> Compose f g b
    f <$> Compose x = Compose $ (<$>) f <$> x

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
-- pure :: a -> Compose f g a
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
-- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose x) = Compose (lift2 (<*>) f x)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
-- (=<<) :: (a -> Compose f g b) -> Compose f g a -> Compose f g b
  (=<<) = error "can't be done"

-- I think the problem is that when we use bind inside f, we end up with f a as the second argument,
-- and we can not extract a out of that, put it in g, and then put the result back in f. And vice versa.
-- (=<<) :: (a -> (f (g b)) -> (f a) -> f (g b)
