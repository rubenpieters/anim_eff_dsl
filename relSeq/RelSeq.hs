{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module RelSeq where

import Data.Functor.Const
import Control.Applicative (liftA2)

main :: IO ()
main = print "hello"

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- Target

newtype Target = To { getTarget :: Float }
  deriving (Ord, Eq, Show)

-- Duration

newtype Duration = For { getDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup Duration where
  (<>) = mappend

instance Monoid Duration where
  mempty = For 0
  mappend (For a) (For b) = For (a + b)

duration :: Const Duration a -> Duration
duration = getConst

-- LinearTo

class LinearTo obj f where
  linearTo :: Lens' obj Float -> Duration -> Target -> f ()

instance LinearTo obj (Const Duration) where
  linearTo _ duration _ = Const duration

-- Delay

class Delay f where
  delay :: Duration -> f ()

-- Sequential

sequential :: (Applicative f) => f () -> f () -> f ()
sequential f1 f2 = liftA2 (\_ _ -> ()) f1 f2


class Parallel f where
  liftP2 :: (a -> b -> c) -> f a -> f b -> f c

instance Parallel (Const Duration) where
  liftP2 _ (Const x1) (Const x2) = Const (max x1 x2)

parallel :: (Parallel f) => f () -> f () -> f ()
parallel f1 f2 = liftP2 (\_ _ -> ()) f1 f2

-- Relative Sequential

relSequential :: forall c g.
  (c (Const Duration), c g, Applicative g) =>
  (forall f. c f => f ()) -> g () -> Float -> g ()
relSequential anim1 anim2 offset = let
  dur = getDuration (duration anim1)
  in anim1 `sequential` ({- delay (dur + offset) *> -} anim2)

animation1 :: (LinearTo Float f) => f ()
animation1 = linearTo id (For 1) (To 0)

animation2 :: (LinearTo Float f) => f ()
animation2 = linearTo id (For 1) (To 1)

test :: (LinearTo Float f, Applicative f) => f ()
test = relSequential @(LinearTo Float) animation1 animation2 (-0.5)

animationP1 :: (LinearTo Float f, Parallel f) => f ()
animationP1 = linearTo id (For 1) (To 0) `parallel` linearTo id (For 1) (To 2)

animationP2 :: (LinearTo Float f, Parallel f) => f ()
animationP2 = linearTo id (For 1) (To 1) `parallel` linearTo id (For 1) (To 3)

class (LinearTo Float f, Parallel f) => New f where

instance (LinearTo Float f, Parallel f) => New f where

test2 :: (LinearTo Float f, Parallel f, Applicative f) => f ()
test2 = relSequential @New animation1 animationP2 (-0.5)



