{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module DSL.RelSeq where

import DSL.Duration
import DSL.MaxDuration
import DSL.Functor
import Util.Types

import Data.Functor.Const

relSequential :: forall c g.
  (c (Const Duration), c g, Applicative g, Delay g) =>
  (forall f. c f => f ()) -> g () -> Float -> g ()
relSequential anim1 anim2 offset = let
  dur = getDuration (duration anim1)
  in anim1 `sequential` (delay (dur + offset) *> anim2)

relMaxSequential :: forall c g.
  (c (Const MaxDuration), c g, Parallel g, Applicative g, Delay g) =>
  (forall f. c f => f ()) -> g () -> Float -> g ()
relMaxSequential anim1 anim2 offset = let
  dur = getMaxDuration (maxDuration anim1)
  in anim1 `parallel` (delay (dur + offset) *> anim2)


test1 :: (Basic Float f) => f ()
test1 = basic id (For 1) (To 0)

test2 :: (Basic Float f, IfThenElse f, Applicative f) => f ()
test2 = ifThenElse
  (pure True)
  (basic id (For 1) (To 1))
  (basic id (For 1) (To 1))

class (Basic Float f, IfThenElse f, Applicative f) => Combined f where
instance (Basic Float f, IfThenElse f, Applicative f) => Combined f where

test3 :: (Basic Float f, Delay f, Applicative f, Parallel f, IfThenElse f) => f ()
test3 = relMaxSequential @Combined test1 test2 (-0.5)
