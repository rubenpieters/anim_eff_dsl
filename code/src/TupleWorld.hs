{-# LANGUAGE FlexibleContexts #-}

module TupleWorld where

import Util.Types
import DSL.Functor
import DSL.Animation

import Data.Functor.Identity

import Lens.Micro

type State = (Float, Float)

state0 :: State
state0 = (0, 0)

x :: Lens' State Float
x = _1

y :: Lens' State Float
y = _2

anim1 :: (Basic State f) => f ()
anim1 = basic x (For 1) (To 50)

anim2 :: (Basic State f) => f ()
anim2 = basic y (For 1) (To 50)

anim3 :: (Basic State f, Applicative f) => f ()
anim3 = anim1 `sequential` anim2

anim4 :: (Basic State f, Parallel f) => f ()
anim4 = anim1 `parallel` anim2

resultLinearTo1 :: (State, Either (Animation State Identity ()) (), Maybe Float)
resultLinearTo1 = runIdentity (runAnimation anim1 state0 0.5)

resultLinearTo2 :: (State, Either (Animation State Identity ()) (), Maybe Float)
resultLinearTo2 = case resultLinearTo1 ^. _2 of
  Left anim -> runIdentity (runAnimation anim (resultLinearTo1 ^. _1) 1)
  Right _ -> error "no animation remainder"

remAnim2 = case resultLinearTo2 ^. _2 of
  Left _ -> error "no animation result"
  Right a -> a

resultSeq1 :: (State, Either (Animation State Identity ()) (), Maybe Float)
resultSeq1 = runIdentity (runAnimation anim3 state0 0.5)

resultSeq2 :: (State, Either (Animation State Identity ()) (), Maybe Float)
resultSeq2 = case resultSeq1 ^. _2 of
  Left anim -> runIdentity (runAnimation anim (resultSeq1 ^. _1) 1)
  Right _ -> error "no animation remainder"

resultSeq3 :: (State, Either (Animation State Identity ()) (), Maybe Float)
resultSeq3 = case resultSeq2 ^. _2 of
  Left anim -> runIdentity (runAnimation anim (resultSeq2 ^. _1) 1)
  Right _ -> error "no animation remainder"

