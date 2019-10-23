{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGe RankNTypes #-}

module DSL.Functor where

import Util.Types

import Control.Applicative (liftA2)

import Lens.Micro

class Basic obj f where
  basic :: Lens' obj Float -> Duration -> Target -> f ()

seq :: (Applicative f) => f () -> f () -> f ()
seq f1 f2 = liftA2 (\_ _ -> ()) f1 f2

class Par f where
  liftP2 :: (a -> b -> c) -> f a -> f b -> f c

par :: (Par f) => f () -> f () -> f ()
par f1 f2 = liftP2 (\_ _ -> ()) f1 f2
