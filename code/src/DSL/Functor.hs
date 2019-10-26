{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGe RankNTypes #-}

module DSL.Functor where

import Util.Types

import Control.Applicative (liftA2)

import Lens.Micro

class Basic obj f where
  basic :: Traversal' obj Float -> Duration -> Target -> f ()

sequential :: (Applicative f) => f () -> f () -> f ()
sequential f1 f2 = liftA2 (\_ _ -> ()) f1 f2

class Parallel f where
  liftP2 :: (a -> b -> c) -> f a -> f b -> f c

parallel :: (Parallel f) => f () -> f () -> f ()
parallel f1 f2 = liftP2 (\_ _ -> ()) f1 f2

class Set obj f where
  set :: Traversal' obj a -> a -> f ()
