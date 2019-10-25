{-# LANGUAGE RankNTypes #-}

module Util.Animate where

class Animate a where
  animate :: a -> Float -> a
