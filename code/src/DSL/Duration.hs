{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DSL.Duration where

import Util.Types
import DSL.Functor

import Data.Functor.Const

instance Basic obj (Const Duration) where
  basic _ duration _ = Const duration

instance Parallel (Const Duration) where
  liftP2 _ (Const x1) (Const x2) = Const (max x1 x2)

instance Set obj (Const Duration) where
  set _ _ = Const (For 0)

duration :: Const Duration a -> Duration
duration = getConst
