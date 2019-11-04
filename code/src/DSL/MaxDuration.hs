{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DSL.MaxDuration where

import Util.Types
import DSL.Functor

import Data.Functor.Const

instance Basic obj (Const MaxDuration) where
  basic _ (For duration) _ = Const (MaxFor duration)

instance Parallel (Const MaxDuration) where
  liftP2 _ (Const x1) (Const x2) = Const (max x1 x2)

instance Set obj (Const MaxDuration) where
  set _ _ = Const (MaxFor 0)

instance Get obj (Const MaxDuration) where
  get _ = Const (MaxFor 0)

instance IfThenElse (Const MaxDuration) where
  ifThenElse (Const (MaxFor durCondition))
             (Const (MaxFor durThen))
             (Const (MaxFor durElse)) =
    Const (MaxFor (durCondition + max durThen durElse))

maxDuration :: Const MaxDuration a -> MaxDuration
maxDuration = getConst
