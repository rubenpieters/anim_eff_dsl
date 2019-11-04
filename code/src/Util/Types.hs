module Util.Types where

import Numeric (readHex)

type RGB = (Float, Float, Float)

hexToRgb :: String -> String -> String -> RGB
hexToRgb v1 v2 v3 = let
  toRgbVal x = fst (head (readHex x)) / 256
  r = toRgbVal v1
  g = toRgbVal v2
  b = toRgbVal v3
  in (r, g, b)

newtype Duration = For { getDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup Duration where
  (<>) = mappend

instance Monoid Duration where
  mempty = For 0
  mappend (For a) (For b) = For (a + b)

newtype MaxDuration = MaxFor { getMaxDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup MaxDuration where
  (<>) = mappend

instance Monoid MaxDuration where
  mempty = MaxFor 0
  mappend (MaxFor a) (MaxFor b) = MaxFor (a + b)

newtype Target = To { getTarget :: Float }
  deriving (Ord, Eq, Show)
