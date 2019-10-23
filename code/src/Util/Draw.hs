module Util.Draw where

import Graphics.Gloss (Picture)

class Draw a where
  draw :: a -> Picture
