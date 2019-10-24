module Util.Draw where

import Graphics.Gloss (Picture(..))

class Draw a where
  draw :: a -> Picture

instance Draw () where
  draw _ = Blank
