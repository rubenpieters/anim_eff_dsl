{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Util.Sprite where

import Util.Types
import Util.Draw

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data Sprite = Sprite
  { _x :: Float
  , _y :: Float
  , _color :: RGB
  , _alpha :: Float
  , _pic :: Float -> Float -> Picture
  , _width :: Float
  , _height :: Float
  , _rotation :: Float
  , _scale :: Float
  , _spriteId :: Int
  }

makeLenses ''Sprite

spriteDef :: Sprite
spriteDef = Sprite
  { _alpha = 1
  , _rotation = 0
  , _scale = 1
  , _spriteId = -1
  }

instance Draw Sprite where
  draw Sprite{ _x, _y, _color, _alpha, _pic, _width, _height, _rotation, _scale } = let
    (r, g, b) = _color
    sw = 100
    sh = 100
    in _pic _width _height
    & Color (makeColor r g b _alpha)
    & Rotate _rotation
    & Scale _scale _scale
    & Translate (_x + _width / 2 - sw / 2) (-_y - _height / 2 + sh / 2)
