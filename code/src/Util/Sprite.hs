{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Util.Sprite where

import Util.Coordinates
import Util.Types
import Util.Draw

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

type Sprite = SpriteExtra ()

data Anchor = TopLeft | Center
  deriving Eq

data SpriteExtra a = SpriteExtra
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
  , _extra :: a
  , _anchor :: Anchor
  }

makeLenses ''SpriteExtra

spriteDef :: SpriteExtra a
spriteDef = SpriteExtra
  { _alpha = 1
  , _rotation = 0
  , _scale = 1
  , _spriteId = -1
  , _anchor = TopLeft
  }

noop :: Picture -> Picture
noop = Scale 1 1

instance Draw (SpriteExtra a) where
  draw SpriteExtra{ _x, _y, _color, _alpha, _pic, _width, _height, _rotation, _scale, _anchor } = let
    (r, g, b) = _color
    sw = 100
    sh = 100
    (x, y) = toCenterCoords (sw, sh) (_x, _y)
    in _pic _width _height
    & Color (makeColor r g b _alpha)
    & Rotate _rotation
    & Scale _scale _scale
    & if _anchor == TopLeft then
        Translate (x + _width / 2) (y - _height / 2)
      else
        Translate x y
