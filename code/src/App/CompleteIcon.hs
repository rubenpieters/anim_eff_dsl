{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module App.CompleteIcon where

import Util.Coordinates
import Util.Types
import Util.Sprite
import Util.Draw

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data CompleteIcon = CompleteIcon
  { _checkmark :: Sprite
  , _circle :: SpriteExtra Float
  , _marks :: [Sprite]
  , _checked :: Bool
  }

makeLenses ''CompleteIcon

instance Draw CompleteIcon where
  draw CompleteIcon{ _checkmark, _circle, _marks } =
    (Pictures $
    [ draw _checkmark
    , drawCompleteCircle _circle
    ] ++ map draw _marks) & Scale 0.4 0.4

completeGreen :: RGB
completeGreen = hexToRgb "32" "cd" "32"

completeGray :: RGB
completeGray = hexToRgb "46" "4e" "51"

initialCompleteIcon :: CompleteIcon
initialCompleteIcon = CompleteIcon
  { _checkmark = spriteDef
    { _x = 50
    , _y = 50
    , _color = completeGray
    , _pic = \_ _ -> (Pictures [rectangleSolid 2 1, rectangleSolid 1 3 & Translate 0.5 1] & Translate (-0.5) (-0.5) & Rotate 45 & Scale 8 8)
    , _anchor = Center
    , _scale = 1
    }
  , _circle = spriteDef
    { _x = 50
    , _y = 50
    , _width = 20
    , _color = completeGreen
    , _extra = 1
    , _rotation = (-90)
    }
  , _marks = []
  , _checked = False
  }

drawCompleteCircle :: SpriteExtra Float -> Picture
drawCompleteCircle SpriteExtra{ _x, _y, _color, _alpha, _pic, _width, _height, _rotation, _scale, _extra } = let
  (r, g, b) = _color
  sw = 100
  sh = 100
  (x, y) = toCenterCoords (sw, sh) (_x, _y)
  in ThickArc 0 _extra _width 6
  & Color (makeColor r g b _alpha)
  & Rotate _rotation
  & Scale _scale _scale
  & Translate x y
