{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module App.CompleteIcon where

import Util.Coordinates
import Util.Types
import Util.Sprite
import Util.Draw
import DSL.Functor

import Lens.Micro hiding (set)
import Lens.Micro.TH
import Graphics.Gloss hiding (circle, color, scale)

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
    ] ++ map draw _marks)

completeGreen :: RGB
completeGreen = hexToRgb "4f" "8c" "57"

completeGray :: RGB
completeGray = hexToRgb "46" "4e" "51"

mkCompleteIcon :: Float -> Float -> CompleteIcon
mkCompleteIcon x y = CompleteIcon
  { _checkmark = spriteDef
    { _x = x
    , _y = y
    , _color = completeGray
    , _pic = \_ _ -> (Pictures [rectangleSolid 2 1, rectangleSolid 1 3 & Translate 0.5 1] & Translate (-0.5) (-0.5) & Rotate 45 & Scale 3 3)
    , _anchor = Center
    , _scale = 1
    }
  , _circle = spriteDef
    { _x = x
    , _y = y
    , _width = 20
    , _color = completeGreen
    , _extra = 1
    , _rotation = -90
    , _scale = 0.5
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

completeIconCheck :: (Basic CompleteIcon f, Monad f, Parallel f, Set CompleteIcon f) => f ()
completeIconCheck = do
  set checked True
  basic (checkmark . scale) (For 0.05) (To 0)
  set (checkmark . color) completeGreen
  parallel
    (basic (circle . extra) (For 0.2) (To 360))
    (basic (checkmark . scale) (For 0.2) (To 1.2))
  basic (checkmark . scale) (For 0.05) (To 1)

completeIconUncheck :: (Basic CompleteIcon f, Monad f, Parallel f, Set CompleteIcon f) => f ()
completeIconUncheck = do
  set checked False
  set (checkmark . color) completeGray
  parallel
    (do
       basic (checkmark . scale) (For 0.2) (To 0.8)
       basic (checkmark . scale) (For 0.05) (To 1)
    )
    (basic (circle . extra) (For 0.05) (To 1))
