{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module App.Menu where

import Util.Types
import Util.Sprite
import Util.Draw
import Util.Animate

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data MenuStatus = MenuOpen | MenuClosed
  deriving Show

type Menu = SpriteExtra MenuStatus

initialMenu :: Menu
initialMenu = spriteDef
  { _x = 0
  , _y = 0
  , _width = 0
  , _height = 100
  , _color = hexToRgb "31" "55" "70"
  , _pic = menuPic
  , _extra = MenuClosed
  }

menuPic :: Float -> Float -> Picture
menuPic width height = let
  (r, g, b) = hexToRgb "58" "74" "8a"
  itemColor = Color (makeColor r g b 1)
  in Pictures
  [ rectangleSolid width height
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (-0.30 * height) & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (-0.15 * height) & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) 0 & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (0.15 * height) & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (0.30 * height) & itemColor
  ]
