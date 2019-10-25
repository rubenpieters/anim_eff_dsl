{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module App.Navbar where

import Util.Types
import Util.Sprite
import Util.Draw

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data Navbar = Navbar
  { _navbarBg :: Sprite
  , _btn1 :: Sprite
  , _btn2 :: Sprite
  , _btn3 :: Sprite
  , _underline1 :: Sprite
  , _underline2 :: Sprite
  , _underline3 :: Sprite
  , _menuBtn :: Sprite
  , _selectedBtn :: Int
  }

makeLenses ''Navbar

instance Draw Navbar where
  draw Navbar{ _navbarBg, _btn1, _btn2, _btn3, _underline1, _underline2, _underline3, _menuBtn } =
    Pictures
    [ draw _navbarBg
    , draw _btn1
    , draw _btn2
    , draw _btn3
    , draw _underline1
    , draw _underline2
    , draw _underline3
    , draw _menuBtn
    ]

initialNavbar :: Navbar
initialNavbar = Navbar
  { _navbarBg = spriteDef
    { _x = 0
    , _y = 0
    , _width = 100
    , _height = 25
    , _color = hexToRgb "55" "7a" "95"
    , _pic = rectangleSolid
    }
  , _btn1 = spriteDef
    { _x = 4
    , _y = 4
    , _width = 20
    , _height = 10
    , _color = hexToRgb "73" "95" "ae"
    , _pic = rectangleSolid
    }
  , _btn2 = spriteDef
    { _x = 32
    , _y = 4
    , _width = 20
    , _height = 10
    , _color = hexToRgb "73" "95" "ae"
    , _pic = rectangleSolid
    }
  , _btn3 = spriteDef
    { _x = 60
    , _y = 4
    , _width = 20
    , _height = 10
    , _color = hexToRgb "73" "95" "ae"
    , _pic = rectangleSolid
    }
  , _underline1 = spriteDef
    { _x = 0
    , _y = 17
    , _width = 28
    , _height = 8
    , _color = hexToRgb "5d" "5c" "61"
    , _pic = rectangleSolid
    }
  , _underline2 = spriteDef
    { _x = 28
    , _y = 17
    , _width = 0
    , _height = 8
    , _color = hexToRgb "5d" "5c" "61"
    , _pic = rectangleSolid
    }
  , _underline3 = spriteDef
    { _x = 56
    , _y = 17
    , _width = 0
    , _height = 8
    , _color = hexToRgb "5d" "5c" "61"
    , _pic = rectangleSolid
    }
  , _menuBtn = spriteDef
    { _x = 86
    , _y = 0
    , _width = 15
    , _height = 25
    , _color = (1, 1, 1)
    , _pic = menuBtnPic
    }
  , _selectedBtn = 1
}

menuBtnPic :: Float -> Float -> Picture
menuBtnPic w h = Pictures
  [ rectangleSolid 0.8 0.15
  , rectangleSolid 0.8 0.15 & Translate 0 (-0.23)
  , rectangleSolid 0.8 0.15 & Translate 0 0.23
  ] & Scale w h

btn :: Int -> Lens' Navbar Sprite
btn 1 = btn1
btn 2 = btn2
btn 3 = btn3
btn i = error ("unknown button " ++ show i)

underline :: Int -> Lens' Navbar Sprite
underline 1 = underline1
underline 2 = underline2
underline 3 = underline3
underline i = error ("unknown underline " ++ show i)
