{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts#-}

module App.State where

import Util.Types
import Util.Draw
import Util.Sprite
import App.Navbar
import App.MainWindow
import App.Menu
import DSL.Functor
import DSL.Animation

import Control.Monad.Identity
import Lens.Micro.TH
import Graphics.Gloss

data Application = Application
  { _mainWindow :: MainWindow
  , _menu :: Menu
  , _obscuringBox :: Sprite
  , _navbar :: Navbar
  , _animations :: Animation Application Identity ()
  , _menuAnimation :: Animation Application Identity ()
  }

makeLenses ''Application

instance Draw Application where
  draw Application{ _mainWindow, _menu, _obscuringBox, _navbar } =
    Pictures
    [ draw _mainWindow
    , draw _navbar
    , draw _obscuringBox
    , draw _menu
    ]

initialApplication :: Application
initialApplication = Application
  { _mainWindow = initialMainWindow
  , _menu = initialMenu
  , _obscuringBox = spriteDef
    { _x = 0
    , _y = 0
    , _width = 100
    , _height = 100
    , _color = (0, 0, 0)
    , _pic = rectangleSolid
    , _alpha = 0
    }
  , _navbar = initialNavbar
  , _animations = return ()
  , _menuAnimation = return ()
  }

menuSlideIn :: (Basic Application f) => f ()
menuSlideIn = basic (menu . width) (For 0.5) (To 75)

appFadeOut :: (Basic Application f) => f ()
appFadeOut = basic (obscuringBox . alpha) (For 0.5) (To 0.5)

menuIntro :: (Basic Application f, Parallel f) => f ()
menuIntro = menuSlideIn `parallel` appFadeOut

menuOutro :: (Basic Application f, Parallel f) => f ()
menuOutro = basic (menu . width) (For 0.5) (To 0)
  `parallel` basic (obscuringBox . alpha) (For 0.5) (To 0)

line1Outro :: (Basic Application f) => f ()
line1Outro = basic (navbar . underline1 . width) (For 0.25) (To 0)

line2Intro :: (Basic Application f) => f ()
line2Intro = basic (navbar . underline2 . width) (For 0.25) (To 28)

selectBtn2Anim :: (Basic Application f, Applicative f) => f ()
selectBtn2Anim = line1Outro `sequential` line2Intro
