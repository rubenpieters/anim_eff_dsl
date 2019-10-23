{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module App.State where

import Util.Draw
import App.Navbar

import Lens.Micro.TH
import Graphics.Gloss

data Application = Application
  { _mainWindow :: ()
  , _menu :: ()
  , _navbar :: Navbar
  }

makeLenses ''Application

instance Draw Application where
  draw Application{ _navbar } =
    Pictures
    [ draw _navbar
    ]

initialApplication :: Application
initialApplication = Application
  { _mainWindow = ()
  , _menu = ()
  , _navbar = initialNavbar
  }
