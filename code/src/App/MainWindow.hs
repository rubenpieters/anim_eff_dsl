{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module App.MainWindow where

import Util.Types
import Util.Sprite
import Util.Draw

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data MainWindow = MainWindow
  { _mainBg :: Sprite
  , _completedTodos :: [SpriteExtra Int]
  , _stillTodos :: [SpriteExtra Int]
  }

instance Draw MainWindow where
  draw MainWindow{ _mainBg, _completedTodos, _stillTodos } =
    Pictures $
    [ draw _mainBg
    ] ++ map draw _completedTodos ++ map draw _stillTodos

initialMainWindow :: MainWindow
initialMainWindow = MainWindow
  { _mainBg = spriteDef
    { _x = 0
    , _y = 25
    , _width = 100
    , _height = 75
    , _color = hexToRgb "c4" "c8" "c2"
    , _pic = rectangleSolid
    }
  , _completedTodos = []
  , _stillTodos = []
  }
