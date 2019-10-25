{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module App.MainWindow where

import Util.Types
import Util.Sprite
import Util.Draw
import App.TodoItem

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data MainWindow = MainWindow
  { _mainBg :: Sprite
  , _completedTodos :: [TodoItem]
  , _stillTodos :: [TodoItem]
  }

makeLenses ''MainWindow

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
  , _stillTodos =
    [ mkTodoItem 0 25
    , mkTodoItem 0 48.75
    , mkTodoItem 0 72.5
    ]
  }
