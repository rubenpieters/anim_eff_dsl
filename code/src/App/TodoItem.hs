{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module App.TodoItem where

import Util.Types
import Util.Sprite
import Util.Draw
import App.CompleteIcon

import Lens.Micro
import Lens.Micro.TH
import Graphics.Gloss

data TodoItem = TodoItem
  { _todoItemBg :: Sprite
  , _completeIcon :: CompleteIcon
  }

makeLenses ''TodoItem

instance Draw TodoItem where
  draw TodoItem{ _todoItemBg, _completeIcon } =
    Pictures
    [ draw _todoItemBg
    , draw _completeIcon
    ]

mkTodoItem :: Float -> Float -> TodoItem
mkTodoItem x y = TodoItem
  { _todoItemBg = spriteDef
    { _x = x + 15
    , _y = y + 3.75
    , _width = 60
    , _height = 20
    , _color = hexToRgb "73" "95" "ae"
    , _pic = rectangleSolid
    }
  , _completeIcon = mkCompleteIcon (x + 88) (y + 13.75)
  }
