{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Util.Coordinates
import Util.Draw
import Util.Animate
import Util.Sprite
import App.State
import App.Menu
import App.Navbar
import App.CompleteIcon
import App.MainWindow
import App.TodoItem
import DSL.Animation
import DSL.Functor

import Lens.Micro
import Graphics.Gloss hiding (animate)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Export.PNG

handleInput :: Event -> Application -> Application
handleInput (EventKey (MouseButton LeftButton) Down _ centerCoords) w = let
  (mouseX, mouseY) = unCenterCoords (100, 100) centerCoords
  w0 = case (insideMenuBtn (mouseX, mouseY), w ^. menu . extra) of
     (False, _) -> w
     (True, MenuClosed) ->
       w & menuAnimation .~ menuIntro
         & menu . extra .~ MenuOpen
     (True, MenuOpen) ->
       w & menuAnimation .~ menuOutro
         & menu . extra .~ MenuClosed
  prev = w ^. navbar . selectedBtn
  showTodoAnim 1 = showAll
  showTodoAnim 2 = onlyComplete
  showTodoAnim 3 = onlyTodo
  w1 = case (insideNavBarBtn (mouseX, mouseY), w ^. menu . extra) of
     (Nothing, _) -> w0
     (_, MenuOpen) -> w0
     (Just new, _) | new == prev -> w0
     (Just new, _) ->
       w0 & animations %~ (\l -> l `parallel` selectBtnXAnim prev new `parallel` showTodoAnim new)
          & navbar . selectedBtn .~ new
  w2 = case (insideTodoItem (mouseX, mouseY), w ^. menu . extra) of
     (Nothing, _) -> w1
     (_, MenuOpen) -> w1
     (Just i, _) ->
       if w1 ^. mainWindow . todoItems . atIndex (i - 1) . completeIcon . checked then
         w1 & animations %~ (\l -> l `parallel` embed (mainWindow . todoItems . atIndex (i - 1) . completeIcon) completeIconUncheck)
       else
         w1 & animations %~ (\l -> l `parallel` embed (mainWindow . todoItems . atIndex (i - 1) . completeIcon) completeIconCheck)
  in w2
handleInput _ w = w

update :: Float -> Application -> Application
update t w = let
  (w0, newAnimations) = applyIdAnimation w t (w ^. animations)
  (w1, newMenuAnimation) = applyIdAnimation w0 t (w0 ^. menuAnimation)
  in w1 & animations .~ newAnimations
        & menuAnimation .~ newMenuAnimation

main :: IO ()
main = let
  bgColor = makeColor 0 0 0 1
  sw = 100
  sh = 100
  window = InWindow "animation-dsl" (sw, sh) (100, 100)
  in play window bgColor 60 initialApplication draw handleInput update

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)
