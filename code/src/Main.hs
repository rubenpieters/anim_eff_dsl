module Main where

import Util.Coordinates
import Util.Draw
import Util.Sprite
import App.State
import App.Menu
import DSL.Animation
import DSL.Functor

import Lens.Micro
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
  w1 = case (insideNavBarBtn (mouseX, mouseY), w ^. menu . extra) of
     (Nothing, _) -> w0
     (_, MenuOpen) -> w0
     (Just 1, _) -> w0
     (Just 2, _) -> w0 & animations %~ \l -> l `parallel` selectBtn2Anim
     (Just 3, _) -> w0
     (Just _, _) -> w0
  in w1
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
