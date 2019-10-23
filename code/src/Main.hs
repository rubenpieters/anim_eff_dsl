module Main where

import Util.Draw
import App.State

import Lens.Micro
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = let
  bgColor = makeColor 0 0 0 1
  sw = 100
  sh = 100
  window = InWindow "animation-dsl" (sw, sh) (100, 100)
  in play window bgColor 60 initialApplication draw (\_ w -> w) (\_ w -> w)
