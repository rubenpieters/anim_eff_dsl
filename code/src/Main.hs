{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Util.Types
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
import DSL.Duration

import Data.Functor.Identity
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
  showTodoAnim 2 = onlyDone
  showTodoAnim 3 = onlyNotDone
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

drawnWorlds :: Draw obj => [obj] -> Picture
drawnWorlds worlds = let
  k = length worlds
  f (i, w) = draw w & Translate (50 - 206 + 104 * i) 0
  whiteBoxTop i = rectangleSolid 5 5 & Color (makeColor 1 1 1 1) & Translate (-203 + 10 * i) (-55)
  whiteBoxBot i = rectangleSolid 5 5 & Color (makeColor 1 1 1 1) & Translate (-203 + 10 * i) 55
  in Pictures (map f (zip [0..] worlds) ++ map whiteBoxTop [0..50] ++ map whiteBoxBot [0..50])

worldAfterAnim :: obj -> Float -> Animation obj Identity a -> obj
worldAfterAnim obj totalTime anim =
  case runIdentity (runAnimation anim obj totalTime) of
    (newObj, _, _) -> newObj

completeIconCheckFig :: [Application]
completeIconCheckFig = let
  k = 3
  in fetchInbetweens (0.3 / fromIntegral k) k initialApplication (embed (mainWindow . todoItems . atIndex 0 . completeIcon) completeIconCheck) []

onlyDoneFig :: [Application]
onlyDoneFig = let
  k = 3
  startAppState = worldAfterAnim initialApplication 0.3 (embed (mainWindow . todoItems . atIndex 0 . completeIcon) completeIconCheck)
  in fetchInbetweens (0.5 / fromIntegral k) k startAppState (onlyDone `parallel` selectBtn2Anim) []

menuIntroFig :: [Application]
menuIntroFig = let
  k = 3
  in fetchInbetweens (getDuration (duration menuIntro) / fromIntegral k) k initialApplication menuIntro []

line1OutroFig :: [Application]
line1OutroFig = let
  k = 3
  in fetchInbetweens (getDuration (duration line1Outro) / fromIntegral k) k initialApplication line1Outro []

line2IntroFig :: [Application]
line2IntroFig = let
  k = 3
  startAppState = worldAfterAnim initialApplication 0.25 line1Outro
  in fetchInbetweens (getDuration (duration line2Intro) / fromIntegral k) k startAppState line2Intro []

menuSlideInFig :: [Application]
menuSlideInFig = let
  k = 3
  in fetchInbetweens (getDuration (duration menuSlideIn) / fromIntegral k) k initialApplication menuSlideIn []

appFadeOutFig :: [Application]
appFadeOutFig = let
  k = 3
  in fetchInbetweens (getDuration (duration appFadeOut) / fromIntegral k) k initialApplication appFadeOut []

selectBtn2AnimFig :: [Application]
selectBtn2AnimFig = let
  k = 3
  in fetchInbetweens (getDuration (duration selectBtn2Anim) / fromIntegral k) k initialApplication selectBtn2Anim []

main :: IO ()
main = let
  bgColor = makeColor 0 0 0 1
  sw = 100
  sh = 100
  window = InWindow "animation-dsl" (sw, sh) (100, 100)
  in do
  putStrLn "Command:"
  x :: Int <- readLn
  case x of
    1 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/completeIconCheckFig.png" (drawnWorlds completeIconCheckFig)
    2 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/onlyDoneFig.png" (drawnWorlds onlyDoneFig)
    3 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/menuIntroFig.png" (drawnWorlds menuIntroFig)
    4 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/line1OutroFig.png" (drawnWorlds line1OutroFig)
    5 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/line2IntroFig.png" (drawnWorlds line2IntroFig)
    6 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/menuSlideInFig.png" (drawnWorlds menuSlideInFig)
    7 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/appFadeOutFig.png" (drawnWorlds appFadeOutFig)
    8 -> exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/selectBtn2AnimFig.png" (drawnWorlds selectBtn2AnimFig)
    99 -> play window bgColor 60 initialApplication draw handleInput update

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)
