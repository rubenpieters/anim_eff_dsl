{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RebindableSyntax #-}

module App.State where

import Prelude

import Util.Types
import Util.Draw
import Util.Sprite
import App.Navbar
import App.MainWindow
import App.Menu
import App.CompleteIcon
import App.TodoItem
import DSL.Functor
import DSL.Animation

import Control.Monad.Identity
import Lens.Micro hiding (set)
import Lens.Micro.TH
import Graphics.Gloss hiding (circle, scale, color)

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


menuOutro :: (Basic Application f, Parallel f) => f ()
menuOutro = basic (menu . width) (For 0.5) (To 0)
  `parallel` basic (obscuringBox . alpha) (For 0.5) (To 0)

lineXOutro :: (Basic Application f) => Int -> f ()
lineXOutro x = basic (navbar . underline x . width) (For 0.25) (To 0)

lineXIntro :: (Basic Application f) => Int -> f ()
lineXIntro x = basic (navbar . underline x . width) (For 0.25) (To 28)

selectBtnXAnim :: (Basic Application f, Applicative f) => Int -> Int -> f ()
selectBtnXAnim prev new = lineXOutro prev `sequential` lineXIntro new

----- BASIC

line1Outro :: (Basic Application f) => f ()
line1Outro = basic (navbar . underline1 . width) (For 0.25) (To 0)

line2Intro :: (Basic Application f) => f ()
line2Intro = basic (navbar . underline2 . width) (For 0.25) (To 28)

menuSlideIn :: (Basic Application f) => f ()
menuSlideIn = basic (menu . width) (For 0.5) (To 75)

appFadeOut :: (Basic Application f) => f ()
appFadeOut = basic (obscuringBox . alpha) (For 0.5) (To 0.5)

-- SEQ

selectBtn2Anim :: (Basic Application f, Applicative f) => f ()
selectBtn2Anim = line1Outro `sequential` line2Intro

-- PAR

menuIntro :: (Basic Application f, Parallel f) => f ()
menuIntro = menuSlideIn `parallel` appFadeOut

-- COMPLETE ICON ANIM

{-
completeIconCheck :: (Basic Application f, Monad f, Parallel f, Set Application f) => f ()
completeIconCheck = do
  set (completeIcon . checked) True
  basic (completeIcon . checkmark . scale) (For 0.05) (To 0)
  set (completeIcon . checkmark . color) completeGreen
  parallel
    (basic (completeIcon . circle . extra) (For 0.2) (To 360))
    (basic (completeIcon . checkmark . scale) (For 0.2) (To 1.2))
  basic (completeIcon . checkmark . scale) (For 0.05) (To 1)

completeIconUncheck :: (Basic Application f, Monad f, Parallel f, Set Application f) => f ()
completeIconUncheck = do
  set (completeIcon . checked) False
  set (completeIcon . checkmark . color) completeGray
  parallel
    (do
       basic (completeIcon . checkmark . scale) (For 0.2) (To 0.8)
       basic (completeIcon . checkmark . scale) (For 0.05) (To 1)
    )
    (basic (completeIcon . circle . extra) (For 0.05) (To 1))
-}

-- SHOW ALL / FILTER

showAll :: (Basic Application f, Parallel f) => f ()
showAll =
  (basic (mainWindow . todoItems . traverse . todoItemBg . alpha) (For 0.5) (To 1))
    `parallel`
    (basic (mainWindow . todoItems . traverse . completeIcon . checkmark . alpha) (For 0.5) (To 1))
    `parallel`
    (basic (mainWindow . todoItems . traverse . completeIcon . circle . alpha) (For 0.5) (To 1))

hideNotDone :: (Basic Application f, Applicative f, Parallel f) => f ()
hideNotDone =
  (basic (mainWindow . todoItems . traverse . filtered (\x -> not $ x ^. completeIcon . checked) . todoItemBg . alpha) (For 0.5) (To 0))
    `parallel`
    (basic (mainWindow . todoItems . traverse . filtered (\x -> not $ x ^. completeIcon . checked) . completeIcon . checkmark . alpha) (For 0.5) (To 0))
    `parallel`
    (basic (mainWindow . todoItems . traverse . filtered (\x -> not $ x ^. completeIcon . checked) . completeIcon . circle . alpha) (For 0.5) (To 0))

hideDone :: (Basic Application f, Applicative f, Parallel f) => f ()
hideDone =
  (basic (mainWindow . todoItems . traverse . filtered (\x -> x ^. completeIcon . checked) . todoItemBg . alpha) (For 0.5) (To 0))
    `parallel`
    (basic (mainWindow . todoItems . traverse . filtered (\x -> x ^. completeIcon . checked) . completeIcon . checkmark . alpha) (For 0.5) (To 0))
    `parallel`
    (basic (mainWindow . todoItems . traverse . filtered (\x -> x ^. completeIcon . checked) . completeIcon . circle . alpha) (For 0.5) (To 0))

onlyDoneNaive :: (Basic Application f, Applicative f, Parallel f) => f ()
onlyDoneNaive = do showAll ; hideNotDone ; return ()

doneItemsGt0 :: (Get Application f, Functor f) => f Bool
doneItemsGt0 = do
  doneItems <- get (mainWindow . todoItems)
  return (length (filter (\x -> x ^. completeIcon . checked && x ^. todoItemBg . alpha < 1) doneItems) > 0)

onlyDoneMonad :: (Basic Application f, Get Application f, Set Application f, Monad f, Parallel f) => f ()
onlyDoneMonad = do
  cond <- doneItemsGt0
  case cond of
    True -> do showAll ; hideNotDone
    False -> hideNotDone

onlyDone :: (Basic Application f, Get Application f, Set Application f, Applicative f, Parallel f, IfThenElse f) => f ()
onlyDone = if doneItemsGt0
  then do showAll ; hideNotDone ; return ()
  else hideNotDone

notDoneItemsGt0 :: (Get Application f, Functor f) => f Bool
notDoneItemsGt0 = do
  doneItems <- get (mainWindow . todoItems)
  return (length (filter (\x -> x ^. completeIcon . checked) doneItems) > 0)

onlyNotDone :: (Basic Application f, Get Application f, Set Application f, Applicative f, Parallel f, IfThenElse f) => f ()
onlyNotDone = if notDoneItemsGt0
  then showAll *> hideDone
  else hideDone


