{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding (seq)

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Identity

import Data.Functor.Const

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss hiding (color)
import Graphics.Gloss.Interface.Pure.Game hiding (color)

newtype Duration = For Float
  deriving Show
newtype Target = To Float
  deriving Show

class Basic obj f where
  basic :: Traversal' obj Float -> Duration -> Target -> f ()

seq :: (Applicative f) => f () -> f () -> f ()
seq f1 f2 = liftA2 (\_ _ -> ()) f1 f2

class Par f where
  liftP2 :: (a -> b -> c) -> f a -> f b -> f c

par :: (Par f) => f () -> f () -> f ()
par f1 f2 = liftP2 (\_ _ -> ()) f1 f2

newtype Animation obj m a = Animation {
  runAnimation ::
    obj -> -- previous state
    Float -> -- time delta
    m -- result is wrapped in m
      ( obj -- next state
      , Either
        (Animation obj m a) -- animation remainder
        a -- animation result
      , Maybe Float -- remaining time delta
      )
}

instance (Applicative m) => Basic obj (Animation obj m) where
  basic traversal (For duration) (To target) =
    Animation $ \obj t -> let
    -- construct new object state
    newObj = over traversal (updateValue t duration target) obj
    -- calculate remaining duration of this basic animation
    newDuration = duration - t
    -- create remainder animation / time delta
    (remainingAnim, remainingDelta) =
      if newDuration > 0
      then ( Left (basic traversal (For newDuration) (To target))
           , Nothing
           )
      else (Right (), Just (-newDuration))
    in pure (newObj, remainingAnim, remainingDelta)

updateValue ::
  Float -> -- time delta
  Float -> -- duration
  Float -> -- target value
  Float -> -- current value
  Float -- new value
updateValue t duration target current = let
  speed = (target - current) * t / duration
  newValue = current + speed
  in if target > current
    then min target newValue
    else max target newValue

instance (Monad m) => Functor (Animation obj m) where
  fmap = liftM

instance (Monad m) => Applicative (Animation obj m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (Animation obj m) where
  return a = Animation $ \obj t -> return (obj, Right a, Just t)
  (Animation f) >>= k = Animation $ \obj t -> do
    (newObj, animResult, mRemainingDelta) <- f obj t
    case (animResult, mRemainingDelta) of
      (Left anim, Nothing) ->
        return (newObj, Left (anim >>= k), Nothing)
      (Right a, Nothing) ->
        return (newObj, Left (k a), Nothing)
      (Left anim, Just remainingDelta) ->
        runAnimation (anim >>= k) newObj remainingDelta
      (Right a, Just remainingDelta) ->
        runAnimation (k a) newObj remainingDelta

instance (Monad m) => Par (Animation obj m) where
  liftP2 combine (Animation f1) (Animation f2) =
    Animation $ \obj t -> do
    (obj1, remAnim1, mRem1) <- f1 obj t
    (obj2, remAnim2, mRem2) <- f2 obj1 t
    let newRem = case (mRem1, mRem2) of
          (Nothing, _) -> Nothing
          (_, Nothing) -> Nothing
          (Just rem1, Just rem2) -> Just (min rem1 rem2)
    let newAnim = case (remAnim1, remAnim2) of
          (Right a, Right b) ->
            Right (combine a b)
          (Left aniA, Right b) ->
            Left (fmap (\a -> combine a b) aniA)
          (Right a, Left aniB) ->
            Left (fmap (\b -> combine a b) aniB)
          (Left aniA, Left aniB) ->
            Left (liftP2 combine aniA aniB)
    return (obj2, newAnim, newRem)

type RGB = (Float, Float, Float)

data Sprite = Sprite
  { _x :: Float
  , _y :: Float
  , _color :: RGB
  , _alpha :: Float
  , _pic :: Float -> Float -> Picture
  , _width :: Float
  , _height :: Float
  }

makeLenses ''Sprite

drawSprite :: Sprite -> Picture
drawSprite Sprite{ _x, _y, _color, _alpha, _pic, _width, _height } = let
  (r, g, b) = _color
  sw = 100
  sh = 100
  in _pic _width _height &
  Color (makeColor r g b _alpha) &
  Translate (_x + _width / 2 - sw / 2) (-_y - _height / 2 + sh / 2)

data MenuWorld = MenuWorld
  { _menu :: Sprite
  }

makeLenses ''MenuWorld

data NavBarWorld = NavBarWorld
  { _navBarBg :: Sprite
  , _navBarBtn1 :: Sprite
  , _navBarBtn2 :: Sprite
  , _navBarBtn3 :: Sprite
  , _navBarLine1 :: Sprite
  , _navBarLine2 :: Sprite
  , _nbAnimations :: [Animation NavBarWorld Identity ()]
  , _barSelected :: Int
  }

makeLenses ''NavBarWorld

drawNavBarWorld :: NavBarWorld -> Picture
drawNavBarWorld w = Pictures
  [ drawSprite (w ^. navBarBg)
  , drawSprite (w ^. navBarBtn1)
  , drawSprite (w ^. navBarBtn2)
  , drawSprite (w ^. navBarBtn3)
  , drawSprite (w ^. navBarLine1)
  , drawSprite (w ^. navBarLine2)
  ]

bar1Anim :: (Applicative f, Basic NavBarWorld f) => f ()
bar1Anim =
  basic (navBarLine1 . height) (For 0.25) (To 4) *>
  basic (navBarLine2 . height) (For 0.25) (To 0)

bar2Anim :: (Applicative f, Basic NavBarWorld f) => f ()
bar2Anim =
  basic (navBarLine2 . height) (For 0.25) (To 4) *>
  basic (navBarLine1 . height) (For 0.25) (To 0)

handleInputNb :: Event -> NavBarWorld -> NavBarWorld
handleInputNb (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) w =
  if w ^. barSelected == 1
  then
    w & nbAnimations .~ [bar2Anim]
      & barSelected .~ 2
  else
    w & nbAnimations .~ [bar1Anim]
      & barSelected .~ 1
handleInputNb _ w = w

updateNb :: Float -> NavBarWorld -> NavBarWorld
updateNb t w = let
  (newWorld, newAnimations) = runAnimations t w (w ^. nbAnimations)
  in newWorld & nbAnimations .~ newAnimations

runAnimations :: Float -> obj -> [Animation obj Identity ()] -> (obj, [Animation obj Identity ()])
runAnimations t w [] = (w, [])
runAnimations t w (anim:r) = let
  (newWorld, res, _) = runIdentity (runAnimation anim w t)
  in case res of
    Left newAnim -> fmap (\l -> newAnim : l) (runAnimations t newWorld r)
    Right _ -> runAnimations t newWorld r

initialNavBarWorld :: NavBarWorld
initialNavBarWorld = NavBarWorld
  { _navBarBg =
      Sprite
      { _x = 0
      , _y = 0
      , _width = 100
      , _height = 28
      , _color = (0, 1, 0)
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarBtn1 =
      Sprite
      { _x = 7.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = (1, 0, 0)
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarBtn2 =
      Sprite
      { _x = 37.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = (1, 0, 0)
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarBtn3 =
      Sprite
      { _x = 67.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = (1, 0, 0)
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarLine1 =
      Sprite
      { _x = 7.5
      , _y = 20
      , _width = 25
      , _height = 4
      , _color = (0, 0, 1)
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarLine2 =
      Sprite
      { _x = 37.5
      , _y = 20
      , _width = 25
      , _height = 0
      , _color = (0, 0, 1)
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _nbAnimations = []
  , _barSelected = 1
  }

main :: IO ()
main = let
  bgColor = makeColor (196 / 256) (200 / 256) (194 / 256) 1
  window = InWindow "animation-dsl" (100, 100) (100, 100)
  in
  play window bgColor 60 initialNavBarWorld drawNavBarWorld handleInputNb updateNb
