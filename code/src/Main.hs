{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (seq)

import Numeric

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Identity

import Data.Functor.Const

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss hiding (color)
import Graphics.Gloss.Interface.Pure.Game hiding (color)
import Graphics.Gloss.Export.PNG

newtype Duration = For { getDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup Duration where
  (<>) = mappend

instance Monoid Duration where
  mempty = For 0
  mappend (For a) (For b) = For (a + b)

newtype Target = To { getTarget :: Float }
  deriving (Ord, Eq, Show)

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

instance Basic obj (Const Duration) where
  basic _ duration _ = Const duration

instance Par (Const Duration) where
  liftP2 _ (Const x1) (Const x2) = Const (max x1 x2)

duration :: Const Duration a -> Duration
duration = getConst

runAnimations :: Float -> obj -> [Animation obj Identity ()] -> (obj, [Animation obj Identity ()])
runAnimations t w [] = (w, [])
runAnimations t w (anim:r) = let
  (newWorld, res, _) = runIdentity (runAnimation anim w t)
  in case res of
    Left newAnim -> fmap (\l -> newAnim : l) (runAnimations t newWorld r)
    Right _ -> runAnimations t newWorld r

type RGB = (Float, Float, Float)

hexToRgb :: String -> String -> String -> RGB
hexToRgb v1 v2 v3 = let
  r = fst (head (readHex v1))
  g = fst (head (readHex v2))
  b = fst (head (readHex v3))
  in (r / 256, g / 256, b / 256)

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
  { _menuBgElements :: [Sprite]
  , _obscuringBox :: Sprite
  , _menu :: Sprite
  , _mnAnimations :: [Animation MenuWorld Identity ()]
  , _menuOpen :: Bool
  }

makeLenses ''MenuWorld

drawMenuWorld :: MenuWorld -> Picture
drawMenuWorld w = Pictures $
  map drawSprite (w ^. menuBgElements) ++
  [ drawSprite (w ^. obscuringBox)
  , drawSprite (w ^. menu)
  ]

menuSlideIn :: (Basic MenuWorld f) => f ()
menuSlideIn = basic (menu . width) (For 0.5) (To 75)

appFadeOut :: (Basic MenuWorld f) => f ()
appFadeOut = basic (obscuringBox . alpha) (For 0.5) (To 0.6)

menuAnimation :: (Par f, Basic MenuWorld f) => f ()
menuAnimation = menuSlideIn `par` appFadeOut

menuAnimationRev :: (Par f, Basic MenuWorld f) => f ()
menuAnimationRev = par
  (basic (menu . width) (For 0.5) (To 0))
  (basic (obscuringBox . alpha) (For 0.5) (To 0))

handleInputMn :: Event -> MenuWorld -> MenuWorld
handleInputMn (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) w =
  if w ^. menuOpen
  then
    w & mnAnimations .~ [menuAnimationRev]
      & menuOpen .~ False
  else
    w & mnAnimations .~ [menuAnimation]
      & menuOpen .~ True
handleInputMn _ w = w

menuPic :: Float -> Float -> Picture
menuPic width height = let
  (r, g, b) = hexToRgb "58" "74" "8a"
  itemColor = Color (makeColor r g b 1)
  in Pictures
  [ rectangleSolid width height
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (-0.30 * height) & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (-0.15 * height) & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) 0 & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (0.15 * height) & itemColor
  , rectangleSolid (0.6 * width) (0.1 * height) & Translate (0.05 * width) (0.30 * height) & itemColor
  ]

updateMn :: Float -> MenuWorld -> MenuWorld
updateMn t w = let
  (newWorld, newAnimations) = runAnimations t w (w ^. mnAnimations)
  in newWorld & mnAnimations .~ newAnimations

initialMenuWorld :: MenuWorld
initialMenuWorld = MenuWorld
  { _menuBgElements =
      [ Sprite
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 100
        , _color = hexToRgb "c4" "c8" "c2"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 28
        , _color = hexToRgb "55" "7a" "95"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 35
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 45
        , _width = 60
        , _height = 30
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 83
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 93
        , _width = 60
        , _height = 7
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 7.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 37.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 67.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      ]
  , _menu =
      Sprite
      { _x = 0
      , _y = 0
      , _width = 0
      , _height = 100
      , _color = hexToRgb "31" "55" "70"
      , _alpha = 1
      , _pic = menuPic
      }
  , _obscuringBox =
      Sprite
      { _x = 0
      , _y = 0
      , _width = 100
      , _height = 100
      , _color = (0, 0, 0)
      , _alpha = 0
      , _pic = rectangleSolid
      }
  , _mnAnimations = []
  , _menuOpen = False
  }

data NavBarWorld = NavBarWorld
  { _navBarBgElements :: [Sprite]
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
drawNavBarWorld w = Pictures $
  map drawSprite (w ^. navBarBgElements) ++
  [ drawSprite (w ^. navBarBtn1)
  , drawSprite (w ^. navBarBtn2)
  , drawSprite (w ^. navBarBtn3)
  , drawSprite (w ^. navBarLine1)
  , drawSprite (w ^. navBarLine2)
  ]

bar1Animation :: (Applicative f, Basic NavBarWorld f) => f ()
bar1Animation =
  basic (navBarLine2 . height) (For 0.25) (To 0) *>
  basic (navBarLine1 . height) (For 0.25) (To 4)

bar2Animation :: (Applicative f, Basic NavBarWorld f) => f ()
bar2Animation = decreaseBar1 `seq` increaseBar2

decreaseBar1 :: (Basic NavBarWorld f) => f ()
decreaseBar1 = basic (navBarLine1 . height) (For 0.25) (To 0)

increaseBar2 :: (Basic NavBarWorld f) => f ()
increaseBar2 = basic (navBarLine2 . height) (For 0.25) (To 4)

handleInputNb :: Event -> NavBarWorld -> NavBarWorld
handleInputNb (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) w =
  if w ^. barSelected == 1
  then
    w & nbAnimations .~ [bar2Animation]
      & barSelected .~ 2
  else
    w & nbAnimations .~ [bar1Animation]
      & barSelected .~ 1
handleInputNb _ w = w

updateNb :: Float -> NavBarWorld -> NavBarWorld
updateNb t w = let
  (newWorld, newAnimations) = runAnimations t w (w ^. nbAnimations)
  in newWorld & nbAnimations .~ newAnimations

initialNavBarWorld :: NavBarWorld
initialNavBarWorld = NavBarWorld
  { _navBarBgElements =
      [ Sprite
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 100
        , _color = hexToRgb "c4" "c8" "c2"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 28
        , _color = hexToRgb "55" "7a" "95"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 35
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 45
        , _width = 60
        , _height = 30
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 83
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      , Sprite
        { _x = 20
        , _y = 93
        , _width = 60
        , _height = 7
        , _color = hexToRgb "b1" "a2" "96"
        , _alpha = 1
        , _pic = rectangleSolid
        }
      ]
  , _navBarBtn1 =
      Sprite
      { _x = 7.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = hexToRgb "73" "95" "ae"
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarBtn2 =
      Sprite
      { _x = 37.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = hexToRgb "73" "95" "ae"
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarBtn3 =
      Sprite
      { _x = 67.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = hexToRgb "73" "95" "ae"
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarLine1 =
      Sprite
      { _x = 7.5
      , _y = 20
      , _width = 25
      , _height = 4
      , _color = hexToRgb "5d" "5c" "61"
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _navBarLine2 =
      Sprite
      { _x = 37.5
      , _y = 20
      , _width = 25
      , _height = 0
      , _color = hexToRgb "5d" "5c" "61"
      , _alpha = 1
      , _pic = rectangleSolid
      }
  , _nbAnimations = []
  , _barSelected = 1
  }

fetchInbetweens :: Float -> Int -> obj -> Animation obj Identity a -> [obj] -> [obj]
fetchInbetweens _ 0 obj _ acc = acc ++ [obj]
fetchInbetweens delta k obj (Animation f) acc = let
  (obj', eFa, _) = runIdentity (f obj delta)
  in case eFa of
     Left anim -> fetchInbetweens delta (k - 1) obj' anim (acc ++ [obj])
     Right _ -> acc ++ [obj, obj']

usecase1fig :: [MenuWorld]
usecase1fig = let
  k = 3
  anim :: (Par f, Basic MenuWorld f) => f ()
  anim = menuAnimation
  in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k initialMenuWorld anim []

usecase1basic1 :: [MenuWorld]
usecase1basic1 = let
  k = 3
  anim :: (Basic MenuWorld f) => f ()
  anim = menuSlideIn
  in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k initialMenuWorld anim []

usecase1basic2 :: [MenuWorld]
usecase1basic2 = let
  k = 3
  anim :: (Basic MenuWorld f) => f ()
  anim = appFadeOut
  in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k initialMenuWorld anim []

usecase2fig :: [NavBarWorld]
usecase2fig = let
  k = 3
  anim :: (Applicative f, Basic NavBarWorld f) => f ()
  anim = bar2Animation
  in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k initialNavBarWorld anim []

usecase2basic1 :: [NavBarWorld]
usecase2basic1 = let
  k = 3
  anim :: (Basic NavBarWorld f) => f ()
  anim = decreaseBar1
  in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k initialNavBarWorld anim []

usecase2basic2 :: [NavBarWorld]
usecase2basic2 = let
  k = 3
  anim :: (Applicative f, Basic NavBarWorld f) => f ()
  anim = increaseBar2
  world = initialNavBarWorld & navBarLine1 . height .~ 0
  in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k world anim []

drawnWorlds :: (obj -> Picture) -> [obj] -> Picture
drawnWorlds drawFun worlds = let
  k = length worlds
  f (i, w) = drawFun w & Translate (50 - 206 + 104 * i) 0
  whiteBoxTop i = rectangleSolid 5 5 & Color (makeColor 1 1 1 1) & Translate (-203 + 10 * i) (-55)
  whiteBoxBot i = rectangleSolid 5 5 & Color (makeColor 1 1 1 1) & Translate (-203 + 10 * i) 55
  in Pictures (map f (zip [0..] worlds) ++ map whiteBoxTop [0..50] ++ map whiteBoxBot [0..50])

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
    1 -> play window bgColor 60 initialNavBarWorld drawNavBarWorld handleInputNb updateNb
    2 -> play window bgColor 60 initialMenuWorld drawMenuWorld handleInputMn updateMn
    9 -> do
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase1fig.png" (drawnWorlds drawMenuWorld usecase1fig)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase1basic1.png" (drawnWorlds drawMenuWorld usecase1basic1)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase1basic2.png" (drawnWorlds drawMenuWorld usecase1basic2)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase2fig.png" (drawnWorlds drawNavBarWorld usecase2fig)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase2basic1.png" (drawnWorlds drawNavBarWorld usecase2basic1)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase2basic2.png" (drawnWorlds drawNavBarWorld usecase2basic2)
    _ -> error "unknown"
