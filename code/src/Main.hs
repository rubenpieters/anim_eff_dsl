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
import Control.Monad.IO.Class

import Data.Functor.Const
import Data.List (find, findIndex, nub)

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss hiding (color, scale)
import Graphics.Gloss.Interface.Pure.Game hiding (color, scale)
import Graphics.Gloss.Interface.IO.Game hiding (color, scale)
import Graphics.Gloss.Export.PNG

import System.Random

newtype Duration = For { getDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup Duration where
  (<>) = mappend

instance Monoid Duration where
  mempty = For 0
  mappend (For a) (For b) = For (a + b)

newtype MaxDuration = MaxDur { getMaxDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup MaxDuration where
  (<>) = mappend

instance Monoid MaxDuration where
  mempty = MaxDur 0
  mappend (MaxDur a) (MaxDur b) = MaxDur (a + b)

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

instance Basic obj (Const MaxDuration) where
  basic _ (For dur) _ = Const (MaxDur dur)

instance Par (Const MaxDuration) where
  liftP2 _ (Const x1) (Const x2) = Const (max x1 x2)

duration :: Const Duration a -> Duration
duration = getConst

maxDuration :: Const MaxDuration a -> MaxDuration
maxDuration = getConst

runAnimations :: Float -> obj -> [Animation obj Identity ()] -> (obj, [Animation obj Identity ()])
runAnimations t w [] = (w, [])
runAnimations t w (anim:r) = let
  (newWorld, res, _) = runIdentity (runAnimation anim w t)
  in case res of
    Left newAnim -> fmap (\l -> newAnim : l) (runAnimations t newWorld r)
    Right _ -> runAnimations t newWorld r

runAnimationsIO :: Float -> obj -> [Animation obj IO ()] -> IO (obj, [Animation obj IO ()])
runAnimationsIO t w [] = return (w, [])
runAnimationsIO t w (anim:r) = do
  (newWorld, res, _) <- runAnimation anim w t
  case res of
    Left newAnim -> do
      res <- runAnimationsIO t newWorld r
      return (fmap (\l -> newAnim : l) res)
    Right _ -> runAnimationsIO t newWorld r

class Rng f where
  rng :: (Random a) => (a, a) -> f a

instance (MonadIO f) => Rng (Animation obj f) where
  rng range = Animation $ \obj t -> do
    v <- liftIO (randomRIO range)
    return (obj, Right v, Just t)

instance Rng (Const Duration) where
  rng _ = (Const (For 0))

instance Rng (Const MaxDuration) where
  rng _ = (Const (MaxDur 0))

class Particle obj f where
  create :: (obj -> (obj, Int)) -> f Int
  delete :: (Int -> obj -> obj) -> Int -> f ()

class IfThenElse f where
  ifThenElse :: f Bool -> f a -> f a -> f a

instance (Monad f) => IfThenElse (Animation obj f) where
  ifThenElse fb fl fr = do
    bool <- fb
    if bool
      then fl
      else fr

instance IfThenElse (Const MaxDuration) where
  ifThenElse (Const (MaxDur db)) (Const (MaxDur dl)) (Const (MaxDur dr)) =
    Const (MaxDur (db + max dl dr))

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
  , _rotation :: Float
  , _scale :: Float
  , _spriteId :: Int
  }

makeLenses ''Sprite

withId :: Int -> Lens' [Sprite] Sprite
withId i = let
  get l = case find (\x -> x ^. spriteId == i) l of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set l x = case findIndex (\x -> x ^. spriteId == i) l of
    Just ix -> take ix l ++ x : drop (ix+1) l
    Nothing -> error ("no particle with index " ++ show i)
  in lens get set

spriteDef :: Sprite
spriteDef = Sprite
  { _alpha = 1
  , _rotation = 0
  , _scale = 1
  , _spriteId = (-1)
  }

drawSprite :: Sprite -> Picture
drawSprite Sprite{ _x, _y, _color, _alpha, _pic, _width, _height, _rotation, _scale } = let
  (r, g, b) = _color
  sw = 100
  sh = 100
  in _pic _width _height &
  Color (makeColor r g b _alpha) &
  Rotate _rotation &
  Scale _scale _scale &
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
      [ spriteDef
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 100
        , _color = hexToRgb "c4" "c8" "c2"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 28
        , _color = hexToRgb "55" "7a" "95"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 35
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 45
        , _width = 60
        , _height = 30
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 83
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 93
        , _width = 60
        , _height = 7
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 7.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 37.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 67.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _pic = rectangleSolid
        }
      ]
  , _menu =
      spriteDef
      { _x = 0
      , _y = 0
      , _width = 0
      , _height = 100
      , _color = hexToRgb "31" "55" "70"
      , _pic = menuPic
      }
  , _obscuringBox =
      spriteDef
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
      [ spriteDef
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 100
        , _color = hexToRgb "c4" "c8" "c2"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 28
        , _color = hexToRgb "55" "7a" "95"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 35
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 45
        , _width = 60
        , _height = 30
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 83
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 93
        , _width = 60
        , _height = 7
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      ]
  , _navBarBtn1 =
      spriteDef
      { _x = 7.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = hexToRgb "73" "95" "ae"
      , _pic = rectangleSolid
      }
  , _navBarBtn2 =
      spriteDef
      { _x = 37.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = hexToRgb "73" "95" "ae"
      , _pic = rectangleSolid
      }
  , _navBarBtn3 =
      spriteDef
      { _x = 67.5
      , _y = 7
      , _width = 25
      , _height = 10
      , _color = hexToRgb "73" "95" "ae"
      , _pic = rectangleSolid
      }
  , _navBarLine1 =
      spriteDef
      { _x = 7.5
      , _y = 20
      , _width = 25
      , _height = 4
      , _color = hexToRgb "5d" "5c" "61"
      , _pic = rectangleSolid
      }
  , _navBarLine2 =
      spriteDef
      { _x = 37.5
      , _y = 20
      , _width = 25
      , _height = 0
      , _color = hexToRgb "5d" "5c" "61"
      , _pic = rectangleSolid
      }
  , _nbAnimations = []
  , _barSelected = 1
  }

data ColorWorld = ColorWorld
  { _cwParticles :: [Sprite]
  , _nextSpriteId :: Int
  , _cwAnimations :: [Animation ColorWorld IO ()]
  }

makeLenses ''ColorWorld

initialColorWorld :: ColorWorld
initialColorWorld = ColorWorld
  { _cwParticles = []
  , _nextSpriteId = 0
  , _cwAnimations = []
  }

drawColorWorld :: ColorWorld -> IO Picture
drawColorWorld w = return $ Pictures (map drawSprite (w ^. cwParticles))

handleInputCw :: Event -> ColorWorld -> IO ColorWorld
handleInputCw (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) w =
  return $ w & cwAnimations %~ \l -> colorAnim (mouseX + 50) (50 - mouseY) : l
handleInputCw _ w = return w

updateCw :: Float -> ColorWorld -> IO ColorWorld
updateCw t w = do
  (newWorld, newAnimations) <- runAnimationsIO t w (w ^. cwAnimations)
  return $ newWorld & cwAnimations .~ newAnimations

intToPic :: Int -> (Float -> Float -> Picture)
intToPic 1 = rectangleSolid
intToPic 2 = \w h -> circleSolid (w / 2)
intToPic 3 = \w h -> Polygon [(0, h/2), (w/2, -h/2), (-w/2, -h/2)]

createParticle :: Int -> Float -> Float -> Float -> Float -> RGB -> ColorWorld -> (ColorWorld, Int)
createParticle ptype x y rot sc col w@(ColorWorld {_cwParticles, _nextSpriteId}) = let
  newIndex = _nextSpriteId
  particle = spriteDef
      { _x = x
      , _y = y
      , _width = 11
      , _height = 11
      , _color = col
      , _pic = intToPic ptype
      , _spriteId = newIndex
      , _rotation = rot
      , _scale = sc
      }
  newWorld = w { _cwParticles = particle : _cwParticles, _nextSpriteId = _nextSpriteId + 1 }
  in (newWorld, newIndex)

deleteParticle :: Int -> ColorWorld -> ColorWorld
deleteParticle id w@(ColorWorld {_cwParticles}) = let
  newWorld = w { _cwParticles = filter (\x -> x ^. spriteId /= id) _cwParticles }
  in newWorld

instance (Applicative f) => Particle obj (Animation obj f) where
  create f = Animation $ \obj t ->
    let (newObj, id) = f obj
    in pure (newObj, Right id, Just t)
  delete f id = Animation $ \obj t ->
    let newObj = f id obj
    in pure (newObj, Right (), Just t)

colorAnim :: (Basic ColorWorld f, Par f, Monad f, Particle ColorWorld f, Rng f) => Float -> Float -> f ()
colorAnim x y = do
  amt :: Int <- rng (7, 11)
  colorAnim1 x y amt

colorAnim1 :: (Basic ColorWorld f, Par f, Monad f, Particle ColorWorld f, Rng f) => Float -> Float -> Int -> f ()
colorAnim1 x y n | n <= 0 = pure ()
colorAnim1 x y n =
  do
    ptype :: Int <- rng (1, 3)
    colorR :: Int <- rng (0, 256)
    colorG :: Int <- rng (0, 256)
    colorB :: Int <- rng (0, 256)
    offsetDeg :: Float <- rng (20, 160)
    let offset = offsetDeg / 180 * pi
    dist :: Float <- rng (15, 45)
    rot :: Float <- rng (0, 360)
    sc :: Float <- rng (0.6, 1.3)
    id <- create (createParticle ptype x y rot sc (fromIntegral colorR / 256, fromIntegral colorG / 256, fromIntegral colorB / 256))
    colorAnim2 x y offset dist id
    delete deleteParticle id
  `par` colorAnim1 x y (n - 1)

colorAnim2 :: (Basic ColorWorld f, Par f) => Float -> Float -> Float -> Float -> Int -> f ()
colorAnim2 mx my offset dist id =
        basic (cwParticles . withId id . x) (For 1) (To (mx + dist * cos offset))
  `par` basic (cwParticles . withId id . y) (For 1) (To (my - dist * sin offset))
  `par` basic (cwParticles . withId id . alpha) (For 1) (To 0.3)

data MouseWorld = MouseWorld
  { _msBgElements :: [Sprite]
  , _msParticles :: [Sprite]
  , _msNextSpriteId :: Int
  , _msAnimations :: [Animation MouseWorld Identity ()]
  }

makeLenses ''MouseWorld

initialMouseWorld :: MouseWorld
initialMouseWorld = MouseWorld
  { _msBgElements =
      [ spriteDef
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 100
        , _color = hexToRgb "c4" "c8" "c2"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 0
        , _y = 0
        , _width = 100
        , _height = 28
        , _color = hexToRgb "55" "7a" "95"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 35
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 45
        , _width = 60
        , _height = 30
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 83
        , _width = 50
        , _height = 8
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 20
        , _y = 93
        , _width = 60
        , _height = 7
        , _color = hexToRgb "b1" "a2" "96"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 7.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 37.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _pic = rectangleSolid
        }
      , spriteDef
        { _x = 67.5
        , _y = 7
        , _width = 25
        , _height = 10
        , _color = hexToRgb "73" "95" "ae"
        , _pic = rectangleSolid
        }
      ]
  , _msParticles = []
  , _msNextSpriteId = 0
  , _msAnimations = []
  }

drawMouseWorld :: MouseWorld -> Picture
drawMouseWorld w = Pictures $
  map drawSprite (w ^. msBgElements) ++
  map drawSprite (w ^. msParticles)

handleInputMs :: Event -> MouseWorld -> MouseWorld
handleInputMs (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) w =
  w & msAnimations %~ \l -> mouseBoxAnimation (mouseX + 50) (50 - mouseY) : l
handleInputMs _ w = w

updateMs :: Float -> MouseWorld -> MouseWorld
updateMs t w = let
  (newWorld, newAnimations) = runAnimations t w (w ^. msAnimations)
  in newWorld & msAnimations .~ newAnimations

createBoxParticle :: Float -> Float -> MouseWorld -> (MouseWorld, Int)
createBoxParticle x y w@(MouseWorld {_msParticles, _msNextSpriteId}) = let
  newIndex = _msNextSpriteId
  particle = spriteDef
      { _x = x
      , _y = y
      , _width = 5
      , _height = 5
      , _color = hexToRgb "96" "00" "18"
      , _pic = rectangleSolid
      , _spriteId = newIndex
      , _rotation = 0
      , _scale = 1
      }
  newWorld = w { _msParticles = particle : _msParticles, _msNextSpriteId = _msNextSpriteId + 1 }
  in (newWorld, newIndex)

deleteBoxParticle :: Int -> MouseWorld -> MouseWorld
deleteBoxParticle id w@(MouseWorld {_msParticles}) = let
  newWorld = w { _msParticles = filter (\x -> x ^. spriteId /= id) _msParticles }
  in newWorld

mouseBoxAnimation :: (Particle MouseWorld f, Basic MouseWorld f, Par f, Monad f) => Float -> Float -> f ()
mouseBoxAnimation mouseX mouseY = do
  i <- create (createBoxParticle mouseX mouseY)
  par (basic (msParticles . withId i . scale) (For 0.5) (To 4))
      (basic (msParticles . withId i . alpha) (For 0.5) (To 0))
  delete deleteBoxParticle i

rareAnimation :: (IfThenElse f, Basic MouseWorld f, Rng f, Functor f) => f ()
rareAnimation =
  ifThenElse
    (fmap (\(x :: Int) -> x == 10) (rng (1, 10)))
    (basic (msParticles . withId 0 . scale) (For 1) (To 1))
    (basic (msParticles . withId 0 . scale) (For 5) (To 1))

rareAnimMaxDuration :: MaxDuration
rareAnimMaxDuration = maxDuration rareAnimation

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

usecase3fig :: [MouseWorld]
usecase3fig = let
  k = 3
  anim :: (Particle MouseWorld f, Basic MouseWorld f, Par f, Monad f) => f ()
  anim = mouseBoxAnimation 50 50
  --in fetchInbetweens (getDuration (duration anim) / fromIntegral k) k initialNavBarWorld anim []
  in fetchInbetweens 0.125 k initialMouseWorld anim []

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
    3 -> playIO window bgColor 60 initialColorWorld drawColorWorld handleInputCw updateCw
    4 -> play window bgColor 60 initialMouseWorld drawMouseWorld handleInputMs updateMs
    9 -> do
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase1fig.png" (drawnWorlds drawMenuWorld usecase1fig)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase1basic1.png" (drawnWorlds drawMenuWorld usecase1basic1)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase1basic2.png" (drawnWorlds drawMenuWorld usecase1basic2)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase2fig.png" (drawnWorlds drawNavBarWorld usecase2fig)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase2basic1.png" (drawnWorlds drawNavBarWorld usecase2basic1)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase2basic2.png" (drawnWorlds drawNavBarWorld usecase2basic2)
      exportPictureToPNG (sw * 4 + 4 * 3, sh + 20) bgColor "../paper/pictures/usecase3fig.png" (drawnWorlds drawMouseWorld usecase3fig)
    _ -> error "unknown"

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)
