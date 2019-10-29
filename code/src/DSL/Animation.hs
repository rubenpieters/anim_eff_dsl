{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DSL.Animation where

import DSL.Functor
import Util.Types

import Control.Arrow (left)
import Control.Monad (ap, liftM)
import Control.Monad.Identity
import Lens.Micro

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

embed :: (Functor m) => Lens' sup sub -> Animation sub m a -> Animation sup m a
embed lens (Animation anim) = Animation $ \sup t -> let
  f (sub, remainingAnim, remainingDelta) =
    (sup & lens .~ sub, left (embed lens) remainingAnim, remainingDelta)
  in fmap f (anim (sup ^. lens) t)

instance (Applicative m) => Basic obj (Animation obj m) where
  basic traversal (For duration) (To target) =
    Animation $ \obj t -> let
    -- construct new object state
    newObj = obj & traversal %~ updateValue t duration target
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

instance (Monad m) => Parallel (Animation obj m) where
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

instance (Applicative m) => Set obj (Animation obj m) where
  set lens a = Animation $ \obj t -> let
    newObj = obj & lens .~ a
    in pure (newObj, Right (), Just t)

applyAnimation :: (Monad m) => obj -> Float -> Animation obj m () -> m (obj, Animation obj m ())
applyAnimation w t anim = let
  f (obj, eAnim, _) = case eAnim of
    Left restAnim -> (obj, restAnim)
    Right _ -> (obj, pure ())
  in fmap f (runAnimation anim w t)

applyIdAnimation :: obj -> Float -> Animation obj Identity () -> (obj, Animation obj Identity ())
applyIdAnimation w t anim = runIdentity (applyAnimation w t anim)

fetchInbetweens :: Float -> Int -> obj -> Animation obj Identity a -> [obj] -> [obj]
fetchInbetweens _ 0 obj _ acc = acc ++ [obj]
fetchInbetweens delta k obj (Animation f) acc = let
  (obj', eFa, _) = runIdentity (f obj delta)
  in case eFa of
     Left anim -> fetchInbetweens delta (k - 1) obj' anim (acc ++ [obj])
     Right _ -> acc ++ [obj, obj']
