module Util.Coordinates where

unCenterCoords :: (Float, Float) -> (Float, Float) -> (Float, Float)
unCenterCoords (screenW, screenH) (x, y) = (x + screenW / 2, (-y) + screenH / 2)

toCenterCoords :: (Float, Float) -> (Float, Float) -> (Float, Float)
toCenterCoords (screenW, screenH) (x, y) = (x - screenW / 2, (-y) + screenH / 2)

insideMenuBtn :: (Float, Float) -> Bool
insideMenuBtn (x, y) = x >= 86 && x <= 100 && y >= 0 && y <= 25

insideNavBarBtn :: (Float, Float) -> Maybe Int
insideNavBarBtn (x, y) =
  if y >= 0 && y <= 25 then
    case x of
      x' | x' >= 4 && x <= 24 -> Just 1
      x' | x' >= 32 && x <= 52 -> Just 2
      x' | x' >= 60 && x <= 80 -> Just 3
      _ -> Nothing
  else
    Nothing
