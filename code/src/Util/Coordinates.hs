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

insideTodoItem :: (Float, Float) -> Maybe Int
insideTodoItem (x, y) | x >= 15 && x <= 85 && y >= 28.75 && y <= 48.75 = Just 1
insideTodoItem (x, y) | x >= 15 && x <= 85 && y >= 52.5 && y <= 72.5 = Just 2
insideTodoItem (x, y) | x >= 15 && x <= 85 && y >= 76.25 && y <= 96.25 = Just 3
insideTodoItem _ = Nothing
