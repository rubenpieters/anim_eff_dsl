module Util.Coordinates where

unCenterCoords :: (Float, Float) -> (Float, Float) -> (Float, Float)
unCenterCoords (screenW, screenH) (x, y) = (x + screenW / 2, (-y) + screenH / 2)

toCenterCoords :: (Float, Float) -> (Float, Float) -> (Float, Float)
toCenterCoords (screenW, screenH) (x, y) = (x - screenW / 2, (-y) + screenH / 2)

insideMenuBtn :: (Float, Float) -> Bool
insideMenuBtn (x, y) = x >= 86 && x <= 100 && y >= 0 && y <= 25
