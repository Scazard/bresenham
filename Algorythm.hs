module Algorythm where

import Structure
import Data.List

gridSize :: Pos
gridSize = (20, 10)

setSquare :: Pos -> Grid -> Grid
setSquare (x, y) (Grid grid) = Grid $ first ++ [replace x (grid!!y)] ++ tail last
  where 
    (first, last) = splitAt y grid

replace :: Int -> [Square] -> [Square]
replace i [] = []
replace 0 (x:xs) = On : xs
replace i (x:xs) = x : replace (i-1) xs

dydx :: Pos -> Pos -> Pos
dydx (x0, y0) (x1, y1) = (x1-x0, y1-y0)

emptyGrid :: Pos -> Grid
emptyGrid (x, y) = Grid $ replicate y (replicate x Off)

getDir :: Pos -> Pos
getDir (dx, dy) = (x,y)
  where
    x = if dx>0 then 1 else if dx < 0 then -1 else 0
    y = if dy>0 then 1 else if dy < 0 then -1 else 0

abst :: Pos -> Pos
abst (x,y) = (abs x, abs y)
startDraw :: Pos -> Pos -> Grid -> Grid
startDraw start end grid = do
  let delta@(dx, dy) = dydx start end
  let dir = getDir delta
  drawLine start (abst delta) dir end grid $ 2*(min (abs dy) (abs dx)) - (max (abs dy) (abs dx))

drawLine :: Pos -> Pos -> Pos -> Pos-> Grid -> Int -> Grid
drawLine cur@(x, y) delta@(dx, dy) dir@(we, ns) end grid ans
  | cur == end = finish cur grid 
  | ans < 0   = do
    drawLine (if abs dx>=abs dy then x + we else x, if abs dy>=abs dx then y + ns else y) delta dir end (setSquare cur grid) $ ans + 2*(min dy dx)
  | otherwise = do
    drawLine (x+we, y+ns) delta dir end (setSquare cur grid) $ ans + 2*(min dy dx) - 2*(max dy dx)

finish :: Pos -> Grid -> Grid
finish end grid = setSquare end grid