module Cw where 

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M


import Control.Monad (guard)

intToFloat :: Int -> Float
intToFloat = fromIntegral

input = [
  "###..#########.#####.",
  ".####.#####..####.#.#",
  ".###.#.#.#####.##..##",
  "##.####.#.###########",
  "###...#.####.#.#.####",
  "#.##..###.########...",
  "#.#######.##.#######.",
  ".#..#.#..###...####.#",
  "#######.##.##.###..##",
  "#.#......#....#.#.#..",
  "######.###.#.#.##...#",
  "####.#...#.#######.#.",
  ".######.#####.#######",
  "##.##.##.#####.##.#.#",
  "###.#######..##.#....",
  "###.##.##..##.#####.#",
  "##.########.#.#.#####",
  ".##....##..###.#...#.",
  "#..#.####.######..###",
  "..#.####.############",
  "..##...###..#########" ]

-- make input list into a set
toSet :: [[Char]] -> S.Set (Int, Int)
toSet input = S.fromList $ do
 (y, row) <- zip [0..] input
 (x, cell) <- zip [0..] row
 guard $ cell == '#'	
 pure (x, y)
	
type Angle = (Maybe Float, Sign, Sign) 

data Sign = Pos | Neg deriving (Eq, Show, Ord)

sign v = if v < 0 then Neg else Pos


radius (x0, y0) (x, y) =
 sqrt (intToFloat (x-x0)**2 + intToFloat (y-y0)**2)

angle (x0, y0) (x, y) = 
 let
  dx = x - x0
  dy = y - y0
  slope = intToFloat dy / intToFloat dx
 in
  (if dx == 0 then Nothing else Just slope, sign dx, sign dy)
  
visible :: (Int, Int) -> S.Set (Int, Int) -> Int 
visible (x0, y0) possible =
  S.size $
   S.map (angle (x0, y0)) $
   S.delete (x0, y0) possible
		
asteroids = toSet input
		
answer = S.findMax $ S.map (\pt -> visible pt asteroids) asteroids
		
main = print answer