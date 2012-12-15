module BingoGame
  (newGame
  ,BingoGame
  ,showTable) where

import Control.Applicative
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)

data BingoGame =
  BingoGame
    { table :: BingoTable
    , invertedTable :: InvertedTable }
type BingoTable = Map Point Cell
type InvertedTable = Map Int [Point]
type Point = (Int, Int)
data Cell =
  Cell { value :: Int, isMarked :: Bool }
  | CenterCell

-- TODO: use difflist
showTable :: BingoTable -> String
showTable b = snd $ Map.foldWithKey f (1, "") b
  where
    f (x, _) cell (lastKey, ac)
      | x == lastKey = ( lastKey, ac ++ show cell )
      | otherwise = ( x, ac ++ "\n" ++ show cell )

instance Show Cell where
  -- TODO: How to colorize?
  show CenterCell = "(  )"
  show (Cell i m) =
    if m
      then "(" ++ f i ++ ")"
      else " " ++ f i ++ " "
    where
      -- assume j is between 1 and 75
      f j = if j < 10 then " " ++ show j else show j

newGame :: RandomGen g => g -> BingoGame
newGame g = BingoGame bt it
  where
    bt = generateTable g
    it = invert bt

-- TODO: How to not generate numbers already generated.
--       Change the range of generated value by column
generateTable :: RandomGen g => g -> BingoTable
generateTable g =
  Map.fromDistinctAscList $ map f $ zip points $ randoms g
  where
    f (p, i) = (p, mkCellAt p (( i `mod` 75 ) + 1 :: Int))

invert :: BingoTable -> InvertedTable
invert bt =
  Map.fromListWith (++) $ map (f . swap) $ Map.toAscList bt
  where
    swap (x1, x2) = (x2, x1)
    f (y1, y2) = (value y1, y2:[])

mkCellAt :: Point -> Int -> Cell
mkCellAt (x, y) i
  | x == center && y == center = CenterCell
  | otherwise = Cell i False

{-mark :: BingoTable -> Int -> BingoTable-}
{-mark bt i =-}

size :: Int
size = 5

center :: Int
center = d + m
  where
    (d, m) = size `divMod` 2

points :: [Point]
points = (,) <$> [1..size] <*> [1..size]
