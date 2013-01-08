module BingoGame
  (newGame
  ,BingoGame
  ,markCell) where

import Control.Applicative
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)

-- data BingoGame = BingoGame BingoTable InvertedTable
data BingoGame =
  BingoGame
    { table :: BingoTable
    , invertedTable :: InvertedTable }
type BingoTable = Map Point Cell
type InvertedTable = Map Int [Point]
type Point = (Int, Int)

data Cell =
  Cell { value :: Int, marked :: Bool }
  | CenterCell

type Line = [Cell]

getValue :: Cell -> Int
getValue CenterCell = 0
getValue c = value c
isMarked :: Cell -> Bool
isMarked CenterCell = True
isMarked c = marked c

-- TODO: use difflist
instance Show BingoGame where
  show (BingoGame bt _it)  = snd $ Map.foldWithKey f (1, "") bt
    where
      f (x, _) cell (lastRow, ac)
        -- processing a row same with the last one.
        | x == lastRow = ( lastRow, ac ++ show cell )
        -- processing row changed.
        | otherwise = ( x, ac ++ "\n" ++ show cell )

instance Show Cell where
  -- TODO: How to colorize?
  show CenterCell = "(  )"
  show (Cell i m) =
    if m
      then "(" ++ i' ++ ")"
      else " " ++ i' ++ " "
    where
      -- assume i is between 1 and 75
      i' = if i < 10 then ' ':(show i) else show i

newGame :: RandomGen g => g -> BingoGame
newGame g = BingoGame bt it
  where
    bt = generateTable g
    it = invert bt

-- TODO: How to not generate numbers already generated.
--       Change the range of generated value by column
generateTable :: RandomGen g => g -> BingoTable
generateTable g =
  Map.fromDistinctAscList $ zipWith f points $ randoms g
  where
    f p i = attach p $ roundAsCellValue i

attach :: Point -> Int -> (Point, Cell)
attach p i = (p, mkCellAt p i)

roundAsCellValue :: Int -> Int
roundAsCellValue i = (abs i) `mod` 75 + 1

invert :: BingoTable -> InvertedTable
invert bt =
  Map.fromListWith (++) $ map (f . swap) $ Map.toAscList bt
  where
    swap (x1, x2) = (x2, x1)
    f (y1, y2) = (getValue y1, y2:[])

mkCellAt :: Point -> Int -> Cell
mkCellAt (x, y) i
  | x == center && y == center = CenterCell
  | otherwise = Cell i False

markCell :: Int -> BingoGame -> BingoGame
markCell i (BingoGame bt it) = BingoGame bt' it'
  where
    (ps', it') = Map.updateLookupWithKey popPoint i it
    popPoint _ [] = Nothing
    popPoint _ (_:ps) = Just ps
    bt' = maybe bt (\ (p:_) ->  Map.insert p (Cell i True) bt) ps'

lines :: BingoGame -> [Line]
lines b = Map.foldWithKey f

collectBingoLines = filter (all . isMarked)

collectBingoLines :: [Line] -> [Line]
collectBingoLines = filter (all . isMarked)

size :: Int
size = 5

center :: Int
center = d + m
  where
    (d, m) = size `divMod` 2

points :: [Point]
points = (,) <$> [1..size] <*> [1..size]
