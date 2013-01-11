module BingoGame
  (newGame
  ,BingoGame(..)
  ,markCell) where

import Control.Applicative
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|<))
import qualified Data.Foldable as Foldable

-- data BingoGame = BingoGame BingoTable InvertedTable
data BingoGame =
  BingoGame
    { table :: BingoTable
    , invertedTable :: InvertedTable
    , completingLines :: CompletingLines
    , bingoLines :: Seq Line }
type BingoTable = Map Point Cell
type InvertedTable = IntMap [Point]
data CompletingLines =
  -- is Map really good?
  CompletingLines (IntMap Line) (IntMap Line) -- only horizontal lines and vertical lines so far.

data Line = Line Direction (Seq Point)
-- only horizontal lines and vertical lines so far.
data Direction = Horizontal | Vertical deriving (Show)

type Point = (Int, Int)

data Cell =
  Cell { value :: Int, marked :: Bool }
  | CenterCell

getValue :: Cell -> Int
getValue CenterCell = 0
getValue c = value c
isMarked :: Cell -> Bool
isMarked CenterCell = True
isMarked c = marked c

-- TODO: use blaze-builder
instance Show BingoGame where
  show (BingoGame bt _it _cl bl)  = showTable bt ++ showBingos bl

showTable :: BingoTable -> String
showTable bt = snd $ Map.foldWithKey build (1, "") bt
  where
    build (x, _) cell (lastRow, ac)
      -- processing a row same with the last one.
      | x == lastRow = ( lastRow, ac ++ show cell )
      -- processing row changed.
      | otherwise = ( x, ac ++ "\n" ++ show cell )

showBingos :: Seq Line -> String
showBingos = Foldable.foldr buildBingos ""
  where
    buildBingos (Line d ps) ac =
      "BINGO! " ++ show d ++ ": " ++ show ps ++ "\n" ++ ac

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
newGame g = BingoGame bt it cl bl
  where
    bt = generateTable g
    it = invert bt
    cl = noMarkedLines
    bl = Seq.empty

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
roundAsCellValue i = ((abs i) `mod` 75) + 1

mkCellAt :: Point -> Int -> Cell
mkCellAt (x, y) i
  | x == center && y == center = CenterCell
  | otherwise = Cell i False

invert :: BingoTable -> InvertedTable
invert bt =
  IntMap.fromListWith (++) $ map (f . swap) $ Map.toAscList bt
  where
    swap (x1, x2) = (x2, x1)
    f (y1, y2) = (getValue y1, y2:[])

noMarkedLines :: CompletingLines
noMarkedLines = CompletingLines IntMap.empty IntMap.empty

markCell :: Int -> BingoGame -> BingoGame
markCell i (BingoGame bt it cl bl) = BingoGame bt' it' cl' bl'
  where
    (ps', it') = IntMap.updateLookupWithKey popPoint i it

    popPoint _ [] = Nothing
    popPoint _ (_:ps) = Just ps

    newCell = (Cell i True)

    bt' = maybe bt whenJust1 ps'
    whenJust1 (p:_) = Map.insert p newCell bt

    (bs, cl') = updateLookupBingos cl 
    bl' = maybe bl whenJust2 bs

size :: Int
size = 5

center :: Int
center = d + m
  where
    (d, m) = size `divMod` 2

points :: [Point]
points = (,) <$> [1..size] <*> [1..size]
