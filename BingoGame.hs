module BingoGame
  (newGame
  ,BingoGame(..)
  ,markCell) where

import Control.Applicative
import System.Random
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

data BingoGame =
  BingoGame
    { table :: BingoTable
    , invertedTable :: InvertedTable
    , completingLines :: CompletingLines
    , bingoLines :: BingoLines }
type BingoTable = Map Point Cell
type InvertedTable = IntMap [Point]
data CompletingLines =
  -- is IntMap really good?
  CompletingLines (IntMap Line) (IntMap Line) -- only horizontal lines and vertical lines so far.
type BingoLines = [ (Direction, Int, Line) ]

type Line = IntMap Cell
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

showBingos :: BingoLines -> String
showBingos = Foldable.foldr buildBingos ""
  where
    buildBingos (d, i, l) ac =
      "BINGO! " ++ show d ++ " #" ++ show i ++ " : " ++ showLine l ++ "\n" ++ ac

showLine :: Line -> String
showLine = concat . intersperse " " . map (show . snd) . IntMap.toAscList

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
    cl = initialLines
    bl = []

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

initialLines :: CompletingLines
initialLines = CompletingLines onlyCenter onlyCenter
  where
    onlyCenter = IntMap.singleton center $ IntMap.singleton center CenterCell

markCell :: Int -> BingoGame -> BingoGame
markCell i (BingoGame bt it cl bl) = BingoGame bt' it' cl' bl'
  where
    (ps', it') = IntMap.updateLookupWithKey popPoint i it

    popPoint _ [] = Nothing
    popPoint _ (_:ps) = Just ps

    (bt', cl', bl') =
      maybe (bt, cl, bl) (updateWithPoints (bt, cl, bl) i) ps'

updateWithPoints ::
  (BingoTable, CompletingLines, BingoLines) -> Int -> [Point]
    -> (BingoTable, CompletingLines, BingoLines)
updateWithPoints b _ [] = b
updateWithPoints (bt, cl, bl) i (p:_) = (bt', cl', bl')
  where
    c' = (Cell i True) -- marked cell
    bt' = Map.insert p c' bt
    (cl', bl') = updateCollectBingos p c' cl bl

updateCollectBingos ::
  Point -> Cell -> CompletingLines -> BingoLines
    -> (CompletingLines, BingoLines)
updateCollectBingos (x, y) c (CompletingLines h v) bl =
  (CompletingLines h' v', bl')
  where
    h' = IntMap.alter appendToLine y c h
    v' = IntMap.alter appendToLine x c v

    appendToLine k Nothing c = IntMap.singleton k c
    appendToLine k (Just l) c = IntMap.insert k c l

    ys' = (IntMap.!) h' y
    xs' = (IntMap.!) v' x

    bl' =
      appendIfBingo Vertical x xs' $
        appendIfBingo Horizontal y ys' bl

appendIfBingo :: Direction -> Int -> Line -> BingoLines -> BingoLines
appendIfBingo d i l bl
  | IntMap.size l == size =  (d, i, l):bl
  | otherwise = bl

size :: Int
size = 5

center :: Int
center = d + m
  where
    (d, m) = size `divMod` 2

points :: [Point]
points = (,) <$> [1..size] <*> [1..size]
