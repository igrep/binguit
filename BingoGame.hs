module BingoGame
  (generateTable
  ,BingoTable) where

import Control.Applicative
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)

newtype BingoTable = BingoTable (Map Point Cell)
type Point = (Char, Int)
data Cell =
  Cell Int Bool
  | CenterCell

instance Show Cell where
  show CenterCell = "(  )"
  show (Cell i m) =
    if m
      then "(" ++ f i ++ ")"
      else " " ++ f i ++ " "
    where
      -- assume j is between 1 and 75
      f j = if j < 10 then " " ++ show j else show j

-- TODO: How to not generate numbers already generated.
--       Change the range of generated value by column
generateTable :: RandomGen g => g -> BingoTable
generateTable g =
  BingoTable $ Map.fromList $ map f $ zip points $ randoms g
  where
    f (p, i) = (p, mkCellAt p i)

mkCellAt :: Point -> Int -> Cell
mkCellAt ('N', 3) _ = CenterCell
mkCellAt p@(x, _y) i = Cell i False

points :: [Point]
points = (,) <$> "BINGO" <*> [1..5]
