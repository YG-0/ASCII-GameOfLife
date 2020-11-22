module Cell
  ( Cell(..)
  , readCell
  , isAlive
  , cellNextState
  , neighboursIndexes
  ) where

import           Data.List                      ( delete )

data Cell = Dead | Alive deriving(Eq, Read)
instance Show Cell where
  show Alive = "#"
  show Dead  = "-"

isAlive :: Cell -> Bool
isAlive = (== Alive)

readCell :: Char -> Maybe Cell
readCell c | c == '-'  = Just Dead
           | c == '#'  = Just Alive
           | otherwise = Nothing

{- |
 - Returns the next state of a cell
 - gets the cell state and the number of living cells
 - neighbours
 -}
cellNextState :: Cell -> Int -> Cell
cellNextState cell liveCells = if isAlive cell
  then if (liveCells == 2 || liveCells == 3) then Alive else Dead
  else if liveCells == 3 then Alive else Dead

-- |Indexes of a cell's neighbours (may be out of bounds)
neighboursIndexes :: (Int, Int) -> [(Int, Int)]
neighboursIndexes (x, y) =
  let indexes = [ [ (x + dx, y + dy) | dx <- [-1 .. 1] ] | dy <- [-1 .. 1] ]
  in  delete (x, y) $ concat indexes

