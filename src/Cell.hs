module Cell
  ( Cell(..)
  , readCell
  , isAlive
  , cellNextState
  ) where

data Cell = Dead | Alive deriving(Eq, Read)
instance Show Cell where
  show Alive = "#"
  show Dead  = " "

isAlive :: Cell -> Bool
isAlive = (== Alive)

readCell :: Char -> Cell
readCell ch = if ch `elem` " -" then Dead else Alive


{- |
 - Returns the next state of a cell
 - gets the cell state and the number of living cells
 - neighbours
 -}
cellNextState :: Cell -> Int -> Cell
cellNextState cell liveCells = if isAlive cell
  then if (liveCells == 2 || liveCells == 3) then Alive else Dead
  else if liveCells == 3 then Alive else Dead
