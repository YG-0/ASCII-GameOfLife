module Board
  ( Board
  , showBoard
  , readBoard
  , boardNextState
  ) where

import           Cell
import           Data.Array
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromJust )

type Board = Array (Int, Int) Cell


showBoard :: Board -> String
showBoard board =
  let ((x0, y0), (xf, yf)) = bounds board
      rowToStr x =
        foldl' (\acc y -> acc ++ show (board ! (x, y))) "" [y0 .. yf]
      rowsStrs = map rowToStr [x0 .. xf]
  in  foldl' (\acc r -> acc ++ "\n" ++ r) "" rowsStrs


readBoard :: String -> Maybe Board
readBoard str = if not $ isValidBoardLines boardLines
  then Nothing
  else Just $ listArray ((0, 0), (rows - 1, cols - 1)) $ concat justBoardLines
 where
  boardLines     = map readBoardLine $ lines str
  rows           = length justBoardLines
  cols           = length $ head justBoardLines
  justBoardLines = map fromJust boardLines


boardNextState :: Board -> Board
boardNextState board =
  let bnd                  = bounds board
      ind                  = indices board
      aliveNeighboursList  = map (aliveNeighbours board) ind
      aliveNeigbhoursBoard = listArray bnd aliveNeighboursList
  in  listArray
        bnd
        [ cellNextState (board ! i) (aliveNeigbhoursBoard ! i) | i <- ind ]


readBoardLine :: String -> Maybe [Cell]
readBoardLine line =
  let maybeCells = map readCell line
  in  if Nothing `elem` maybeCells
        then Nothing
        else Just $ map fromJust maybeCells


isValidBoardLines :: [Maybe [Cell]] -> Bool
isValidBoardLines [] = False
isValidBoardLines bl@(c : cs) =
  (not $ Nothing `elem` bl) && (and $ map (== (length c)) lengths)
  where lengths = map length cs


aliveNeighbours :: Board -> (Int, Int) -> Int
aliveNeighbours board (x, y) =
  let cell i j = if inRange (bounds board) (i, j) then board ! (i, j) else Dead
  in  length $ filter isAlive $ map (\(i, j) -> cell i j) $ neighboursIndexes
        (x, y)

