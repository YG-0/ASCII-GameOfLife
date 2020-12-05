module Board
  ( Board
  , showBoard
  , readBoard
  , boardNextState
  ) where

import           Cell
import           Data.Array
import           Data.List                      ( delete
                                                , foldl'
                                                )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )

type Board = Array (Int, Int) Cell


showBoard :: Board -> String
showBoard board =
  let ((x0, y0), (xf, yf)) = bounds board
      rowToStr x =
        foldl' (\acc y -> acc ++ show (board ! (x, y))) "" [y0 .. yf]
      rowsStrs = map rowToStr [x0 .. xf]
  in  foldl' (\acc r -> acc ++ "\n" ++ r) "" rowsStrs


readBoard :: String -> Maybe Board
readBoard str = if not $ isAllLensEq strLines
  then Nothing
  else Just $ listArray ((0, 0), (rows - 1, cols - 1)) $ cellsList
 where
  cellsList = map readCell $ concat strLines
  rows      = length strLines
  cols      = length $ head strLines
  strLines  = filter (not . null) $ lines str
  isAllLensEq (x : xs) = all (== length x) $ map length xs


boardNextState :: Board -> Board
boardNextState board =
  let bnd                  = bounds board
      ind                  = indices board
      aliveNeighboursList  = map (aliveNeighbours board) ind
      aliveNeigbhoursBoard = listArray bnd aliveNeighboursList
  in  listArray
        bnd
        [ cellNextState (board ! i) (aliveNeigbhoursBoard ! i) | i <- ind ]


aliveNeighbours :: Board -> (Int, Int) -> Int
aliveNeighbours board (x, y) =
  let cell i j = if inRange (bounds board) (i, j) then board ! (i, j) else Dead
  in  length $ filter isAlive $ map (\(i, j) -> cell i j) $ neighboursIndexes
        (x, y)

-- |Indexes of a cell's neighbours (may be out of bounds)
neighboursIndexes :: (Int, Int) -> [(Int, Int)]
neighboursIndexes (x, y) =
  let indexes = [ [ (x + dx, y + dy) | dx <- [-1 .. 1] ] | dy <- [-1 .. 1] ]
  in  delete (x, y) $ concat indexes

