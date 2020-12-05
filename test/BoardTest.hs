module BoardTest
  ( boardTests
  ) where

import           Test.HUnit
import           Board
import           Cell
import           Data.Array


boardTests = test $ showBoardTests ++ readBoardTests ++ boardNextStateTests


singleAliveBoard :: Board
singleAliveBoard = listArray ((0, 0), (0, 0)) [Alive]

singleAliveStr :: String
singleAliveStr = "#"

singleDeadBoard :: Board
singleDeadBoard = listArray ((0, 0), (0, 0)) [Dead]

singleDeadStr :: String
singleDeadStr = " "

singleRowBoard :: Board
singleRowBoard = listArray ((0, 0), (0, 4)) cellsList0

singleRowStr :: String
singleRowStr = "#  # "

singleColBoard :: Board
singleColBoard = listArray ((0, 0), (4, 0)) cellsList0

singleColStr :: String
singleColStr = "#\n \n \n#\n \n"

cellsList0 :: [Cell]
cellsList0 = [Alive, Dead, Dead, Alive, Dead]

board0 :: Board
board0 = listArray
  ((0, 0), (2, 3))
  [Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead]

boardStr0 :: String
boardStr0 = "\n#   \n #  \n  # "

emptyBoard :: Board
emptyBoard = listArray
  ((0, 0), (2, 3))
  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]

gliderBoard0 :: Board
gliderBoard0 = listArray
  ((0, 0), (3, 3))
  [ Dead
  , Alive
  , Dead
  , Dead
  , Dead
  , Dead
  , Alive
  , Dead
  , Alive
  , Alive
  , Alive
  , Dead
  , Dead
  , Dead
  , Dead
  , Dead
  ]

gliderBoard1 :: Board
gliderBoard1 = listArray
  ((0, 0), (3, 3))
  [ Dead
  , Dead
  , Dead
  , Dead
  , Alive
  , Dead
  , Alive
  , Dead
  , Dead
  , Alive
  , Alive
  , Dead
  , Dead
  , Alive
  , Dead
  , Dead
  ]

showBoardTests = ["showBoard test" ~: showBoard board0 ~=? boardStr0]

readBoardTests =
  [ "readBoard Single cell Alive"
    ~:  readBoard singleAliveStr
    ~=? Just singleAliveBoard
  , "readBoard Single cell Dead"
    ~:  readBoard singleDeadStr
    ~=? Just singleDeadBoard
  , "readBoard single row test"
    ~:  readBoard singleRowStr
    ~=? Just singleRowBoard
  , "readBoard single column test"
    ~:  readBoard singleColStr
    ~=? Just singleColBoard
  , "readBoard unmatch colunms number" ~: readBoard "#-\n-##-\n" ~=? Nothing
  , "readBoard test" ~: readBoard boardStr0 ~=? Just board0
  ]

boardNextStateTests =
  [ "Empty board next state" ~: boardNextState emptyBoard ~=? emptyBoard
  , "Glider board next state" ~: boardNextState gliderBoard0 ~=? gliderBoard1
  ]
