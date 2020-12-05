module BoardTest
  ( boardTests
  ) where

import           Test.HUnit
import           Board
import           Cell
import           Data.Array


boardTests = test $ showBoardTests ++ readBoardTests ++ boardNextStateTests

board0 :: Board
board0 = listArray
  ((0, 0), (2, 3))
  [Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead]

boardStr0 :: String
boardStr0 = "\n#---\n-#--\n--#-"

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
  [ "readBoard unmatch colunms number" ~: readBoard "#-\n-##-\n" ~=? Nothing
  , "readBoard test" ~: readBoard boardStr0 ~=? Just board0
  ]

boardNextStateTests =
  [ "Empty board next state" ~: boardNextState emptyBoard ~=? emptyBoard
  , "Glider board next state" ~: boardNextState gliderBoard0 ~=? gliderBoard1
  ]
