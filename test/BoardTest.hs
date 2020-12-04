module BoardTest
  ( boardTests
  ) where

import           Test.HUnit
import           Board
import           Cell
import           Data.Array

boardArray :: Board
boardArray = listArray
  ((0, 0), (2, 3))
  [Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead]

boardStr :: String
boardStr = "\n#---\n-#--\n--#-"

boardTests = test $ showBoardTests ++ readBoardTests

showBoardTests = ["showBoard test" ~: showBoard boardArray ~=? boardStr]

readBoardTests =
  [ "readBoard unmatch colunms number" ~: readBoard "#-\n-##-\n" ~=? Nothing
  , "readBoard test" ~: readBoard boardStr ~=? Just boardArray
  ]
