import           CellTest
import           BoardTest
import           Test.HUnit

main = do
  runTestTT cellNextStateTests
  runTestTT boardTests
