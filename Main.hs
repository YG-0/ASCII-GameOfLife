import Board
import System.Console.GetOpt
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (getArgs)


defaultFile :: String
defaultFile = "init_board.txt"

defaultSteps :: Int
defaultSteps = 50

data Flag = Input String
          | Steps Int
          deriving Show


options :: [OptDescr Flag]
options = [
             Option
                ['i'] ["input"] (ReqArg Input "FILE")
                "Input file with board starting state"
           , Option
                ['s'] ["steps"] (ReqArg (Steps . read) "NUM")
                "Number of steps"
          ]


main = do
    argv <- getArgs
    let (opts, _, err) = getOpt Permute options argv
    if not (null err)
       then ioError $ userError $ concat err ++ usageInfo "Usage:" options
       else do
           let (input, steps) = parseOpts opts defaultFile defaultSteps
           initBoardStr <- readFile input
           let initBoard = readBoard $ filter (/= ' ') initBoardStr
           if initBoard == Nothing
              then ioError $ userError "Invalid board file"
              else playNSteps steps $ fromJust initBoard


parseOpts :: [Flag] -> String -> Int-> (String, Int)
parseOpts opts dInput dSteps =
    (firstOrDefault input dInput, firstOrDefault steps dSteps)
        where
            input = [i | (Input i) <- opts]
            steps = [s | (Steps s) <- opts]
            firstOrDefault l d = if null l then d else head l


playNSteps :: Int -> Board -> IO ()
playNSteps n b = do
    putStrLn $ showBoard b ++ "\n\n"
    if n == 0
    then return ()
    else playNSteps (n - 1) $ boardNextState b
