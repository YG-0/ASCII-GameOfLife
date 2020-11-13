import Board
import System.Console.GetOpt
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import System.Process (system)


defaultFile :: String
defaultFile = "init_board.txt"

defaultSteps :: Int
defaultSteps = 50

defaultDelayMS :: Int
defaultDelayMS = 150

-- |Command line arguments flags 
data Flag = Input String
          | Steps Int
          | Delay Int
          deriving Show


options :: [OptDescr Flag]
options = [
             Option
                ['i'] ["input"] (ReqArg Input "FILE")
                "Input file with board starting state"
           , Option
                ['s'] ["steps"] (ReqArg (Steps . read) "NUM")
                "Number of steps"
           , Option
                ['d'] ["delay"] (ReqArg (Delay . read) "MS")
                "Delay in milliseconds between steps"
          ]


main = do
    argv <- getArgs
    let (opts, _, err) = getOpt Permute options argv
    if not (null err)
       then ioError $ userError $ concat err ++ usageInfo "\nUsage:" options
       else do
           let (input, steps, delay) =
                   parseOpts opts defaultFile defaultSteps defaultDelayMS
           initBoardStr <- readFile input
           let initBoard = readBoard $ filter (/= ' ') initBoardStr
           if initBoard == Nothing
              then ioError $ userError "Invalid board file"
              else playNSteps steps delay $ fromJust initBoard


parseOpts :: [Flag] ->          -- list of command line arguments flags
             String ->          -- default input file name
             Int    ->          -- default steps
             Int    ->          -- default delay
             (String, Int, Int) -- (input-file-name, steps, delay)
parseOpts opts dInput dSteps dDelay =
    (
        firstOrDefault input dInput
      , firstOrDefault steps dSteps
      , firstOrDefault delay dDelay
    )
        where
            input = [i | (Input i) <- opts]
            steps = [s | (Steps s) <- opts]
            delay = [s | (Delay s) <- opts]
            firstOrDefault l d = if null l then d else head l


playNSteps :: Int   -> -- steps
              Int   -> -- delay
              Board -> -- init board
              IO ()
playNSteps n delay board = do
    system "clear"
    putStrLn $ showBoard board
    threadDelay $ delay * 1000
    if n == 0
    then return ()
    else playNSteps (n - 1) delay $ boardNextState board
