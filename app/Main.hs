module Main where

import Control.Monad (void)
import Options.Applicative (
    ParserInfo,
    execParser,
    help,
    info,
    metavar,
    progDesc,
    strArgument,
 )
import Sudoku

main :: IO ()
main = execParser opts >>= void . runSolverOnFile
  where
    opts :: ParserInfo String
    opts =
        info
            (strArgument (metavar "FILE" <> help "file path to the givens for the puzzle to solve"))
            (progDesc "run the sudoku solver on a puzzle")
