module Main where

import Control.Monad (void)
import Options.Applicative (
    Parser,
    customExecParser,
    help,
    helper,
    info,
    long,
    metavar,
    prefs,
    progDesc,
    short,
    showHelpOnEmpty,
    showHelpOnError,
    simpleVersioner,
    strArgument,
    switch,
    (<**>),
 )
import Sudoku.Grid (SolverOptions (..))
import Sudoku.Solver

solverOpts :: Parser SolverOptions
solverOpts =
    SolverOptions
        <$> strArgument (metavar "FILE" <> help "file path to the givens for the puzzle to solve")
        <*> pure False -- when running the console app, never treat it as a test case
        <*> switch (short 'v' <> long "verbose" <> help "enable verbose, step by step logging")

main :: IO ()
main = customExecParser ps opts >>= void . runSolverOnFile
  where
    ps = prefs $ showHelpOnEmpty <> showHelpOnError
    opts = info (solverOpts <**> helper <**> simpleVersioner "sudoku v0.0.1.0") (progDesc "run the sudoku solver on a puzzle")
