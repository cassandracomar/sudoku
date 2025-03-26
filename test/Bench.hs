module Main where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nfAppIO)
import Data.Default (def)
import Paths_sudoku (getDataFileName)
import Sudoku.Cell (Digit)
import Sudoku.Grid (Grid)
import Sudoku.Solver (parseGrid, runSolver)

main :: IO ()
main =
    defaultMain
        [ bgroup
            "Solver Benchmarks"
            [ benchCase "basic"
            , benchCase "hard"
            , benchCase "expert"
            , benchCase "expert2"
            , benchCase "diabolical"
            ]
        ]

prepareGrid :: String -> IO (Grid Digit)
prepareGrid name = getDataFileName ("test/data/test-" ++ name ++ ".json") >>= parseGrid

benchCase :: String -> Benchmark
benchCase name = env (prepareGrid name) $ \grid -> bench name $ nfAppIO (runSolver def) grid
