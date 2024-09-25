module Main where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nfAppIO)
import Paths_sudoku (getDataFileName)
import Sudoku (Grid, parseGrid, runSolver)

main :: IO ()
main =
    defaultMain
        [ bgroup
            "Solver Benchmarks"
            [ benchCase "basic"
            , benchCase "hard"
            , benchCase "expert"
            , benchCase "diabolical"
            ]
        ]

prepareGrid :: String -> IO Grid
prepareGrid name = getDataFileName ("test/data/test-" ++ name ++ ".json") >>= parseGrid

benchCase :: String -> Benchmark
benchCase name = env (prepareGrid name) $ \grid -> bench name $ nfAppIO runSolver grid
