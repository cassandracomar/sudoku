module Main (main) where

import Paths_sudoku (getDataFileName)
import Sudoku (Grid, checkContradictions, parseGrid, runSolver, solved)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "Sudoku Tests" [fullSolverTests]

fullSolverTests :: TestTree
fullSolverTests =
    testGroup
        "Full Solver Tests"
        [ solverTest "basic" "test/data/test-basic.json"
        , solverTest "hard" "test/data/test-hard.json"
        , solverTest "expert" "test/data/test-expert.json"
        , solverTest "diabolical" "test/data/test-diabolical.json"
        ]

solverTest :: String -> FilePath -> TestTree
solverTest name file = withResource (getDataFileName file >>= parseGrid) checkContradictions (solverTestCase name)

solverTestCase :: String -> IO Grid -> TestTree
solverTestCase name mg = testCase name $ mg >>= testPuzzleSolver

testPuzzleSolver :: Grid -> Assertion
testPuzzleSolver g = do
    g' <- runSolver g
    assertBool "the solver did not solve the puzzle" $ solved g'
