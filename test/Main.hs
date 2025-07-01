{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.Accelerate (Z (..), (:.) (..))
import Data.Array.Accelerate qualified as A
import Data.Default (Default (def))
import Data.Word (Word16)
import Paths_sudoku (getDataFileName)
import Sudoku.Accelerate.Run (foldMat)
import Sudoku.Cell (Digit)
import Sudoku.Grid
  ( Grid,
  )
import Sudoku.Simplifiers
  ( HasSimplifierResult (contradictionsExplained),
    SimplifyKnowns (SimplifyKnowns),
    fullSimplifyStep,
  )
import Sudoku.Solver (parseGrid, runSolver, solverCheckGridSolved)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "Sudoku Tests" [fullSolverTests, simplifierTests]

fullSolverTests :: TestTree
fullSolverTests =
  testGroup
    "Full Solver Tests"
    [ solverTest "basic" "test/data/test-basic.json",
      solverTest "hard" "test/data/test-hard.json",
      solverTest "expert" "test/data/test-expert.json",
      solverTest "expert2" "test/data/test-expert2.json",
      solverTest "diabolical" "test/data/test-diabolical.json"
    ]

simplifierTests :: TestTree
simplifierTests = testGroup "Simplifier Tests" [testAcc]

testAcc :: TestTree
testAcc = testCase "acc.f" . liftIO $ print "m: " >> print m >> print (foldMat m)
  where
    m :: A.Array (Z :. Int :. Int) Word16
    m = A.fromList (Z :. 3 :. 2) [0 ..]

solverTest :: String -> FilePath -> TestTree
solverTest name file = withResource (getDataFileName file >>= parseGrid) checkContradictions (solverTestCase name)

solverTestCase :: String -> IO (Grid Digit) -> TestTree
solverTestCase name mg = testCase name $ mg >>= testPuzzleSolver

testPuzzleSolver :: Grid Digit -> Assertion
testPuzzleSolver = runSolver def >=> pure . solverCheckGridSolved >=> assertBool "the solver failed to solve the puzzle"

checkContradictions :: Grid Digit -> Assertion
checkContradictions =
  fullSimplifyStep SimplifyKnowns
    >>> view contradictionsExplained
    >>> null
    >>> assertBool "the solver returned a contradicted grid"

-- -- | `Traversal` that we can use to set an entire region
-- gridRegion :: RegionIndicator -> Word8 -> IndexedTraversal' Int (Grid Digit) (Cell Digit)
-- gridRegion ri i = reindexed (rel ri) (runIndexedTraversal $ riLens ri)
--   where
--     riLens Row = rowAt i
--     riLens Column = colAt i
--     riLens Box = boxAt i
--     rel :: RegionIndicator -> CellPos -> Int
--     rel Row (_, c, _) = fromIntegral c
--     rel Column (r, _, _) = fromIntegral r
--     rel Box loc = fromIntegral (boxIndex loc)

-- genBitSet :: Gen (BS.BitSet Word16 Digit)
-- genBitSet = BS.fromList <$> F.withoutShrinking (list (R.between (2, 4)) (inRange (R.enum (One, Nine))))

-- genGrid :: VU.Vector (Cell Digit) -> Gen (RegionIndicator, Word8, Grid Digit)
-- genGrid region = do
--     ri <- F.withoutShrinking $ F.elem (NonEmpty.fromList [Row, Column, Box])
--     i <- F.withoutShrinking . F.int $ between (1, 9)
--     let g = def & gridRegion ri (fromIntegral i) %@~ const . (region VU.!) . (+ (-1))
--     return (ri, fromIntegral i, g)

-- relIndexToCellPos :: RegionIndicator -> Word8 -> Int -> CellPos
-- relIndexToCellPos Row i offset = (i, fromIntegral offset, boxNumber i (fromIntegral offset))
-- relIndexToCellPos Column i offset = (fromIntegral offset, i, boxNumber (fromIntegral offset) i)
-- relIndexToCellPos Box i offset = (i, fromIntegral offset) ^. from boxIndexing
