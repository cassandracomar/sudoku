{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad ((>=>))
import Data.BitSet qualified as BS
import Data.Default (Default (def))
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as S
import Data.Vector.Generic.Lens (vectorTraverse)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word16, Word8)
import Debug.Trace (trace)
import Paths_sudoku (getDataFileName)
import Sudoku.Cell (Cell, CellPos, Digit (Nine, One), boxIndex, boxIndexing, boxNumber, mkCell, _CellSet, _Possibly)
import Sudoku.Grid (
    CommonPossibilities (CommonPossibilities),
    Grid,
    boxAt,
    colAt,
    grid,
    knownTuples,
    rowAt,
 )
import Sudoku.Simplifiers (
    HasSimplifierResult (contradictionsExplained, simplifierOutput),
    fullSimplifyStep,
    SimplifyCellTuples (SimplifyCellTuples),
    SimplifyKnowns (SimplifyKnowns),
 )
import Sudoku.Solver (SolverConstraints, parseGrid, runSolver, solverCheckGridSolved)
import Sudoku.Summaries (RegionIndicator (..))
import Test.Falsify.Generator (Gen, inRange, list)
import Test.Falsify.Generator qualified as F
import Test.Falsify.Predicate (satisfies, (.$))
import Test.Falsify.Property (Property, assert, gen)
import Test.Falsify.Range (between)
import Test.Falsify.Range qualified as R
import Test.Tasty
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "Sudoku Tests" [fullSolverTests, simplifierTests]

fullSolverTests :: TestTree
fullSolverTests =
    testGroup
        "Full Solver Tests"
        [ solverTest "basic" "test/data/test-basic.json"
        , solverTest "hard" "test/data/test-hard.json"
        , solverTest "expert" "test/data/test-expert.json"
        , solverTest "expert2" "test/data/test-expert2.json"
        , solverTest "diabolical" "test/data/test-diabolical.json"
        ]

simplifierTests :: TestTree
simplifierTests = testGroup "Simplifier Tests" []

solverTest :: String -> FilePath -> TestTree
solverTest name file = withResource (getDataFileName file >>= parseGrid) checkContradictions (solverTestCase name)

solverTestCase :: String -> IO (Grid Digit) -> TestTree
solverTestCase name mg = testCase name $ mg >>= testPuzzleSolver

testPuzzleSolver :: Grid Digit -> Assertion
testPuzzleSolver = runSolver >=> pure . solverCheckGridSolved >=> assertBool "the solver failed to solve the puzzle"

checkContradictions :: Grid Digit -> Assertion
checkContradictions =
    fullSimplifyStep SimplifyKnowns
        >>> view contradictionsExplained
        >>> null
        >>> assertBool "the solver returned a contradicted grid"

-- | `Traversal` that we can use to set an entire region
gridRegion :: RegionIndicator -> Word8 -> IndexedTraversal' Int (Grid Digit) (Cell Digit)
gridRegion ri i = reindexed (rel ri) (runIndexedTraversal $ riLens ri)
  where
    riLens Row = rowAt i
    riLens Column = colAt i
    riLens Box = boxAt i
    rel :: RegionIndicator -> CellPos -> Int
    rel Row (_, c, _) = fromIntegral c
    rel Column (r, _, _) = fromIntegral r
    rel Box loc = fromIntegral (boxIndex loc)

genBitSet :: Gen (BS.BitSet Word16 Digit)
genBitSet = BS.fromList <$> F.withoutShrinking (list (R.between (2, 4)) (inRange (R.enum (One, Nine))))

genGrid :: VU.Vector (Cell Digit) -> Gen (RegionIndicator, Word8, Grid Digit)
genGrid region = do
    ri <- F.withoutShrinking $ F.elem (NonEmpty.fromList [Row, Column, Box])
    i <- F.withoutShrinking . F.int $ between (1, 9)
    let g = def & gridRegion ri (fromIntegral i) %@~ const . (region VU.!) . (+ (-1))
    return (ri, fromIntegral i, g)

relIndexToCellPos :: RegionIndicator -> Word8 -> Int -> CellPos
relIndexToCellPos Row i offset = (i, fromIntegral offset, boxNumber i (fromIntegral offset))
relIndexToCellPos Column i offset = (fromIntegral offset, i, boxNumber (fromIntegral offset) i)
relIndexToCellPos Box i offset = (i, fromIntegral offset) ^. from boxIndexing
