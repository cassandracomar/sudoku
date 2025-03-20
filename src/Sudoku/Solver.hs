{-# LANGUAGE UndecidableInstances #-}

module Sudoku.Solver where

import Control.Applicative (Alternative (..))
import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (foldM, guard, unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.LogicState (TransLogicState (observeT))
import Control.Monad.RWS.Lazy (MonadState (put))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer.Lazy (runWriterT)
import Data.Aeson (eitherDecodeStrict')
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Word (Word16)
import Sudoku.Backtracking (Contradicted, Sudoku, attemptToContradict, findRestrictedCells, ixCell)
import Sudoku.Cell (Cell, CellPos, Digit, cellPos, showLocB, _CellSet, _Possibly)
import Sudoku.Grid
import Sudoku.Simplifiers
import Sudoku.Summaries (checkSolved, solved, summaryOf)
import TextShow (TextShow (showbList), toLazyText)

import Data.BitSet qualified as A.BS
import Data.ByteString qualified as BS
import Data.Set qualified as S
import Data.Text.Lazy qualified as T
import Data.Vector.Unboxed qualified as VU

type SolverConstraints a = (Show a, Ord a, Enum a, VU.Unbox a, VU.Unbox (Cell a))

printStep :: (SolverMonad m) => Simplify s a -> s a -> SimplifierResult s a -> m ()
printStep (Simplify f _) = printStepImpl f

printContradictions :: (SolverMonad m) => [Text] -> m ()
printContradictions contras = printChecked $ T.intercalate "\n" ("Found Contradictions: " : contras)

runSimplifierOnce :: forall m. (SolverMonad m) => Simplify Grid Digit -> Grid Digit -> m (Grid Digit)
runSimplifierOnce f g =
    ensure (const solveCheck) g <|> ensure (== g) g' <|> do
        let contras = res ^. contradictionsExplained
        when (null contras) $ printStep f g res
        unless (null contras) $ printContradictions contras
        guard (null contras)
        return g'
  where
    res = runSimplifierPure f g
    solveCheck = fromMaybe False $ res ^? isSolved
    g' = res ^. simplifierOutput

solverCheckGridSolved :: Grid Digit -> Bool
solverCheckGridSolved = checkSolved . summaryOf (grid . cellPos <. solved)

runSimplifier :: forall m. (SolverMonad m) => Simplify Grid Digit -> m (Grid Digit) -> m (Grid Digit)
runSimplifier f = fix $ \rec mg -> do
    g <- mg
    g' <- runSimplifierOnce f g
    ensure (== g) g' <|> rec (return g')

{- | run each basic strategy in sequence, interleaving strategies that should be run in between every pair of steps.
i.e. if we've marked some cells as `Known`, we should make sure that we remove impossible values from cells before
running the next simplifier.
-}
runCheapSimplifiers :: forall m. (SolverMonad m) => m (Grid Digit) -> m (Grid Digit)
runCheapSimplifiers = fix $ \rec mg -> do
    g <- mg
    g' <- foldM (flip runSimplifierOnce) g (orderSimplifiers cheapSimplifiers)
    ensure (== g) g' <|> rec (return g')

runExpensiveSimplifier :: forall m. (SolverMonad m) => Simplify Grid Digit -> Grid Digit -> m (Grid Digit)
runExpensiveSimplifier f g = runSimplifierOnce f g & runCheapSimplifiers

-- | run each expensive strategy in sequence, interleaving strategies that should be run in between every pair of steps.
runExpensiveSimplifiers :: forall m. (SolverMonad m) => m (Grid Digit) -> m (Grid Digit)
runExpensiveSimplifiers = fix $ \rec mg -> do
    g <- mg
    g' <- foldM (flip runExpensiveSimplifier) g (orderSimplifiers expensiveSimplifiers)
    ensure (== g) g' <|> rec (return g')

parseGrid :: String -> IO (Grid Digit)
parseGrid file = do
    j <- BS.readFile file
    let rawConstraints = eitherDecodeStrict' j ^. singular _Right
    let g = mkGrid $ mkRules rawConstraints
    return g

runSolverOnFile :: SolverOptions -> IO ()
runSolverOnFile opts = do
    g <- parseGrid (opts ^. fp)
    g' <- runSolver opts g
    guard (solverCheckGridSolved g')

{- | run the basic strategies for solving sudoku puzzles:
    1. if a `Cell` is already `Known`, remove it's value as a possibility from all `Cell`s in it's neighborhood.
    2. if a `Cell` only has one possibility remaining, mark it as `Known`
    3. if a `Digit` can only occur in a single location in a region (`Row`\/`Column`\/`Box`), mark it as `Known` in that location.

if recursively applying these strategies fails to make progress and the puzzle isn't solved yet, then try the more expensive strategies:
    1. try to remove possibilities for `Cell`s based on "Pointing" --
       i.e. if the only possible locations for a `Digit` in one region are aligned in an intersecting region, remove the `Digit` as a possibility
       from all `Cell`s in the intersecting region outside the one being checked. it's easier to understand this strategy visually (TODO: link to picture)
    2. look for pairs/triples/quadruples/etc. within regions of the puzzle. these can either show up as a set of digits that have to occupy the same cells
       or as a set of cells that have already been restricted to subsets of the same `Digit`s. e.g. if `1` and `2` can only occur in two places in a row,
       that's a pair. if three `Cell`s can each be `1 2`, `1 3`, and `2 3`, we have a `1 2 3` triple. in both cases, the digits in the `Tuple` can be
       removed from all `Cell`s outside the tuple and if there are other possible values in `Cell`s within the Tuple, we can remove those as well.
       this is because in the finished puzzle, each of the `Cell`s within the tuple will take one of the possible values within the tuple and no others,
       while `Cell`s outside of the tuple but within the same regions can't ever take any of those values.

the name of the game with these more expensive strategies isn't themselves to determine the value of a `Cell` but rather to reduce the number of possibilities
in `Cell`s. the three basic strategies are much more powerful than they seem and will solve puzzles very quickly once a few key possibilities are removed.
the backtracking solver takes this to the next level and attempts to rule out just one possibility from one key `Cell` at a time by trying to prove that if a
`Cell` does take a particular value, it results in a contradiction. the hardest puzzle I can find is completely unsolveable until just one value is eliminated
from one `Cell`. there are apparently even harder puzzles than that which require the solver (human or machine) to eliminate values from single `Cell`s by
trying all of the aforementioned strategies recursively just to eliminate a single possibility from a single `Cell`.
-}
runBasicSolver :: (SolverMonad m) => Grid Digit -> m (Grid Digit)
runBasicSolver g = do
    g' <- runCheapSimplifiers (return g)
    ensure solverCheckGridSolved g' <|> ensure (== g) g' <|> runExpensiveSimplifiers (return g')

printNotice :: (SolverMonad m) => [Text] -> m ()
printNotice ss = printChecked $ T.unlines report
  where
    reportHeader = [toLazyText dashRow, "NOTICE:"]
    reportFooter = [toLazyText dashRow]
    report = reportHeader <> ss <> reportFooter

printNoticeStart :: (SolverMonad m) => CellPos -> m ()
printNoticeStart loc =
    printNotice
        [ toLazyText $ "using the backtracking solver to search for impossible values in " <> showLocB loc
        , "another attempt to solve logically will be made after we've eliminated impossibilities from this cell."
        ]

printNoticeEnd :: (SolverMonad m, TextShow a, Enum a) => CellPos -> Contradicted a -> m ()
printNoticeEnd loc cs =
    printNotice
        [ toLazyText $
            "resuming logical solving with the knowledge that " <> showLocB loc <> " cannot be " <> showbList (A.BS.toList cs)
        ]

reportSolved :: (SolverMonad m) => Grid Digit -> m ()
reportSolved g = when (solverCheckGridSolved g) (printUnchecked . T.unlines $ report)
  where
    report = [toLazyText dashRow, "Step: Solved!", "Final Grid:", textShow g]

{- | use the basic solver until it fails to make progress, then try to prove what values cells cannot take. this uses
backtracking to find contradictions. this solver may actually solve the puzzle while looking for contradictions.
however, because we're searching for contradictions, such lucky guesses aren't used. instead, the solver tracks what
values cells cannot take, removes those values from those cells, and attempts to resolve using the basic solver.
candidate cells are ordered by 1) the number of available values for the cell, followed by 2) how restricted the cell's
neighborhood is, where we sum the number of values each cell in the neighborhood can take, as a proxy for restriction. a
neighborhood, here, refers to the cells that can see the cell in question -- i.e. it's row\/column\/box.

this strategy is called a "Forcing Chain". to solve every puzzle with a unique solution, we actually need to apply this
strategy recursively -- i.e. we should search for contradictions in values by calling the backtracking solver itself.
however, this is much, much slower than just checking the cheap simplifiers to see if they can produce a contradiction.
if I can find some example puzzles that require so called "Dynamic Forcing Chains", I'll consider implementing a branch
that retries the backtracking solver with itself as the function to call to find contradictions if this basic version
fails to find any solutions.

the notice printed by the solver claiming this strategy is less than logical is apparently unwarranted -- hard puzzles
require this same strategy from human solvers. the trick is to parse out when it's being applied unnecessarily.
consequently, the basic solver invests a lot of cycles into trying every other strategy in conjunction until it really
can't make any progress with any of them before giving up, and the backtracking search for contradictions begins.

this solver never guesses. it only employs logical techniques, both to find the solution to a puzzle, but more
importantly, to prove that it's correct and unique.

TODO: there's more information available from `attemptToContradict` than we're actually using at the moment. it should
      be possible to look at the `Grid`s resulting from trying each option for a cell, even when we fail to contradict
      any values. i.e. if a `Cell` can take one of two values, we can look at `Cell`s in its neighborhood to see what
      values they take in both resulting grids. if a `Cell` is marked known in each `Grid`, then it can ONLY take one of
      those two values, whatever other values we believe to be possible. consequently, we should look at such cases and
      remove impossible values from many more `Cell`s than just the single `Cell` we're updating right now. it's likely
      that we can write-in a few values for `Cell`s in such grids as we'll probably find some that take the same value,
      whatever the outcome.
-}
runBacktrackingSolver :: Sudoku Digit (Grid Digit) -> Sudoku Digit (Grid Digit)
runBacktrackingSolver mg = do
    g <- runBasicSolver =<< mg
    reportSolved g
    ensure solverCheckGridSolved g <|> do
        g' <- go (findRestrictedCells g ^.. folded . _1) g
        reportSolved g'
        ensure solverCheckGridSolved g' <|> ensure (== g) g' <|> runBacktrackingSolver (return g')
  where
    removePoss loc cs = ixCell loc . _Possibly . _CellSet %~ (A.BS.\\ cs)
    addContra loc = contradictionSearched %~ S.insert loc
    applyUpdates loc cs = removePoss loc cs . addContra loc
    -- worker that first tries to find contradictions in a `Cell`, then reruns the basic solver after updating the grid to remove
    -- values the solver was able to prove resulted in contradictions.
    go :: [CellPos] -> Grid Digit -> Sudoku Digit (Grid Digit)
    go (loc : locs) g = do
        printNoticeStart loc
        put (A.BS.empty @Word16, g) >> attemptToContradict runCheapSimplifiers loc <|> return ()
        cs <- use _1
        printNoticeEnd loc cs
        let g' = applyUpdates loc cs g
        g'' <- put (A.BS.empty @Word16, g') >> runBasicSolver g'
        ensure solverCheckGridSolved g'' <|> go locs g''
    go [] g = return g

-- | interface to the solver: run the backtracking solver and strip away the intervening monads such that we're left with an IO action.
runSolver :: SolverOptions -> Grid Digit -> IO (Grid Digit)
runSolver opts g =
    initialize
        >>> runBacktrackingSolver
        >>> observeT (A.BS.empty @Word16 @Digit, g)
        >>> flip runReaderT opts
        >>> runWriterT
        >>> fmap fst
        $ g
  where
    report g' = [toLazyText dashRow, "Step: Initial", "Starting Grid:", textShow g']
    startNotice g' = printUnchecked $ T.unlines (report g')
    initialize g' = startNotice g' >> return g'
