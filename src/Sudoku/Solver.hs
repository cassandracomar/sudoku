module Sudoku.Solver where

import Control.Applicative (Alternative (..), (<**>))
import Control.Lens
import Control.Monad (foldM, guard, unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.RWS.Lazy (MonadState (put))
import Data.Aeson (eitherDecodeStrict')
import Data.Function (on)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Sudoku.Backtracking (Contradicted, Sudoku, attemptToContradict, findRestrictedCells, runSudokuT)
import Sudoku.Cell
import Sudoku.Grid
import Sudoku.Simplifiers
import Sudoku.Summaries (ValueConstraint)
import TextShow as TB (Builder, TextShow (showb, showbList), toLazyText, unlinesB)

import Data.ByteString qualified as BS
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU
import Data.Word16Set qualified as A.BS

type SolverConstraints a = (Show a, Ord a, Enum a, VU.Unbox a, VU.Unbox a)

printStep :: (SolverMonad m) => Simplify s a -> s a -> SimplifierResult s a -> m ()
printStep (Simplify f _) = printStepImpl f
{-# INLINE printStep #-}

printContradictions :: (SolverMonad m) => [TB.Builder] -> m ()
printContradictions contras = printVerbose . toLazyText $ unlinesB ("Found Contradictions: " : contras)
{-# INLINE printContradictions #-}

{- |
this action returns a grid if the solver has failed to make progress. the solver cannot make progress if:
  1. the grid is already solved
  2. the output of the last simplifier is the same as it's input
-}
cannotProgress :: (SolverMonad m, Eq a) => Grid a -> Grid a -> m (Grid a)
cannotProgress g g' = alreadySolved g <|> noProgress g g'
{-# INLINE cannotProgress #-}

{- |
this action yields the input grid if it's already solved and `mzero` if it is not.
-}
alreadySolved :: (SolverMonad m) => Grid a -> m (Grid a)
alreadySolved = ensure (view isSolved)
{-# INLINE alreadySolved #-}

{- |
this action yields the input grid if the grid pre- and post- simplifier execution are identical.
-}
noProgress :: (SolverMonad m, Eq a) => Grid a -> Grid a -> m (Grid a)
noProgress = ensure . gEq
  where
    gEq = (==) `on` view grid
{-# INLINE noProgress #-}

runSimplifierOnce :: forall m a. (SolverMonad m, ValueConstraint a) => Simplify Grid a -> Grid a -> m (Grid a)
runSimplifierOnce !f !g =
    cannotProgress g g' <|> do
        when (null contras) $ printStep f g res
        unless (null contras) $ printContradictions (contras ^.. folded . to showb)
        guard (null contras) $> g'
  where
    !res = runSimplifierPure f g
    !contras = res ^. contradictionDescs
    !solveCheck = fromMaybe False $ res ^? isSolvedRes
    !g' = res ^. simplifierOutput & isSolved .~ solveCheck
{-# INLINE runSimplifierOnce #-}

runCheapSimplifier :: forall m a. (SolverMonad m, ValueConstraint a) => Simplify Grid a -> Grid a -> m (Grid a)
runCheapSimplifier !f !g = alreadySolved g <|> runSimplifierOnce f g
{-# INLINE runCheapSimplifier #-}

{- | run each basic strategy in sequence, interleaving strategies that should be run in between every pair of steps.
i.e. if we've marked some cells as `Known`, we should make sure that we remove impossible values from cells before
running the next simplifier.
-}
runCheapSimplifiers :: forall m a. (SolverMonad m, ValueConstraint a) => m (Grid a) -> m (Grid a)
runCheapSimplifiers = fix $ \rec mg -> do
    g <- mg
    g' <- foldM (flip runCheapSimplifier) g cheapSimplifiers
    cannotProgress g g' <|> rec (return g')
{-# SPECIALIZE runCheapSimplifiers :: Sudoku Digit (Grid Digit) -> Sudoku Digit (Grid Digit) #-}

runExpensiveSimplifier :: forall m a. (SolverMonad m, ValueConstraint a) => Simplify Grid a -> Grid a -> m (Grid a)
runExpensiveSimplifier !f !g =
    alreadySolved g <|> do
        g' <- runSimplifierOnce f g
        noProgress g g' <|> runCheapSimplifiers (return g')
{-# INLINE runExpensiveSimplifier #-}

-- | run each expensive strategy in sequence, interleaving strategies that should be run in between every pair of steps.
runExpensiveSimplifiers :: forall m a. (SolverMonad m, ValueConstraint a) => m (Grid a) -> m (Grid a)
runExpensiveSimplifiers = fix $ \rec mg -> do
    g <- mg
    g' <- alreadySolved g <|> foldM (flip runExpensiveSimplifier) g expensiveSimplifiers
    cannotProgress g g' <|> rec (return g')
{-# SPECIALIZE runExpensiveSimplifiers :: Sudoku Digit (Grid Digit) -> Sudoku Digit (Grid Digit) #-}

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
    guard (view isSolved g')

{- |
run the basic strategies for solving sudoku puzzles:
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
runBasicSolver :: (SolverMonad m, ValueConstraint a) => Grid a -> m (Grid a)
runBasicSolver g = do
    g' <- runCheapSimplifiers (return g)
    cannotProgress g g' <|> runExpensiveSimplifiers (return g')
{-# SPECIALIZE runBasicSolver :: Grid Digit -> Sudoku Digit (Grid Digit) #-}

printNotice :: (SolverMonad m) => [Builder] -> m ()
printNotice ss = printVerbose . toLazyText $ unlinesB report
  where
    reportHeader = [dashRow, "NOTICE:"]
    reportFooter = [dashRow]
    report = reportHeader <> ss <> reportFooter
{-# INLINE printNotice #-}

printNoticeStart :: (SolverMonad m) => CellPos -> m ()
printNoticeStart loc =
    printNotice
        [ "using the backtracking solver to search for impossible values in " <> showLocB loc
        , "another attempt to solve logically will be made after we've eliminated impossibilities from this cell."
        ]
{-# INLINE printNoticeStart #-}

printNoticeEnd :: (SolverMonad m, ValueConstraint a) => CellPos -> Contradicted a -> m ()
printNoticeEnd loc cs =
    printNotice
        [ "resuming logical solving with the knowledge that " <> showLocB loc <> " cannot be " <> showbList (A.BS.toList cs)
        ]
{-# INLINE printNoticeEnd #-}

reportIfSolved :: (SolverMonad m, ValueConstraint a) => Grid a -> m ()
reportIfSolved g = when (view isSolved g) (printQuiet . toLazyText . unlinesB $ report)
  where
    report = [dashRow, "Step: Solved!", "Final Grid:", showb g]
{-# INLINE reportIfSolved #-}

{- |
use the basic solver until it fails to make progress, then try to prove what values cells cannot take. this uses
backtracking to find contradictions. this solver may actually solve the puzzle while looking for contradictions.
however, because we're searching for contradictions, such lucky guesses aren't used. instead, the solver tracks what
values cells cannot take, removes those values from those cells, and attempts to resolve using the basic solver.
candidate cells are ordered by 1) the number of available values for the cell, followed by 2) how restricted the cell's
neighborhood is, where we sum the number of values each cell in the neighborhood can take, as a proxy for restriction. a
neighborhood, here, refers to the cells that can see the cell in question -- i.e. it's `Row`\/`Column`\/`Box`.

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
runBacktrackingSolver :: (ValueConstraint a) => Sudoku a (Grid a) -> Sudoku a (Grid a)
runBacktrackingSolver mg = do
    g <- runBasicSolver =<< mg
    reportIfSolved g
    alreadySolved g <|> do
        g' <- go (findRestrictedCells g ^.. folded . _1) g
        reportIfSolved g'
        cannotProgress g g' <|> runBacktrackingSolver (return g')
  where
    removePoss loc cs = ix loc . _Possibly . _CellSet %~ (A.BS.\\ cs)
    addContra loc = contradictionSearched %~ S.insert loc
    applyUpdates loc cs = removePoss loc cs . addContra loc
    -- worker that first tries to find contradictions in a `Cell`, then reruns the basic solver after updating the grid to remove
    -- values the solver was able to prove resulted in contradictions.
    go :: (ValueConstraint a) => [CellPos] -> Grid a -> Sudoku a (Grid a)
    go (loc : locs) g = do
        printNoticeStart loc
        put (A.BS.empty, g) >> attemptToContradict runCheapSimplifiers loc <|> return ()
        cs <- use _1
        printNoticeEnd loc cs
        let g' = applyUpdates loc cs g
        g'' <- put (A.BS.empty, g') >> runBasicSolver g'
        alreadySolved g'' <|> go locs g''
    go [] g = return g
{-# SPECIALIZE runBacktrackingSolver :: Sudoku Digit (Grid Digit) -> Sudoku Digit (Grid Digit) #-}

runBacktrackingSolver' :: (ValueConstraint a) => Grid a -> Sudoku a (Grid a)
runBacktrackingSolver' = runBacktrackingSolver . pure
{-# SPECIALIZE runBacktrackingSolver' :: Grid Digit -> Sudoku Digit (Grid Digit) #-}

displayStartNotice :: (ValueConstraint a) => Grid a -> Sudoku a ()
displayStartNotice = printQuiet . toLazyText . unlinesB . report
  where
    report g = [dashRow, "Step: Initial", "Starting Grid:", showb g]
{-# INLINE displayStartNotice #-}

-- | interface to the solver: run the backtracking solver and strip away the intervening monads such that we're left with an `IO` action.
runSolver :: SolverOptions -> Grid Digit -> IO (Grid Digit)
runSolver opts = (*>) <$> displayStartNotice <*> runBacktrackingSolver' <**> runSudokuT opts
