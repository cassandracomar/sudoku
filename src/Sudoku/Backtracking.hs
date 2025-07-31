{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Sudoku.Backtracking where

import Control.Applicative (Alternative (..))
import Control.Category ((>>>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.LogicState (LogicStateT, MonadLogic (interleave, lnot, (>>-)), MonadLogicState (backtrack))
import Control.Monad.RWS.Lazy (MonadState, MonadWriter (tell))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Lazy (WriterT (runWriterT))
import Control.Monad.TransLogicState.Class (observeT)
import Data.List (sortBy)
import Data.Monoid (Sum (Sum))
import Data.Word (Word16)
import Sudoku.Cell
import Sudoku.Grid
import Sudoku.Summaries ((>>>>))

import Data.BitSet qualified as A.BS
import Data.Set qualified as S
import Data.Vector.Unboxed qualified as VU

type Contradicted a = A.BS.BitSet Word16 a

type SudokuT m a r = LogicStateT (Contradicted a) (Grid a) m r

type Sudoku a r = SudokuT (ReaderT SolverOptions (WriterT (BacktrackStateLog a) IO)) a r

data BacktrackState a = BacktrackState {_bs :: !(Grid a), _location :: !CellPos, _contradicted :: !a}

type BacktrackStateLog a = [BacktrackState a]

{- | strip the monad wrappers to yield back the base monad. takes the options used to run the solver and the initial grid as `Reader`
arguments.
-}
runSudokuT ::
    forall a m.
    (Monad m, MonadFail m) =>
    SolverOptions -> Grid a -> SudokuT (ReaderT SolverOptions (WriterT (BacktrackStateLog a) m)) a (Grid a) -> m (Grid a)
runSudokuT opts = (A.BS.empty @Word16 @a,) >>> observeT >>>> flip runReaderT opts >>> runWriterT >>> fmap fst
{-# INLINE runSudokuT #-}

makeLenses ''BacktrackState

-- | the neighborhood of a `Cell` is it's `Row`\/`Column`\/`Box`
neighborhoodOf :: (Ord a, VU.Unbox (Cell a)) => CellPos -> [SudokuSetTraversal a]
neighborhoodOf (r, c, b) = [rowAt r, colAt c, boxAt b]
{-# INLINE neighborhoodOf #-}

-- | a `Cell`'s degree of `Restriction` is the number of possible values it can take.
restrictionScoreCell :: (Enum a) => Cell a -> Int
restrictionScoreCell = lengthOf (_Possibly . _CellSet . bsfolded)
{-# INLINE restrictionScoreCell #-}

{- | `Traversal`s that pass through a given `Cell` form it's neighborhood -- that is, it's row/column/box. calculate the score for
each `Traversal` and sum. the `Restriction` score along a `Traversal` is the sum of the `Restriction` scores for each cell along it.
-}
restrictionScoreNeighborhood :: (Ord a, Enum a, VU.Unbox (Cell a)) => Grid a -> CellPos -> Sum Int
restrictionScoreNeighborhood g loc = foldMap restrictionScoreTraversal (neighborhoodOf loc)
  where
    restrictionScoreTraversal l = foldMapOf (runIndexedTraversal l) (Sum . restrictionScoreCell) g
{-# INLINE restrictionScoreNeighborhood #-}

{- | an `Eq`/`Ord` container to track restriction scores for `Cell`s, ensuring `Cell`s are compared first by their own
degree of `Restriction`. among `Cell`s that have the same degree of internal `Restriction`, we order them by
the degree of `Restriction` in their neighborhood.
-}
data Restriction = Restriction {cellScore :: !(Sum Int), neighborhoodScore :: !(Sum Int)} deriving (Eq, Ord)

restrictionScore :: (Ord a, Enum a, VU.Unbox (Cell a)) => Grid a -> (CellPos, Cell a) -> Restriction
restrictionScore g (loc, cell) = Restriction (Sum (restrictionScoreCell cell)) (restrictionScoreNeighborhood g loc)
{-# INLINE restrictionScore #-}

-- | a `Restriction`-ordered listing of `Cell`s
findRestrictedCells :: (Ord a, Enum a, VU.Unbox (Cell a)) => Grid a -> [(CellPos, Cell a)]
findRestrictedCells g =
    sortBy restrictionOrdering $
        g ^@.. grid . cellPos . indices (not . flip S.member (_contradictionSearched g)) . filtered (has _Possibly)
  where
    restrictionOrdering a b = restrictionScore g a `compare` restrictionScore g b
{-# INLINE findRestrictedCells #-}

choosePossible ::
    (Alternative m, MonadLogic m, MonadState (s, Grid a) m, VU.Unbox (Cell a), Enum a, VU.IsoUnbox a Word16) =>
    CellPos -> m (Cell a)
choosePossible loc =
    use (_2 . singular (ix loc))
        >>= foldrOf (_Possibly . _CellSet . bsfolded) (view (re _Known . to pure . to interleave)) empty
{-# SPECIALIZE INLINE choosePossible :: CellPos -> Sudoku Digit (Cell Digit) #-}

{- | search for values a `Cell` cannot take. it's completely possible that the solver accidentally solves the puzzle while
trying values for `Cell`s while trying to figure out what definitely doesn't work and why. however, because we're searching
for values that cause the solver to fail, we necessarily have to discard successful results. see the type of `lnot`
to understand why.

we primarily use the `LogicStateT` monad in this function, moreso than anywhere else. `Grid` state is used as the backtracking half
of the state while we track values we've contradicted in `loc` in the global state component. the former is used within this
function and the latter is used to update the `Grid` backtracking began on.
-}
attemptToContradict ::
    (Monad m, MonadWriter (BacktrackStateLog a) m, Eq a, Enum a, VU.Unbox (Cell a), Ord a, VU.IsoUnbox a Word16) =>
    (SudokuT m a (Grid a) -> SudokuT m a (Grid a)) -> CellPos -> SudokuT m a ()
attemptToContradict act loc =
    choosePossible loc >>- \cell -> join . backtrack $ do
        _2 . ix loc .= cell
        g <- use _2
        lnot $ act (return g)
        _1 %= A.BS.insert (d cell)
        lift $ tell [BacktrackState g loc (d cell)]
        _2 . ix loc . _Possibly . _CellSet %= A.BS.delete (d cell)
  where
    d cell = cell ^. singular _Known
{-# SPECIALIZE INLINE attemptToContradict ::
    (Sudoku Digit (Grid Digit) -> Sudoku Digit (Grid Digit)) -> CellPos -> Sudoku Digit ()
    #-}
