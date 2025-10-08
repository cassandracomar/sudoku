{-# LANGUAGE UndecidableInstances #-}

module Sudoku.Simplifiers where

import Control.Applicative (Alternative, (<|>))
import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader)
import Data.Functor (void)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Monoid (Sum)
import Data.Text.Lazy (Text)
import Data.Word (Word16)
import Foreign (Word8)
import Sudoku.Cell
import Sudoku.Grid (
    Grid,
    SolverOptions,
    dashRow,
    ensure,
    grid,
    knownTuples,
    printVerbose,
 )
import Sudoku.Summaries (
    ContradictionDesc,
    ExplainDesc (..),
    LocationAlignment,
    RegionSummaries,
    Summary,
    Union,
    ValueConstraint,
    applySummary,
    checkSolved,
    countedPoss,
    explainHiddenSingles,
    explainKnowns,
    explainNakedSingles,
    explainSummary,
    filterSummariesByCount,
    knowns,
    simplifyFromDigitsAligned,
    singlePossibility,
    summarizeWithContradictions,
    testForContradictions,
    updateFromHiddenSingles,
    updateFromKnowns,
    updateFromNakedSingles,
    valueAlignment,
 )
import Sudoku.Tuples (
    CellPartitions,
    DigitPartitions,
    cellPartitions,
    digitPartitions,
    explainPartitions,
    mergePartitionsInSummary,
    simplifyFromCellTuples,
    simplifyFromDigitTuples,
    _CellPartitions,
    _DigitPartitions,
 )
import TextShow (Builder, TextShow (showb), toLazyText, unlinesB)

import Data.IntMap.Monoidal.Strict qualified as M
import Data.Text.Lazy.IO qualified as T
import Data.Vector.Unboxed qualified as VU
import qualified Data.HashSet as HS

data SimplifierResult s a
    = Contradicted
        { _simplifierOutput :: !(s a)
        , _contradictionDescs :: !(HS.HashSet (ContradictionDesc Int a))
        , _explanations :: [ExplainDesc a]
        }
    | Successful {_simplifierOutput :: !(s a), _explanations :: [ExplainDesc a], _isSolvedRes :: !Bool}

makeClassy ''SimplifierResult
makeClassyPrisms ''SimplifierResult

class (VU.Unbox a, Enum a, Ord a, TextShow a) => Simplifier f (s :: Type -> Type) a where
    -- | type family to determine the kind of `Monoid` we need for a particular simplifier
    type MonoidFor f a

    -- | apply the monoidal `RegionSummaries` to the `Grid`
    simplify :: f -> RegionSummaries (MonoidFor f a) -> s a -> s a

    -- | describe the updates the simplifier performed in this step
    explain :: (TextShow a) => f -> s a -> RegionSummaries (MonoidFor f a) -> [ExplainDesc a]

    -- | collect a monoidal summary for this step -- this summary will be collated into `RegionSummaries` and provided to `simplify`
    summarize :: f -> Fold (CellPos, Cell a) (RegionIndicator -> Word8 -> MonoidFor f a)

    -- | friendly description of this simplifier's algorithm
    simplifierName :: f -> Builder

    -- | post-process the collected `RegionSummaries` -- can be used to filter summaries, e.g. to only keep those of a certain size.
    updateSummaries :: f -> s a -> RegionSummaries (MonoidFor f a) -> RegionSummaries (MonoidFor f a)
    updateSummaries _ _ = id
    {-# INLINE updateSummaries #-}

    -- | get the underlying `grid` `VU.Vector` from the container type
    gridLens :: f -> Lens' (s a) (VU.Vector (Cell a))
    default gridLens :: (s ~ Grid) => f -> Lens' (s a) (VU.Vector (Cell a))
    gridLens _ = grid
    {-# INLINE gridLens #-}

data SimplifyKnowns = SimplifyKnowns deriving (Eq)

data SimplifyNakedSingles = SimplifyNakedSingles deriving (Eq)

data SimplifyHiddenSingles = SimplifyHiddenSingles deriving (Eq)

data SimplifyCellTuples = SimplifyCellTuples deriving (Eq)

data SimplifyDigitTuples = SimplifyDigitTuples deriving (Eq)

data SimplifyPointing = SimplifyPointing deriving (Eq)

type PrintConstraint f (s :: Type -> Type) a =
    (TextShow a, Eq a, TextShow (s a), Ord a, Enum a, VU.Unbox a, Eq (s a))

class (PrintConstraint f s a) => PrintStep f (s :: Type -> Type) a where
    getSimplifierName :: f -> Builder

    printStepImpl ::
        (MonadIO m, Alternative m, MonadReader SolverOptions m, Simplifier f s a) => f -> s a -> SimplifierResult s a -> m ()
    printStepImpl f g res = void (ensure (== g) g') <|> (printVerbose . toLazyText . unlinesB) step
      where
        stepHeader =
            [ dashRow
            , "Step: " <> getSimplifierName @_ @s @a f
            , "Starting Grid: "
            , showb g
            , "Actions: "
            ]
        stepFooter =
            [ ""
            , ""
            , "Resulting In: "
            , showb g'
            ]
        step = stepHeader <> fmap showb exps <> stepFooter
        exps = res ^. explanations
        g' = res ^. simplifierOutput

instance {-# OVERLAPPABLE #-} (PrintConstraint f s a, Simplifier f s a) => PrintStep f s a where
    getSimplifierName = simplifierName @f @s @a

instance (PrintConstraint SimplifyKnowns s a, Simplifier SimplifyKnowns s a) => PrintStep SimplifyKnowns s a where
    getSimplifierName = simplifierName @SimplifyKnowns @s @a
    printStepImpl _ _ _ = return ()

type SimplifierConstraint f (s :: Type -> Type) a = (Simplifier f s a, Monoid (MonoidFor f a), ValueConstraint a)

{- | Existential wrapper holding a simplifier. provides type-directed dispatch to the execution function for a simplifier along with a token
  to allow dispatch to other functions that need to know the type of the currently executing simplifier.
-}
data Simplify (s :: Type -> Type) a where
    Simplify ::
        forall f s a.
        (SimplifierConstraint f s a, PrintStep f s a) =>
        { _simplifierHandle :: f
        {- ^ can't actually use this function as it leads to escape of the existential.
        provides a handle for dispatching simplifier-based effects. use via pattern-matching.
        -}
        , simplifier :: s a -> SimplifierResult s a
        -- ^ execution function for the wrapped simplifier
        }
        -> Simplify s a

mkSimplify :: (SimplifierConstraint f s a, PrintStep f s a) => f -> Simplify s a
mkSimplify = Simplify <*> fullSimplifyStep
{-# INLINE mkSimplify #-}

collectSummaries ::
    forall f (s :: Type -> Type) a. (SimplifierConstraint f s a) => f -> s a -> Summary (MonoidFor f a) a
collectSummaries f = summarizeWithContradictions (gridLens f . cells) (summarize @_ @s f)

fullSimplifyStep :: forall f s a. (SimplifierConstraint f s a) => f -> s a -> SimplifierResult s a
fullSimplifyStep f g
    | null detectedContras = Successful g' exps isSolved
    | otherwise = Contradicted g' detectedContras exps
  where
    (!contras, !solvedSumms, !summs) = collectSummaries f g
    !summs' = updateSummaries f g summs
    !g' = simplify f summs' g
    exps = explain f g' summs'
    !detectedContras = testForContradictions contras
    !isSolved = checkSolved solvedSumms

instance (ValueConstraint a) => Simplifier SimplifyKnowns Grid a where
    type MonoidFor SimplifyKnowns a = Union (CellSet a)

    simplify _ = applySummary updateFromKnowns grid
    {-# INLINE simplify #-}
    explain _ _ = explainSummary explainKnowns
    {-# NOINLINE explain #-}
    summarize _ = knowns
    {-# INLINE summarize #-}
    simplifierName _ = "Remove Knowns from possible cell values"

instance (ValueConstraint a) => Simplifier SimplifyNakedSingles Grid a where
    type MonoidFor SimplifyNakedSingles a = Union (CellSet a)

    simplify _ = applySummary updateFromNakedSingles grid
    {-# INLINE simplify #-}
    explain _ _ = explainSummary explainNakedSingles
    {-# NOINLINE explain #-}
    summarize _ = singlePossibility
    {-# INLINE summarize #-}
    simplifierName _ = "(Naked Single) Set cells with just one option to Known"

instance (ValueConstraint a) => Simplifier SimplifyHiddenSingles Grid a where
    type MonoidFor SimplifyHiddenSingles a = M.MonoidalIntMap (Sum Word16)
    updateSummaries _ _ = filterSummariesByCount 1
    {-# INLINE updateSummaries #-}

    simplify _ = applySummary updateFromHiddenSingles grid
    {-# INLINE simplify #-}
    explain _ _ = explainSummary explainHiddenSingles
    {-# NOINLINE explain #-}
    summarize _ = countedPoss
    {-# INLINE summarize #-}
    simplifierName _ = "(Hidden Single) Place digit with just one location"

instance (ValueConstraint a) => Simplifier SimplifyPointing Grid a where
    type MonoidFor SimplifyPointing a = LocationAlignment a
    simplify _ = simplifyFromDigitsAligned grid
    {-# INLINE simplify #-}
    explain _ _ _ = [LookedForPointing]
    summarize _ = valueAlignment
    {-# INLINE summarize #-}
    simplifierName _ = "(Pointing) Remove possibilities by locating alignment of values"

instance (ValueConstraint a) => Simplifier SimplifyCellTuples Grid a where
    type MonoidFor SimplifyCellTuples a = CellPartitions a
    updateSummaries _ g = mergePartitionsInSummary _CellPartitions (g ^. knownTuples)
    {-# INLINE updateSummaries #-}
    simplify _ _ = simplifyFromCellTuples
    {-# INLINE simplify #-}
    explain _ g = explainSummary (explainPartitions g _CellPartitions)
    {-# NOINLINE explain #-}
    summarize _ = cellPartitions
    {-# INLINE summarize #-}
    simplifierName _ = "Locate tuples based on possible values of cells"

instance (ValueConstraint a) => Simplifier SimplifyDigitTuples Grid a where
    type MonoidFor SimplifyDigitTuples a = DigitPartitions a
    updateSummaries _ g = mergePartitionsInSummary _DigitPartitions (g ^. knownTuples)
    {-# INLINE updateSummaries #-}
    simplify _ _ = simplifyFromDigitTuples
    {-# INLINE simplify #-}
    explain _ g = explainSummary (explainPartitions g _DigitPartitions)
    {-# NOINLINE explain #-}
    summarize _ = digitPartitions
    {-# INLINE summarize #-}
    simplifierName _ = "Locate tuples based on possible locations of digits"

printUnquoted :: (MonadIO m) => Text -> m ()
printUnquoted = liftIO . T.putStrLn

cheapSimplifiers :: (ValueConstraint a) => [[Simplify Grid a]]
cheapSimplifiers =
    [ [mkSimplify SimplifyHiddenSingles]
    , [mkSimplify SimplifyNakedSingles]
    ]
{-# SPECIALIZE cheapSimplifiers :: [[Simplify Grid Digit]] #-}

expensiveSimplifiers :: (ValueConstraint a) => [[Simplify Grid a]]
expensiveSimplifiers =
    [ [mkSimplify SimplifyPointing]
    , [mkSimplify SimplifyCellTuples]
    , [mkSimplify SimplifyDigitTuples]
    ]
{-# SPECIALIZE expensiveSimplifiers :: [[Simplify Grid Digit]] #-}

interleavedSteps :: (ValueConstraint a) => [Simplify Grid a]
interleavedSteps = [mkSimplify SimplifyKnowns]
{-# SPECIALIZE interleavedSteps :: [Simplify Grid Digit] #-}

orderSimplifiers :: (ValueConstraint a) => [[Simplify Grid a]] -> [Simplify Grid a]
orderSimplifiers = (interleavedSteps <>) . intercalate interleavedSteps
{-# SPECIALIZE orderSimplifiers :: [[Simplify Grid Digit]] -> [Simplify Grid Digit] #-}

-- | run the provided simplifier on the given `Grid`
runSimplifierPure :: (ValueConstraint a) => Simplify Grid a -> Grid a -> SimplifierResult Grid a
runSimplifierPure = simplifier
{-# SPECIALIZE runSimplifierPure :: Simplify Grid Digit -> Grid Digit -> SimplifierResult Grid Digit #-}
