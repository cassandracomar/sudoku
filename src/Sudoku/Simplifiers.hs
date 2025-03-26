{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sudoku.Simplifiers where

import Control.Applicative (Alternative, (<|>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader)
import Data.Default (def)
import Data.Functor (void)
import Data.Kind (Type)
import Data.List (intersperse)
import Data.Monoid (Sum)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Lazy (Text)
import GHC.Word (Word16)
import Sudoku.Backtracking (Sudoku)
import Sudoku.Cell (Cell, CellPos, CellSet (..), Digit)
import Sudoku.Grid (
    Grid,
    SolverOptions,
    dashRow,
    ensure,
    grid,
    knownTuples,
    printChecked,
    textShow,
 )
import Sudoku.Summaries (
    Contradictions,
    LocationAlignment,
    RegionSummaries,
    SummaryRecord (..),
    Union,
    applySummary,
    boxes,
    byRegion,
    checkSolved,
    columns,
    completelySummarize,
    countedPoss,
    explainHiddenSingles,
    explainKnowns,
    explainNakedSingles,
    explainSummary,
    filterSummariesByCount,
    knowns,
    rows,
    simplifyFromDigitsAligned,
    singlePossibility,
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
import TextShow (TextShow, toLazyText)

import Data.Map.Monoidal.Strict qualified as M
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T
import Data.Vector.Generic qualified as V
import Data.Vector.Unboxed qualified as VU

data SimplifierResult s a
    = Contradicted {_simplifierOutput :: !(s a), _contradictionsExplained :: [Text], _explanations :: [Text]}
    | Successful {_simplifierOutput :: !(s a), _explanations :: [Text], _isSolved :: !Bool}

makeClassy ''SimplifierResult
makeClassyPrisms ''SimplifierResult

class (VU.Unbox (Cell a), Enum a, Ord a, TextShow a) => Simplifier f (s :: Type -> Type) a where
    -- | type family to determine the kind of `Monoid` we need for a particular simplifier
    type MonoidFor f a

    -- | apply the monoidal `RegionSummaries` to the `Grid`
    simplify :: f -> RegionSummaries (MonoidFor f a) -> s a -> s a

    -- | describe the updates the simplifier performed in this step
    explain :: (TextShow a) => f -> s a -> RegionSummaries (MonoidFor f a) -> [Text]

    -- | collect a monoidal summary for this step -- this summary will be collated into `RegionSummaries` and provided to `simplify`
    summarize :: f -> Proxy s -> Fold (CellPos, Cell a) (MonoidFor f a)

    -- | friendly description of this simplifier's algorithm
    simplifierName :: f -> Proxy s -> Proxy a -> Text

    -- | post-process the collected `RegionSummaries` -- can be used to filter summaries, e.g. to only keep those of a certain size.
    updateSummaries :: f -> s a -> RegionSummaries (MonoidFor f a) -> RegionSummaries (MonoidFor f a)
    updateSummaries _ _ = id

    -- | get the underlying `grid` `VU.Vector` from the container type
    gridLens :: f -> Lens' (s a) (VU.Vector (Cell a))
    default gridLens :: (s ~ Grid) => f -> Lens' (s a) (VU.Vector (Cell a))
    gridLens _ = grid
    {-# INLINE gridLens #-}

type CollectedSummaries f a =
    (RegionSummaries (Contradictions a), RegionSummaries (Union (CellSet a)), RegionSummaries (MonoidFor f a))

type SimplifierConstraint f (s :: Type -> Type) a =
    (Simplifier f s a, Monoid (MonoidFor f a), Ord a, Enum a, VU.IsoUnbox a Word16)

-- | split the `SummaryRecord` GADT into a tuple by unzipping `RegionSummaries` across each region.
factorSummary ::
    forall f s a.
    (SimplifierConstraint f s a) =>
    f -> Proxy s -> RegionSummaries (SummaryRecord (MonoidFor f a) a) -> CollectedSummaries f a
factorSummary _ _ summs = (mk (rs, cs, bs), mk (rs', cs', bs'), mk (rs'', cs'', bs''))
  where
    unpackSummary (SummaryRecord !contras !solvedSumms !step) = (contras, solvedSumms, step)
    (!rs, !rs', !rs'') = V.unzip3 $ V.map unpackSummary (summs ^. rows . byRegion)
    (!cs, !cs', !cs'') = V.unzip3 $ V.map unpackSummary (summs ^. columns . byRegion)
    (!bs, !bs', !bs'') = V.unzip3 $ V.map unpackSummary (summs ^. boxes . byRegion)
    mk (!r, !c, !b) = def & rows . byRegion .~ r & columns . byRegion .~ c & boxes . byRegion .~ b

-- | helper that just calls `completelySummarize` and unpacks the `SummaryRecord` GADT.
collectSummaries :: forall f (s :: Type -> Type) a. (SimplifierConstraint f s a) => f -> s a -> CollectedSummaries f a
collectSummaries f = factorSummary f (Proxy @s) . toSumms
  where
    toSumms = completelySummarize (gridLens f) . curry . foldOf $ summarize f (Proxy @s)

fullSimplifyStep :: forall f s a. (SimplifierConstraint f s a) => f -> s a -> SimplifierResult s a
fullSimplifyStep f g
    | null detectedContras = Successful g' exps (checkSolved solvedSumms)
    | otherwise = Contradicted g' detectedContras exps
  where
    (contras, solvedSumms, summs) = collectSummaries f g
    summs' = updateSummaries f g summs
    g' = simplify f summs' g
    exps = explain f g' summs'
    detectedContras = testForContradictions contras
{-# SPECIALIZE fullSimplifyStep :: SimplifyKnowns -> Grid Digit -> SimplifierResult Grid Digit #-}
{-# SPECIALIZE fullSimplifyStep :: SimplifyNakedSingles -> Grid Digit -> SimplifierResult Grid Digit #-}
{-# SPECIALIZE fullSimplifyStep :: SimplifyHiddenSingles -> Grid Digit -> SimplifierResult Grid Digit #-}
{-# SPECIALIZE fullSimplifyStep :: SimplifyCellTuples -> Grid Digit -> SimplifierResult Grid Digit #-}
{-# SPECIALIZE fullSimplifyStep :: SimplifyDigitTuples -> Grid Digit -> SimplifierResult Grid Digit #-}
{-# SPECIALIZE fullSimplifyStep :: SimplifyPointing -> Grid Digit -> SimplifierResult Grid Digit #-}

data SimplifyKnowns = SimplifyKnowns deriving (Eq)

data SimplifyNakedSingles = SimplifyNakedSingles deriving (Eq)

data SimplifyHiddenSingles = SimplifyHiddenSingles deriving (Eq)

data SimplifyCellTuples = SimplifyCellTuples deriving (Eq)

data SimplifyDigitTuples = SimplifyDigitTuples deriving (Eq)

data SimplifyPointing = SimplifyPointing deriving (Eq)

type PrintConstraint f (s :: Type -> Type) a =
    (Simplifier f s a, TextShow a, Eq a, TextShow (s a), Ord a, Enum a, VU.Unbox a, VU.Unbox (Cell a), Eq (s a))

class (PrintConstraint f s a) => PrintStep f (s :: Type -> Type) a where
    printStepImpl :: (MonadIO m, Alternative m, MonadReader SolverOptions m) => f -> s a -> SimplifierResult s a -> m ()

instance (PrintConstraint f s a) => PrintStep f s a where
    printStepImpl :: (MonadIO m, Alternative m, MonadReader SolverOptions m) => f -> s a -> SimplifierResult s a -> m ()
    printStepImpl f g res = void (ensure (== g) g') <|> printChecked (T.unlines displayLines)
      where
        stepHeader =
            [ toLazyText dashRow
            , "Step: " <> simplifierName f (Proxy @s) (Proxy @a)
            , "Starting Grid: "
            , textShow g
            , "Actions: "
            ]
        stepFooter =
            [ ""
            , ""
            , "Resulting In: "
            , textShow g'
            ]
        displayLines = stepHeader <> exps <> stepFooter
        exps = res ^. explanations
        g' = res ^. simplifierOutput
    {-# SPECIALIZE printStepImpl ::
        SimplifyNakedSingles -> Grid Digit -> SimplifierResult Grid Digit -> Sudoku Digit ()
        #-}
    {-# SPECIALIZE printStepImpl ::
        SimplifyHiddenSingles -> Grid Digit -> SimplifierResult Grid Digit -> Sudoku Digit ()
        #-}
    {-# SPECIALIZE printStepImpl ::
        SimplifyCellTuples -> Grid Digit -> SimplifierResult Grid Digit -> Sudoku Digit ()
        #-}
    {-# SPECIALIZE printStepImpl ::
        SimplifyDigitTuples -> Grid Digit -> SimplifierResult Grid Digit -> Sudoku Digit ()
        #-}
    {-# SPECIALIZE printStepImpl ::
        SimplifyPointing -> Grid Digit -> SimplifierResult Grid Digit -> Sudoku Digit ()
        #-}

instance {-# OVERLAPS #-} (PrintConstraint SimplifyKnowns s a) => PrintStep SimplifyKnowns s a where
    printStepImpl _ _ _ = return ()
    {-# SPECIALIZE printStepImpl ::
        SimplifyKnowns -> Grid Digit -> SimplifierResult Grid Digit -> Sudoku Digit ()
        #-}

data Simplify (s :: Type -> Type) a where
    Simplify :: forall f s a. (SimplifierConstraint f s a, PrintStep f s a) => !f -> (f -> s a -> SimplifierResult s a) -> Simplify s a

mkSimplify ::
    forall f (s :: Type -> Type) a. (SimplifierConstraint f s a, PrintStep f s a) => f -> Simplify s a
mkSimplify f = Simplify f fullSimplifyStep

printUnquoted :: (MonadIO m) => Text -> m ()
printUnquoted = liftIO . T.putStrLn

type ValueConstraint a = (VU.Unbox (Cell a), VU.IsoUnbox a Word16, VU.Unbox a, Enum a, Ord a, TextShow a)

instance (ValueConstraint a) => Simplifier SimplifyKnowns Grid a where
    type MonoidFor SimplifyKnowns a = Union (CellSet a)

    simplify _ = applySummary updateFromKnowns grid
    {-# SPECIALIZE simplify ::
        SimplifyKnowns
        -> RegionSummaries (MonoidFor SimplifyKnowns Digit)
        -> Grid Digit
        -> Grid Digit
        #-}
    explain _ _ = explainSummary explainKnowns
    {-# SPECIALIZE explain ::
        SimplifyKnowns -> Grid Digit -> RegionSummaries (MonoidFor SimplifyKnowns Digit) -> [Text]
        #-}
    summarize _ _ = _2 . knowns
    {-# INLINE summarize #-}
    simplifierName _ _ _ = "Remove Knowns from possible cell values"

instance (ValueConstraint a) => Simplifier SimplifyNakedSingles Grid a where
    type MonoidFor SimplifyNakedSingles a = VU.Vector (CellPos, a)

    simplify _ = applySummary updateFromNakedSingles grid
    {-# SPECIALIZE simplify ::
        SimplifyNakedSingles
        -> RegionSummaries (MonoidFor SimplifyNakedSingles Digit)
        -> Grid Digit
        -> Grid Digit
        #-}
    explain _ _ = explainSummary explainNakedSingles
    {-# SPECIALIZE explain ::
        SimplifyNakedSingles -> Grid Digit -> RegionSummaries (MonoidFor SimplifyNakedSingles Digit) -> [Text]
        #-}
    summarize _ _ = singlePossibility
    {-# INLINE summarize #-}
    simplifierName _ _ _ = "(Naked Single) Set cells with just one option to Known"

instance (ValueConstraint a) => Simplifier SimplifyHiddenSingles Grid a where
    type MonoidFor SimplifyHiddenSingles a = M.MonoidalMap a (Sum Int)
    updateSummaries _ _ = filterSummariesByCount 1
    {-# INLINE updateSummaries #-}

    simplify _ = applySummary updateFromHiddenSingles grid
    {-# SPECIALIZE simplify ::
        SimplifyHiddenSingles
        -> RegionSummaries (MonoidFor SimplifyHiddenSingles Digit)
        -> Grid Digit
        -> Grid Digit
        #-}
    explain _ _ = explainSummary explainHiddenSingles
    {-# SPECIALIZE explain ::
        SimplifyHiddenSingles -> Grid Digit -> RegionSummaries (MonoidFor SimplifyHiddenSingles Digit) -> [Text]
        #-}
    summarize _ _ = _2 . countedPoss
    {-# INLINE summarize #-}
    simplifierName _ _ _ = "(Hidden Single) Place digit with just one location"

instance (ValueConstraint a) => Simplifier SimplifyCellTuples Grid a where
    type MonoidFor SimplifyCellTuples a = CellPartitions a
    updateSummaries _ g = mergePartitionsInSummary _CellPartitions (g ^. knownTuples)
    simplify _ _ = simplifyFromCellTuples
    {-# SPECIALIZE simplify ::
        SimplifyCellTuples
        -> RegionSummaries (MonoidFor SimplifyCellTuples Digit)
        -> Grid Digit
        -> Grid Digit
        #-}
    explain _ g = explainSummary (explainPartitions g _CellPartitions)
    {-# SPECIALIZE explain ::
        SimplifyCellTuples -> Grid Digit -> RegionSummaries (MonoidFor SimplifyCellTuples Digit) -> [Text]
        #-}
    summarize _ _ = cellPartitions
    {-# INLINE summarize #-}
    simplifierName _ _ _ = "Locate tuples based on possible values of cells"

instance (ValueConstraint a) => Simplifier SimplifyDigitTuples Grid a where
    type MonoidFor SimplifyDigitTuples a = DigitPartitions a
    updateSummaries _ g = mergePartitionsInSummary _DigitPartitions (g ^. knownTuples)
    simplify _ _ = simplifyFromDigitTuples
    {-# SPECIALIZE simplify ::
        SimplifyDigitTuples
        -> RegionSummaries (MonoidFor SimplifyDigitTuples Digit)
        -> Grid Digit
        -> Grid Digit
        #-}
    explain _ g = explainSummary (explainPartitions g _DigitPartitions)
    {-# SPECIALIZE explain ::
        SimplifyDigitTuples -> Grid Digit -> RegionSummaries (MonoidFor SimplifyDigitTuples Digit) -> [Text]
        #-}
    summarize _ _ = digitPartitions
    {-# INLINE summarize #-}
    simplifierName _ _ _ = "Locate tuples based on possible locations of digits"

instance (ValueConstraint a) => Simplifier SimplifyPointing Grid a where
    type MonoidFor SimplifyPointing a = LocationAlignment a
    simplify _ = simplifyFromDigitsAligned grid
    {-# SPECIALIZE simplify ::
        SimplifyPointing
        -> RegionSummaries (MonoidFor SimplifyPointing Digit)
        -> Grid Digit
        -> Grid Digit
        #-}
    explain _ _ _ = ["Looked for Pointing"]
    {-# SPECIALIZE explain ::
        SimplifyPointing -> Grid Digit -> RegionSummaries (MonoidFor SimplifyPointing Digit) -> [Text]
        #-}
    summarize _ _ = valueAlignment
    {-# INLINE summarize #-}
    simplifierName _ _ _ = "(Pointing) Remove possibilities by locating alignment of values"

cheapSimplifiers :: [[Simplify Grid Digit]]
cheapSimplifiers =
    [ [mkSimplify SimplifyHiddenSingles]
    , [mkSimplify SimplifyNakedSingles]
    ]

expensiveSimplifiers :: [[Simplify Grid Digit]]
expensiveSimplifiers =
    [ [mkSimplify SimplifyPointing]
    , [mkSimplify SimplifyCellTuples]
    , [mkSimplify SimplifyDigitTuples]
    ]

interleavedSteps :: [Simplify Grid Digit]
interleavedSteps = [mkSimplify SimplifyKnowns]

orderSimplifiers :: [[Simplify Grid Digit]] -> [Simplify Grid Digit]
orderSimplifiers = (interleavedSteps ++) . join . intersperse interleavedSteps

runSimplifierPure :: Simplify Grid Digit -> Grid Digit -> SimplifierResult Grid Digit
runSimplifierPure (Simplify f simplifier) = simplifier f
