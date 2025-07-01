{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-overlapping-patterns #-}

module Sudoku.Accelerate.Summaries where

import Control.Category ((>>>))
import Control.Lens
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Control.Lens.Each ()
import Data.Array.Accelerate.Control.Lens.Shape ()
import Data.Array.Accelerate.Control.Lens.Tuple ()
import Data.Array.Accelerate.Data.Fold as A
import Data.Array.Accelerate.Data.Monoid (Sum (..), pattern Sum_)
import Data.Array.Accelerate.IO.Data.Vector.Generic ()
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Sudoku.Cell (
    Cell,
    CellPos,
    CellSet,
    Digit,
    RegionIndicator,
    boxIndex_,
    boxNumber_,
    rowColumn_,
    vindex_,
    pattern Box_,
    pattern CellSet_,
    pattern Column_,
    pattern KnownRepr_,
    pattern Possibly_,
    pattern Row_,
 )
import Sudoku.Summaries (
    ContradictionDesc,
    ExplainDesc,
    SummaryU,
    Union,
    summuSingleton_,
    pattern AlreadyKnown_,
    pattern CannotPlace_,
    pattern DigitRepeats_,
    pattern NoFillForCell_,
    pattern SharedCell_,
    pattern SingleLoc_,
    pattern SinglePoss_,
    pattern Union_,
 )
import TextShow (Builder)
import Prelude (Applicative (..), Monoid (..), Semigroup (..), String, id, (<$>), (<*>))

import Data.Accelerate.BitSet qualified as BS
import Data.Array.Accelerate.Data.Maybe qualified as A
import Data.BitSet qualified as BS
import Data.Vector.Unboxed qualified as VU
import Prelude qualified as P

{-# ANN module ("HLint: ignore Use guards" :: String) #-}

{-# ANN module ("HLint: ignore Use section" :: String) #-}

newtype First a = First {runFirst :: Maybe a}
    deriving (Generic)

instance (Elt a) => Elt (First a)

mkPattern ''First

instance (Elt a) => Semigroup (Exp (First a)) where
    (<>) = match \case
        First_ (Just_ m) -> const (First_ (Just_ m))
        First_ Nothing_ -> id
    {-# INLINE (<>) #-}

instance (Elt a) => Monoid (Exp (First a)) where
    mempty = First_ Nothing_
    {-# INLINE mempty #-}

getFirst_ :: (Elt a) => Exp (First a) -> Exp (Maybe a)
getFirst_ = match \case
    First_ a -> a
{-# INLINE getFirst_ #-}

-- aliases for clarity on what all the `Int`s are referring to

-- | `Row`: `0`, `Column`: `1`, `Box`: `2`
type Region = Int

type RegionMajorIx = Int

type RegionMinorIx = Int

type DigitIx = Int

type RowIx_ = Int

type ColIx_ = Int

-- | `DIM2`
type GridSh = Z :. RowIx_ :. ColIx_

-- | `DIM3`
type GridRegions = Z :. Region :. RowIx_ :. ColIx_

-- | `DIM2`
type RowCol = Z :. Region :. RegionMajorIx

-- | `DIM3` when applied to `Compact`
type ByDigit sh = sh :. DigitIx

-- | `DIM4`
type UnfoldedGrid = ByDigit RowCol :. RegionMinorIx

type GridUnfolded m = Array UnfoldedGrid m

type RegionSummaries' m = Array Summs m

type AccSummaries m = Acc (RegionSummaries' m)

-- | `DIM2`
type Summs = Z :. Region :. RegionMajorIx

cellPos_ :: Iso' (Exp Int) (Exp CellPos)
cellPos_ = iso rowColumn_ vindex_
{-# INLINE cellPos_ #-}

cellIx :: Iso' (Exp RowCol) (Exp CellPos)
cellIx = iso rowColToCellPos cellPosToRowCol
{-# INLINE cellIx #-}

integral_ :: (A.Integral a, A.Integral b, FromIntegral a b, FromIntegral b a) => Iso' (Exp a) (Exp b)
integral_ = iso A.fromIntegral A.fromIntegral
{-# INLINE integral_ #-}

majorMinor_ :: Exp GridRegions -> Exp GridRegions
majorMinor_ = match \case
    (I3 ri row col) ->
        BS.fromRepr ri & match \case
            Row_ -> I3 0 row col
            Column_ -> I3 1 col row
            Box_ -> I3 2 (boxNumber_ (T2 row col)) (boxIndex_ (T3 row col (boxNumber_ (T2 row col))))

rowColToCellPos :: Exp RowCol -> Exp CellPos
rowColToCellPos = match \case
    (I2 row col) -> T3 r c (boxNumber_ (T2 r c))
      where
        r = A.fromIntegral row
        c = A.fromIntegral col

cellPosToRowCol :: Exp CellPos -> Exp RowCol
cellPosToRowCol = match \case
    (T3 r c _) -> I2 (r ^. integral_) (c ^. integral_)

boxIxToRowCol_ :: (A.Integral a) => Exp a -> Exp a -> Exp (a, a)
boxIxToRowCol_ box boxIdx = T2 row col
  where
    row = box `div` 3 * 3 + boxIdx `div` 3
    col = (box `rem` 3) * 3 + boxIdx `rem` 3

fromMajorMinor_ :: Exp GridRegions -> Exp GridRegions
fromMajorMinor_ = match \case
    (I3 ri row col) ->
        BS.toRepr ri & match \case
            Row_ -> I3 0 row col
            Column_ -> I3 1 col row
            Box_ -> I3 2 row' col'
              where
                T2 row' col' = boxIxToRowCol_ row col

{- | replicate the `Grid` for each `Region` and again for each possible value `Cell`s can take. this opens the most
opportunity for auto-vectorization when folding the `Summaries` together and it enables parallelism during the fold and
apply steps.

in total, this creates 27 copies of the `Grid`. this subsequently gets folded down to just 27 entries in two 9x reductions.
an optimization to reduce the number of empty summaries getting folded together (mostly bitwise `or`s) would be to parameterize
the shape of the unfolded grid, such that each simplifier only creates as many copies as it actually needs.

mass copying, though, is how we avoid nested data parallelism, which Acclerate does not support. this 9x9 inner loop exists
in the non-Accelerate version of the solver as well -- it just doesn't allocate 81 copies of each `Cell`. because this
function uses `backpermute` to associate the right indices when folding, that allocation can't be fused away.

but to put this in perspective, the total allocation for the unfolded array is 162 bytes, for a given summary type, and we
collect ~5 different Summaries when folding the `Grid`, for a grand total, of 810 bytes allocated on each simplification step.
the entire dataset will never leave the CPU's L1 cache by 1-2 orders of magnitude. a good chunk will never leave the SIMD registers.

this is what Accelerate buys the solver. in the vanilla Haskell version, the allocations were on the order of 1M resident in
the heap, and the program spends most of it's time chasing pointer indirections. by stuffing all the allocations into one
right-sized byte array, we ensure good cache locality, enable auto-vectorization, and enable the CPU to prefetch the data in large
chunks all at once. so despite parallelism not being very important for a Sudoku solver (after all, each logical `Simplifier` step
must follow the one previous -- except when backtracking to search for contradictions), we still benefit greatly.

this transposes the last two indices (`Digit` value and `Region` minor index, respectively) to allow folding first across
a `Row`/`Column`/`Box` and then to compile the summary for each `Digit` afterwards.

after this function runs, we go from `Z:.Row:.Column` -> `Z:.RegionType:.RegionMajorIx:.Digit:.RegionMinorIx`. the major index
is the `Row`/`Column`/`Box` number and the minor index counts the entries in that region. so in a `Row` major organization of
the `Grid`, the minor index is the column while in `Column` major, it's the `Row`. counting the positions within a `Box` is
a bit weird, see `boxIndex`. `Region` is purely numeric. `0` means `Row`, `1` means `Column`, and `2` means `Box`.
-}
unfoldGrid :: forall v. (Elt v) => GridArr v -> Acc (GridUnfolded v)
unfoldGrid = replicateRegions >>> indexByRegion >>> replicateDigits
  where
    replicateRegions :: GridArr v -> Acc (Array GridRegions v)
    replicateRegions = A.replicate (lift (Z :. (3 :: Int) :. All :. All))
    -- I3 region major minor

    replicateDigits :: Acc (Array GridRegions v) -> Acc (GridUnfolded v)
    replicateDigits = A.replicate (constant (Z :. All :. All :. (9 :: Int) :. All))
    -- I3 region major digit minor

    indexByRegion :: Acc (Array GridRegions v) -> Acc (Array GridRegions v)
    indexByRegion = A.backpermute (I3 3 9 9) fromMajorMinor_

type CompactFold v m = A.Fold (Exp (UnfoldedGrid, v)) (Exp m)

type DigitFold m m' = A.Fold (Exp (ByDigit RowCol, m)) (Exp m')

-- this is too simplistic. need to replace `fold (<>) mempty` with another `Fold (Digit, m) m'`
compactSummaries ::
    (SummaryC m v, SummaryC m' m) => CompactFold v m -> DigitFold m m' -> Acc (GridUnfolded v) -> AccSummaries m'
compactSummaries m m' = A.indexed >>> A.runFold m >>> A.indexed >>> A.runFold m'

summarizeGrid :: (SummaryC m v, SummaryC m' m) => CompactFold v m -> DigitFold m m' -> GridArr v -> AccSummaries m'
summarizeGrid m m' = unfoldGrid >>> compactSummaries m m'

{- | provide a function that, given the Summary `Array` and a `Row`/`Column` index into the Grid,
derives the update to be applied at that position.
-}
deriveUpdates :: forall m r. (Monoid (Exp m), Elt m, Elt r) => CollateF m r -> AccSummaries m -> AccSummaries r
deriveUpdates updateF summs = generate (I2 9 9) (updateF summs)
{-# INLINE deriveUpdates #-}

{- | the simplest possible update function. it monoidally combines the Summaries for each `Cell`'s `Row`/`Column`/`Box`.
provide this function to `applySummary'` which will pass it to `deriveUpdates`.
-}
collate :: (Monoid (Exp m), Elt m) => AccSummaries m -> Exp RowCol -> Exp m
collate summs = match \case
    (I2 row col) -> summs ! I2 0 row <> summs ! I2 1 col <> summs ! I2 2 (boxNumber_ (T2 row col))
{-# INLINE collate #-}

-- some aliases to shorten these monstrously long function types
type CollateF m r = AccSummaries m -> Exp RowCol -> Exp r

type UpdateF m a = Exp CellPos -> Exp m -> Exp a -> Exp a

type SummCellFold m a = A.Fold (Exp (ByDigit RowCol, (m, Cell a))) (Exp (Maybe (Cell a)))

type SummaryC m a = (Monoid (Exp m), Elt m, Elt a)

type GridArr a = Acc (Array RowCol a)

type IntConv a = (BS.IsIntegralExp Int a, BS.IsIntegralExp a Int, BS.IsIntegralExp a Word16, BS.IsIntegralExp Word16 a)

-- need to add an extra step here so we can generate an update by digit and incorporate the cell
-- goal: e.g. with HiddenSingles -- supply an `A.Fold (ByDigit RowCol, (m, Cell a)) (Cell a)` via an intermediate `First` monoid.
applySummary' :: (SummaryC m a, Elt r) => CollateF m r -> UpdateF r a -> AccSummaries m -> GridArr a -> GridArr a
applySummary' coll f = deriveUpdates coll >>> A.izipWith (f . view cellIx)

applySummaryWith ::
    (SummaryC m a, Elt r) => CollateF m r -> SummCellFold r a -> AccSummaries m -> GridArr (Cell a) -> GridArr (Cell a)
applySummaryWith coll cf summs g = A.zipWith A.fromMaybe g folded'
  where
    gridByDigit = A.replicate (A.constant $ Z :. All :. All :. (9 :: Int)) g
    summs' = A.replicate (A.constant $ Z :. All :. All :. (9 :: Int)) $ deriveUpdates coll summs
    joined = A.izipWith (\i m c -> T2 i (T2 m c)) summs' gridByDigit
    folded' = A.runFold cf joined

type CountedKnownsByDigit = Sum Int

type Knowns a = Union (CellSet a)

type PossLocsByDigit a = Union (CellSet a)

type PossLocs a = SummaryU (Union (CellSet a))

type FillableLocs a = Union (CellSet a)

type KnownLocs a = Union (CellSet a)

type ContradictionsByDigit a = (CountedKnownsByDigit, PossLocsByDigit a, Knowns a, KnownLocs a)

type Contradictions' a = (Knowns a, PossLocs a, Knowns a, FillableLocs a, KnownLocs a)

majorMinorIdxToCellPos :: Exp RowCol -> Exp RegionMinorIx -> Exp CellPos
majorMinorIdxToCellPos = match \case
    I2 ri major -> \minor ->
        fromMajorMinor_ (I3 ri major minor) & match \case
            I3 _ row col ->
                T3 (A.fromIntegral row + 1) (A.fromIntegral col + 1) (boxNumber_ (T2 (A.fromIntegral row) (A.fromIntegral col)) + 1)

oneIndexCellPos :: Exp CellPos -> Exp CellPos
oneIndexCellPos = match \case
    T3 r c b -> T3 (r + 1) (c + 1) (b + 1)

sharesCell ::
    (IntConv a, Elt a) =>
    Exp Summs -> Exp RegionMinorIx -> Exp (Knowns a) -> Exp a -> Exp a -> Exp (Maybe (ContradictionDesc Int a))
sharesCell idx minor =
    match \case
        Union_ (CellSet_ ks) -> \i i' ->
            if A.not (i `BS.member_` ks) && A.not (i' `BS.member_` ks)
                then Just_ (SharedCell_ i i' (oneIndexCellPos (majorMinorIdxToCellPos idx minor)))
                else Nothing_

-- | `Digit`s that have been marked `Known` in multiple places in the same region have been contradicted.
repeatsDigit ::
    forall a.
    (A.Eq a, BS.IsIntegralExp Word16 a, BS.IsIntegralExp Int a) =>
    Exp Summs -> Exp (Knowns a) -> Exp a -> Exp (Maybe (ContradictionDesc Int a))
repeatsDigit = match \case
    I2 ri major -> match \case
        Union_ (CellSet_ ks) -> \a ->
            if a `BS.member_` ks
                then Just_ (DigitRepeats_ a (BS.fromRepr ri) major)
                else Nothing_

-- | `Digit`s that have no fillable location in a region have been contradicted.
digitContradicted_ ::
    forall a.
    (A.Eq a, IntConv a) =>
    Exp Summs -> Exp (Knowns a) -> Exp (PossLocs a) -> Exp a -> Exp (Maybe (ContradictionDesc Int a))
digitContradicted_ = match \case
    I2 ri major -> match \case
        Union_ (CellSet_ ks) -> \possLocs a ->
            if A.not (a `BS.member_` ks)
                then
                    possLocs ^. ix (BS.toRepr a) & match \case
                        Union_ (CellSet_ ps) -> if BS.null_ ps then Just_ (CannotPlace_ a (BS.fromRepr @Int @RegionIndicator ri) major) else Nothing_
                else Nothing_

-- | Cells that have no possible fill and aren't already marked `Known` have been contradicted.
cellContradicted_ ::
    forall a.
    (A.Eq a, BS.IsIntegralExp Word16 a, BS.IsIntegralExp Int a) =>
    Exp Summs -> Exp (FillableLocs a) -> Exp (KnownLocs a) -> Exp a -> Exp (Maybe (ContradictionDesc Int a))
cellContradicted_ (I2 ri major) (Union_ (CellSet_ ls)) (Union_ (CellSet_ ks)) a =
  -- if the `Cell` has a `Known` value or if it's got some (any) fill, it's not contradicted.
  -- `ls` is the set of region minor indices where some fill is possible and `ks`
  -- is the set of region minor indices that have been marked `Known`.
  if A.not (a `BS.member_` ls A.|| a `BS.member_` ks)
  then Just_ (NoFillForCell_ (majorMinorIdxToCellPos (I2 ri major) (BS.toRepr a)))
  else Nothing_

viewKnowns :: Acc (Array Summs (Contradictions' a)) -> Exp Summs -> Exp (Knowns a)
viewKnowns summs idx = view _3 (summs ! idx)

viewPossLocs :: Acc (Array Summs (Contradictions' a)) -> Exp Summs -> Exp (PossLocs a)
viewPossLocs summs idx = view _2 (summs ! idx)

viewCountedKnowns :: Acc (Array Summs (Contradictions' a)) -> Exp Summs -> Exp (Knowns a)
viewCountedKnowns summs idx = view _1 (summs ! idx)

viewFillableLocs :: Acc (Array Summs (Contradictions' a)) -> Exp Summs -> Exp (FillableLocs a)
viewFillableLocs summs idx = view _4 (summs ! idx)

viewKnownLocs :: Acc (Array Summs (Contradictions' a)) -> Exp Summs -> Exp (KnownLocs a)
viewKnownLocs summs idx = view _5 (summs ! idx)

allSharedCells ::
    forall (a :: Type).
    (Elt a, IntConv a, Eq a, P.Bounded a) => Acc (Array Summs (Contradictions' a)) -> Acc (Vector (ContradictionDesc Int a))
allSharedCells summs = A.map A.fromJust . view _1 . A.filter A.isJust . A.generate (shape summs ::. 9 ::. 9) $ A.match \case
    I4 region major i i' ->
        T2 (psFor i) (psFor i') & match \case
            T2 (Union_ (CellSet_ l)) (Union_ (CellSet_ l')) ->
                if i A.< i' && BS.size_ l A.== 1 && BS.size_ l' A.== 1 && locFrom l A.== locFrom l'
                    then sharesCell idx (locFrom l) (viewKnowns summs idx) (BS.fromRepr i) (BS.fromRepr i')
                    else Nothing_
      where
        idx = I2 region major
        locFrom cs = BS.toRepr @Int . A.fromJust $ BS.first_ cs
        psFor a = viewPossLocs summs idx ^. ix a

allDigitRepeats ::
    (Elt a, IntConv a, Eq a) => Acc (Array Summs (Contradictions' a)) -> Acc (Vector (ContradictionDesc Int a))
allDigitRepeats summs = A.map A.fromJust . view _1 . A.filter A.isJust . A.generate (shape summs ::. 9) $ A.match \case
    I3 region major i -> let idx = I2 region major in repeatsDigit idx (viewCountedKnowns summs idx) (BS.fromRepr i)

allContradictedDigits ::
    (Elt a, IntConv a, Eq a) => Acc (Array Summs (Contradictions' a)) -> Acc (Vector (ContradictionDesc Int a))
allContradictedDigits summs = A.map A.fromJust . view _1 . A.filter A.isJust . A.generate (shape summs ::. 9) $ match \case
    I3 region major i -> let idx = I2 region major in digitContradicted_ idx (viewKnowns summs idx) (viewPossLocs summs idx) (BS.fromRepr i)

allContradictedCells ::
    (Elt a, IntConv a, Eq a) => Acc (Array Summs (Contradictions' a)) -> Acc (Vector (ContradictionDesc Int a))
allContradictedCells summs = A.map A.fromJust . view _1 . A.filter A.isJust . A.generate (shape summs ::. 9) $ match \case
    I3 region major i ->
        let idx = I2 region major
        in cellContradicted_ idx (viewFillableLocs summs idx) (viewKnownLocs summs idx) (BS.fromRepr i)

describeContradictions ::
    forall a.
    (Eq a, IntConv a, P.Bounded a) => Acc (Array Summs (Contradictions' a)) -> Acc (A.Vector (ContradictionDesc Int a))
describeContradictions summs = contraDescs
  where
    contraDescs = allSharedCells summs A.++ (allDigitRepeats summs A.++ (allContradictedDigits summs A.++ allContradictedCells summs))

type Solved' = CountedKnownsByDigit

foldOver :: (Field2 i i' s b) => Getting b s b -> A.Fold i' o -> A.Fold i o
foldOver l (A.Fold i o) = A.Fold (i . (_2 %~ view l)) o

completelySummarize_ ::
    forall a m m'.
    (Monoid (Exp m), Elt m, Monoid (Exp m'), Elt m', Elt a, IntConv a) =>
    A.Fold (Exp (UnfoldedGrid, Cell a)) (Exp m)
    -> A.Fold (Exp (ByDigit RowCol, m)) (Exp m')
    -> GridArr (Cell a)
    -> (Acc (Array RowCol (Contradictions' a)), Acc (Array RowCol Solved'), AccSummaries m')
completelySummarize_ m m' = summarizeGrid foldByDigit compactify >>> A.unzip3
  where
    foldByDigit = T3 <$> contradictionsByDigit <*> countedKnownsByDigit_ <*> m
    compactify = T3 <$> foldOver _1 contradictions_ <*> foldOver _2 idFold <*> foldOver _3 m'

-- | `A.Fold` over each index in a region, broken down by `Digit`.
contradictionsByDigit ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (Exp (UnfoldedGrid, Cell a)) (Exp (ContradictionsByDigit a))
contradictionsByDigit = T4 <$> countedKnownsByDigit_ <*> countedPossByDigit_ <*> knownsByDigit_ <*> knownLocsByDigit

-- | `A.Fold` over all `Digit`s, combining their summaries within a single region of the `Grid`.
contradictions_ ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (Exp (ByDigit RowCol, ContradictionsByDigit a)) (Exp (Contradictions' a))
contradictions_ =
    T5
        <$> foldOver _1 multiplyKnown
        <*> foldOver _2 possLocs_
        -- just unioning over all digits yields the set of digits that are known in the region
        <*> foldOver _3 idFold
        -- just unioning over indices that can take a particular digit yields the set of indices that have
        -- some (any) fill
        <*> foldOver _2 idFold
        -- just unioning over indices where a digit is known yields the set of indices that have been
        -- marked known in the region
        <*> foldOver _4 idFold

{- | `A.Fold` that's equivalent to `A.fold (<>) mempty` -- i.e. the `A.Fold` from `i` to `i`
this would properly be `A.Fold id id` if the input were not indexed.
-}
idFold :: (Elt m, Elt idx, Monoid (Exp m)) => A.Fold (A.Exp (idx, m)) (A.Exp m)
idFold = A.Fold (view _2) id

countedKnownsByDigit_ ::
    forall (a :: Type). (Elt a) => A.Fold (A.Exp (UnfoldedGrid, Cell a)) (A.Exp CountedKnownsByDigit)
countedKnownsByDigit_ = flip A.Fold id $ match \case
    -- we're organizing these by `Digit` so we only need an entry here if the index's `Digit` matches the `Known` `Digit`
    T2 (I4 _ _ d _) (KnownRepr_ d') -> if d A.== A.fromIntegral d' then Sum_ 1 else mempty
    _ -> Sum_ 0

knownLocsByDigit ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (A.Exp (UnfoldedGrid, Cell a)) (A.Exp (KnownLocs a))
knownLocsByDigit = flip A.Fold id $ match \case
    T2 (I4 _ _ d l) (KnownRepr_ d') -> if d A.== A.fromIntegral d' then Union_ (CellSet_ (BS.singleton_ (BS.fromRepr l))) else mempty
    _ -> mempty

-- if the count across the current region is > 1, then we've contradicted the `Grid`. the input to this `A.Fold` counts
-- the number of times the `Digit` is `Known` in a single `Region`. its output is a `BitSet` of all `Digit`s that
-- are marked `Known` multiple times within the current `Region`.
multiplyKnown ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (A.Exp (ByDigit RowCol, Sum Int)) (A.Exp (Union (CellSet a)))
multiplyKnown = flip A.Fold id $ match \case
    T2 (I3 _ _ d) count ->
        Union_
            . CellSet_
            $ if count A.> 1
                then BS.singleton_ (BS.fromRepr d)
                else BS.empty_

countedPossByDigit_ ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (Exp (UnfoldedGrid, Cell a)) (Exp (Union (CellSet a)))
countedPossByDigit_ = flip A.Fold id $ match \case
    T2 (I4 _ _ d minor) (Possibly_ (CellSet_ ds)) ->
        Union_ . CellSet_ $ if BS.fromRepr d `BS.member_` ds then BS.singleton_ (BS.fromRepr minor) else BS.empty_
    _ -> Union_ (CellSet_ BS.empty_)

-- | region minor indices where a particular digit is possible
possLocs_ ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (A.Exp (ByDigit RowCol, Union (CellSet a))) (A.Exp (SummaryU (Union (CellSet a))))
possLocs_ = flip A.Fold id $ match \case
    T2 (I3 _ _ d) m -> summuSingleton_ m d

knownsByDigit_ ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (A.Exp (UnfoldedGrid, Cell a)) (A.Exp (Union (CellSet a)))
knownsByDigit_ = flip A.Fold id $ match \case
    T2 (I4 _ _ d _) (KnownRepr_ d') -> Union_ . CellSet_ $ if d A.== A.fromIntegral d' then BS.singleton_ (BS.fromRepr d) else BS.empty_
    _ -> Union_ (CellSet_ BS.empty_)

nakedSinglesByDigit_ :: (Elt a) => A.Fold (A.Exp (UnfoldedGrid, Cell a)) (A.Exp ())
nakedSinglesByDigit_ = A.Fold (pure (A.lift ())) id

newtype Counted a = Counted {runCounted :: (Sum Int, a)}
    deriving (Generic)

instance (Elt a) => Elt (Counted a)

mkPattern ''Counted

instance (Elt a, Semigroup (Exp a)) => Semigroup (Exp (Counted a)) where
    (<>) = match \case
        Counted_ (T2 count a) -> match \case
            Counted_ (T2 count' a') -> Counted_ (T2 (count <> count') (a <> a'))

instance (Elt a, Monoid (Exp a)) => Monoid (Exp (Counted a)) where
    mempty = Counted_ (T2 0 mempty)

hiddenSinglesByDigit_ ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Int a, BS.IsIntegralExp Word16 a) =>
    A.Fold (Exp (UnfoldedGrid, Cell a)) (Exp (Counted (Union (CellSet a))))
hiddenSinglesByDigit_ = flip A.Fold id $ match \case
    T2 (I4 _ _ d _) (Possibly_ (CellSet_ ds)) ->
        if BS.fromRepr d `BS.member_` ds
            then Counted_ . T2 1 . Union_ . CellSet_ $ BS.singleton_ (BS.fromRepr d)
            else mempty
    _ -> mempty

hiddenSingles_ ::
    forall (a :: Type).
    (Elt a, BS.IsIntegralExp Word16 a) =>
    A.Fold (Exp (ByDigit RowCol, Counted (Union (CellSet a)))) (A.Exp (Union (CellSet a)))
hiddenSingles_ = A.Fold (i . view _2) id
  where
    i = match \case
        Counted_ (T2 count cs) -> if count == 1 then cs else mempty

updateFromNakedSinglesFold ::
    forall (a :: Type). (IntConv a, Elt a) => Exp (ByDigit RowCol, ((), Cell a)) -> Exp (Maybe (Cell a))
updateFromNakedSinglesFold = match \case
    T2 (I3 _ _ d) (T2 _ (Possibly_ (CellSet_ cs))) ->
        if BS.size_ cs A.== 1 && BS.fromRepr d `BS.member_` cs
            then Just_ (KnownRepr_ (BS.fromRepr d))
            else Nothing_
    _ -> Nothing_

updateFromNakedSingles_ ::
    forall (a :: Type). (IntConv a, Elt a, P.Bounded a) => Exp (Cell a) -> Exp (Cell a)
updateFromNakedSingles_ = match \case
    cell@(Possibly_ (CellSet_ cs)) ->
        if BS.size_ cs A.== 1
            then KnownRepr_ . BS.toRepr $ A.fromJust (BS.first_ cs)
            else cell
    cell -> cell

updateFromKnowns_ ::
    forall (a :: Type). (Elt a, IntConv a) => Exp CellPos -> Exp (Union (CellSet a)) -> Exp (Cell a) -> Exp (Cell a)
updateFromKnowns_ _ cs = match \case
    cell@(KnownRepr_ _) -> cell
    Possibly_ (CellSet_ ds) ->
        cs & match \case
            -- `ks` is the set of `Digit`s known in this cell's `Row`/`Column`/`Box`. so we just remove them all
            -- from this `Cell`'s possibilities.
            (Union_ (CellSet_ ks)) -> Possibly_ . CellSet_ $ ds BS.\\\ ks

firstOf_ :: (Elt a) => (Exp c -> Exp (Maybe a)) -> A.Fold (Exp c) (Exp (Maybe a))
firstOf_ f = A.Fold (First_ . f) getFirst_

updateFromHiddenSinglesFold ::
    forall (a :: Type). (Elt a, IntConv a) => Exp (ByDigit RowCol, (Union (CellSet a), Cell a)) -> Exp (Maybe (Cell a))
updateFromHiddenSinglesFold = match \case
    T2 (I3 _ _ d) (T2 (Union_ (CellSet_ cs)) (Possibly_ (CellSet_ ds))) ->
        if BS.fromRepr d `BS.member_` BS.intersection_ cs ds
            then Just_ (KnownRepr_ (BS.fromRepr d))
            else Nothing_
    _ -> Nothing_

updateFromHiddenSingles_ ::
    forall (a :: Type).
    (Elt a, IntConv a, P.Bounded a) => Exp CellPos -> Exp (Union (CellSet a)) -> Exp (Cell a) -> Exp (Cell a)
updateFromHiddenSingles_ _ = match \case
    Union_ (CellSet_ cs) -> match \case
        -- the digits in `cs` are those that have a single possible location.
        -- so the intersection between the digits possible in this `Cell`
        -- and the digits in this `Cell`'s row/column/box that have a single possible
        -- location is exactly the digits that must exist simultaneously in this
        -- `Cell`. we ignore the possibility of multiple digits living in the
        -- same cell here because we check for that when assessing the grid to
        -- see if we've arrived at a contradiction.
        cell@(Possibly_ (CellSet_ ds)) -> A.maybe cell (KnownRepr_ . BS.fromRepr) (BS.first_ (BS.intersection_ cs ds))
        cell -> cell

gridIsSolved_ :: AccSummaries Solved' -> Acc (Array Z Bool)
gridIsSolved_ = A.all (A.== 9) >>> A.all id

gridIsContradicted_ :: (Elt a, Elt i) => Acc (Vector (ContradictionDesc i a)) -> Acc (Scalar Bool)
gridIsContradicted_ contras = single
  where
    single = A.generate (lift Z) (const (A.size contras A.> 0))

explainSummary_ ::
    forall (a :: Type) m.
    (Monoid (Exp m), Elt a, Elt m) =>
    (Exp Summs -> Exp m -> Exp (Maybe (ExplainDesc a))) -> AccSummaries m -> Acc (Vector (ExplainDesc a))
explainSummary_ f = A.imap f >>> A.filter A.isJust >>> view _1 >>> A.map A.fromJust

explainKnowns_ :: forall (a :: Type). (Elt a, IntConv a) => Exp Summs -> Exp (Knowns a) -> Exp (Maybe (ExplainDesc a))
explainKnowns_ = match \case
    I2 region major -> match \case
        Union_ (CellSet_ cs) ->
            if BS.size_ cs > 0
                then Just_ (AlreadyKnown_ (BS.fromRepr region) major cs)
                else Nothing_

explainNakedSingles_ ::
    forall (a :: Type). (Elt a, IntConv a, P.Bounded a) => Acc (Vector (Cell a)) -> Acc (Vector (ExplainDesc a))
explainNakedSingles_ = A.reshape (I2 9 9) >>> A.imap describeNakedSingle >>> A.filter A.isJust >>> view _1 >>> A.map A.fromJust
  where
    describeNakedSingle = match \case
        (I2 row col) -> match \case
            (Possibly_ (CellSet_ cs)) ->
                if BS.size_ cs A.== 1
                    then Just_ (SinglePoss_ (oneIndexCellPos (rowColToCellPos (I2 row col))) (A.fromJust (BS.first_ @_ @a cs)))
                    else Nothing_
            _ -> Nothing_

explainHiddenSingles_ ::
    forall (a :: Type). (Elt a) => Exp Summs -> Exp (Union (CellSet a)) -> Exp (Maybe (ExplainDesc a))
explainHiddenSingles_ = match \case
    I2 region major -> match \case
        Union_ (CellSet_ cs) ->
            if BS.size_ cs > 0
                then Just_ (SingleLoc_ (BS.fromRepr region) major cs)
                else Nothing_

class AccSimplifier f a where
    type AccDigitMonoidFor f a
    type AccMonoidFor f a
    accSimplify ::
        (Elt a, P.Bounded a) =>
        f -> AccSummaries (AccMonoidFor f a) -> Acc (Array RowCol (Cell a)) -> Acc (Array RowCol (Cell a))
    accExplain ::
        (Elt a, P.Bounded a) => f -> AccSummaries (AccMonoidFor f a) -> Acc (Vector (Cell a)) -> Acc (Vector (ExplainDesc a))
    accSummarizeDigits :: (Elt a) => f -> A.Fold (Exp (UnfoldedGrid, Cell a)) (Exp (AccDigitMonoidFor f a))
    accSummarize :: (Elt a) => f -> Proxy a -> A.Fold (Exp (ByDigit RowCol, AccDigitMonoidFor f a)) (Exp (AccMonoidFor f a))
    accSimplifierName :: f -> Proxy a -> Builder

newtype RawSimplifierRes a
    = RawSimplifierRes (Vector (Cell a), Vector (ExplainDesc a), Vector (ContradictionDesc Int a), Scalar Bool, Scalar Bool)
    deriving (Generic)

instance (Elt a) => Arrays (RawSimplifierRes a)

pattern RawSimplifierRes_ ::
    (Elt a) =>
    Acc (Vector (Cell a), Vector (ExplainDesc a), Vector (ContradictionDesc Int a), Scalar Bool, Scalar Bool)
    -> Acc (RawSimplifierRes a)
pattern RawSimplifierRes_ res = Pattern res

accFullSimplifyStep ::
    forall (a :: Type) f.
    ( Elt a
    , AccSimplifier f a
    , Monoid (Exp (AccMonoidFor f a))
    , Monoid (Exp (AccDigitMonoidFor f a))
    , Elt (AccMonoidFor f a)
    , Elt (AccDigitMonoidFor f a)
    , IntConv a
    , A.Eq a
    , P.Bounded a
    ) =>
    f -> Acc (Vector (Cell a)) -> Acc (RawSimplifierRes a)
accFullSimplifyStep f rg = RawSimplifierRes_ (T5 (A.flatten g') explanations contraDescs solved' contradicted')
  where
    g = A.reshape (I2 9 9) rg
    (contras, solvedSumms, m) = completelySummarize_ (accSummarizeDigits f) (accSummarize f (Proxy @a)) g
    contraDescs = describeContradictions contras
    g' = accSimplify f m g
    solved' = gridIsSolved_ solvedSumms
    contradicted' = gridIsContradicted_ contraDescs
    explanations = accExplain f m rg
{-# SPECIALIZE INLINE accFullSimplifyStep ::
    AccSimplifyKnowns -> Acc (Vector (Cell Digit)) -> Acc (RawSimplifierRes Digit)
    #-}
{-# SPECIALIZE INLINE accFullSimplifyStep ::
    AccSimplifyNakedSingles -> Acc (Vector (Cell Digit)) -> Acc (RawSimplifierRes Digit)
    #-}
{-# SPECIALIZE INLINE accFullSimplifyStep ::
    AccSimplifyHiddenSingles -> Acc (Vector (Cell Digit)) -> Acc (RawSimplifierRes Digit)
    #-}

accFullSimplifyStepKnowns :: Acc (Vector (Cell Digit)) -> Acc (RawSimplifierRes Digit)
accFullSimplifyStepKnowns = accFullSimplifyStep @Digit AccSimplifyKnowns
{-# NOINLINE accFullSimplifyStepKnowns #-}

accFullSimplifyStepNakedSingles :: Acc (Vector (Cell Digit)) -> Acc (RawSimplifierRes Digit)
accFullSimplifyStepNakedSingles = accFullSimplifyStep @Digit AccSimplifyNakedSingles
{-# NOINLINE accFullSimplifyStepNakedSingles #-}

accFullSimplifyStepHiddenSingles :: Acc (Vector (Cell Digit)) -> Acc (RawSimplifierRes Digit)
accFullSimplifyStepHiddenSingles = accFullSimplifyStep @Digit AccSimplifyHiddenSingles
{-# NOINLINE accFullSimplifyStepHiddenSingles #-}

data AccSimplify where
    AccSimplifyKnownsImpl :: AccSimplify
    AccSimplifyNakedSinglesImpl :: AccSimplify
    AccSimplifyHiddenSinglesImpl :: AccSimplify

data AccSimplifyKnowns = AccSimplifyKnowns

data AccSimplifyNakedSingles = AccSimplifyNakedSingles

data AccSimplifyHiddenSingles = AccSimplifyHiddenSingles

instance (IntConv a) => AccSimplifier AccSimplifyKnowns a where
    type AccDigitMonoidFor AccSimplifyKnowns a = Union (CellSet a)
    type AccMonoidFor AccSimplifyKnowns a = Union (CellSet a)
    accSimplify _ = applySummary' collate updateFromKnowns_
    accExplain _ summs _ = explainSummary_ explainKnowns_ summs
    accSummarizeDigits _ = knownsByDigit_
    accSummarize _ _ = idFold
    accSimplifierName _ _ = "Remove Knowns from possible cell values"

instance (IntConv a) => AccSimplifier AccSimplifyNakedSingles a where
    type AccDigitMonoidFor AccSimplifyNakedSingles a = ()
    type AccMonoidFor AccSimplifyNakedSingles a = ()
    accSimplify _ _ = A.map updateFromNakedSingles_
    accExplain _ _ = explainNakedSingles_
    accSummarizeDigits _ = nakedSinglesByDigit_
    accSummarize _ _ = idFold
    accSimplifierName _ _ = "(Naked Single) Set cells with just one option to Known"

instance (IntConv a) => AccSimplifier AccSimplifyHiddenSingles a where
    type AccDigitMonoidFor AccSimplifyHiddenSingles a = Counted (Union (CellSet a))
    type AccMonoidFor AccSimplifyHiddenSingles a = Union (CellSet a)
    accSimplify _ = applySummary' collate updateFromHiddenSingles_
    accExplain _ summs _ = explainSummary_ explainHiddenSingles_ summs
    accSummarizeDigits _ = hiddenSinglesByDigit_
    accSummarize _ _ = hiddenSingles_
    accSimplifierName _ _ = "(Hidden Single) Place digit with just one location"

toAccVec :: forall (a :: Type). (Elt a) => VU.Vector (Cell a) -> Vector (Cell a)
toAccVec = VU.convert

-- A.replicate (previous) (Z :. All :. 9 (digits))
-- type unfoldedGrid = Z :. region :. regionMajor :. regionMinor :. digit
-- map (Cell Digit) -> type named below (for each fold)
-- fold to accomplish:
--   countedKnowns :: Acc (Array unfoldedGrid (known :: Bool)) -> Acc (Array (Z :. region :. regionMajor :. digit) (Union (CellSet Digit)))  -- cond: count > 1 across regionMinor
--   emptyLocs :: Acc (Array unfoldedGrid (possible :: Bool)) -> Acc (Array (Z :. region :. regionMajor) (Union (CellSet Digit))) -- digits that can't be placed in the region
--   possLocs :: Acc (Array unfoldedGrid (possible :: Bool)) -> Acc (Array (Z :. region :. regionMajor) (SummaryU (Union (CellSet Digit))))
--   knowns :: Acc (Array unfoldedGrid (known :: Bool)) -> Acc (Array (Z :. region :. regionMajor) (Union (CellSet Digit)))
--   nakedSingles :: Acc (Array unfoldedGrid (nakedSingle :: Bool)) -> Acc (Array (Z :. region :. regionMajor :. digit) (Union (CellSet Digit))) -- cond: Cell poss count == 1
--   hiddenSingles :: Acc (Array unfoldedGrid (possCount :: Sum Int)) -> Acc (Array (Z :. region :. regionMajor :. digit) (Union (CellSet Digit))) -- cond: possCount == 1 across region Minor

-- interpretation:
--  countedKnowns: RegionSummaries (MonoidalMap Digit KnownLocs) -- digits known multiple times in region
--  possLocs: RegionSummaries (MonoidalMap Digit PossLocs) -- digits by where they can still occur in region
--  knowns: RegionSummaries (Union (CellSet Digit)) -- digits known in region
--  nakedSingles: RegionSummaries (MonoidalMap Digit SingleCellPoss) -- Digit x CellPos to be marked Known
--  hiddenSingles: RegionSummaries (MonoidalMap Digit PossCount) -- digits with only a single loc left in region

-- TODO:
-- 1. translate one simplifier to Accelerate (`SimplifyKnowns` should be very easy)
-- 2. translate `explainSummary`
-- 3. translate contradiction checking & solved checking
-- 4. translate the other cheap simplifiers to Accelerate
-- 5. `runQAsync` for the backtracking solver during contradictions search
-- 6. figure out what the hell to do about Tuples & LocationAlignment
