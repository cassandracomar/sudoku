{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | this module provides monoidal summaries of a grid. these summaries provide the workhorses we use to actually define simplifier rules
and are therefore the basic building blocks of the solver. the idea is to only process the grid twice per step. we'd love to do once per step
but applying the updates can't happen at the same time as info collection because we need non-local information when determining what update
to apply to each cell -- i.e. a summary of each cell's `Row`, `Column`, and `Box`. so we instead collect summaries monoidally, appending
a single cell's summary to the summaries for each of its `Row`/`Column`/`Box`.
-}
module Sudoku.Summaries where

import Control.Applicative ((<**>))
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Containers.ListUtils (nubOrd)
import Data.Default (Default (def))
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Monoid (Sum (..))
import Data.Utils ((>>>>))
import Data.Vector.Generic.Lens (vectorIx)
import Data.Vector.Unboxed (IsoUnbox)
import Data.Word (Word16, Word8)
import Data.Word16Set (bsfolded)
import GHC.Base (inline)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, natVal)
import GHC.TypeLits.Singletons (KnownNat)
import Sudoku.Cell
import Sudoku.Grid (
    Grid,
    allIndicesV,
    allSame,
    boxAt,
    colAt,
    ensure,
    removePossibilitiesOfOn,
    rowAt,
    sameBox,
    sameCol,
    sameRow,
    (@\\),
 )
import TextShow (TextShow (showb), toString)
import TextShow.Data.List (showbListWith)

import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.IntMap.Internal qualified as MM
import Data.IntMap.Monoidal.Strict qualified as M
import Data.IntMap.Strict qualified as MS
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as MVU
import Data.Word16Set qualified as A.BS

-- | `A.BS.BitSet` with all digits marked
completeDigitSet :: A.BS.BitSet a
completeDigitSet = A.BS.BitSet 0b111111111

newtype Union a = Union {runUnion :: a}
    deriving (Eq, Ord, Generic)

instance IsoUnbox (Union (CellSet a)) Word16 where
    fromURepr = Union . CellSet . A.BS.BitSet
    toURepr (Union (CellSet (A.BS.BitSet bs))) = bs

newtype instance VU.MVector s (Union a) = MV_Union (VU.MVector s Word16)

newtype instance VU.Vector (Union a) = V_Union (VU.Vector Word16)

deriving via (Union (CellSet a) `VU.As` Word16) instance MVG.MVector MVU.MVector (Union (CellSet a))

deriving via (Union (CellSet a) `VU.As` Word16) instance VG.Vector VU.Vector (Union (CellSet a))

instance VU.Unbox (Union (CellSet a))

makePrisms ''Union

newtype Intersects a = Intersects {runIntersection :: a} deriving (Eq, Ord, VP.Prim, Generic)

instance IsoUnbox (Intersects (CellSet a)) Word16 where
    fromURepr = Intersects . CellSet . A.BS.BitSet
    toURepr (Intersects (CellSet (A.BS.BitSet bs))) = bs

newtype instance VU.MVector s (Intersects a) = MV_Intersects (VU.MVector s Word16)

newtype instance VU.Vector (Intersects a) = V_Intersects (VU.Vector Word16)

deriving via (Intersects (CellSet a) `VU.As` Word16) instance MVG.MVector MVU.MVector (Intersects (CellSet a))

deriving via (Intersects (CellSet a) `VU.As` Word16) instance VG.Vector VU.Vector (Intersects (CellSet a))

makePrisms ''Intersects

class SetLike a where
    union :: a -> a -> a
    setEmpty :: a
    intersection :: a -> a -> a
    setFull :: a

instance (Enum a) => SetLike (CellSet a) where
    !x `union` !y = CellSet $! _bitSet x `A.BS.union` _bitSet y
    {-# INLINE union #-}
    setEmpty = CellSet A.BS.empty
    {-# INLINE setEmpty #-}
    !x `intersection` !y = CellSet $! _bitSet x `A.BS.intersection` _bitSet y
    {-# INLINE intersection #-}
    setFull = CellSet completeDigitSet
    {-# INLINE setFull #-}

instance (SetLike a) => Semigroup (Union a) where
    !x <> !y = Union $! runUnion x `union` runUnion y
    {-# INLINE (<>) #-}

instance (SetLike a) => Semigroup (Intersects a) where
    Intersects !x <> Intersects !y = Intersects $! x `intersection` y
    {-# INLINE (<>) #-}

instance (SetLike a) => Monoid (Union a) where
    mempty = Union setEmpty
    {-# INLINE mempty #-}

instance (SetLike a) => Monoid (Intersects a) where
    mempty = Intersects setFull
    {-# INLINE mempty #-}

data NoMoreThan (n :: Nat) a where
    NoMoreThan :: {-# UNPACK #-} !Word8 -> NoMoreThan n a
    TooMany :: NoMoreThan n a

type role NoMoreThan phantom phantom

nmtMax :: forall (n :: Nat). (KnownNat n) => Word8
nmtMax = fromIntegral (natVal @n undefined)

deriving instance Generic (NoMoreThan n a)

deriving instance Eq (NoMoreThan n a)

deriving instance Ord (NoMoreThan n a)

instance (KnownNat n) => Semigroup (NoMoreThan (n :: Nat) a) where
    TooMany <> _ = TooMany
    _ <> TooMany = TooMany
    (NoMoreThan x) <> (NoMoreThan y)
        | x + y <= nmtMax @n = NoMoreThan (x + y)
        | otherwise = TooMany
    {-# INLINE (<>) #-}

instance (KnownNat n) => Monoid (NoMoreThan (n :: Nat) a) where
    mempty = NoMoreThan 0
    {-# INLINE mempty #-}

makePrisms ''NoMoreThan

singleOccurrence :: IndexedFold a a (NoMoreThan n a)
singleOccurrence = ifolding (Identity . (,NoMoreThan 1))
{-# INLINE singleOccurrence #-}

occurrenceCounting ::
    forall a. (VU.IsoUnbox a Word16, Enum a) => IndexedFold CellPos (CellPos, Cell a) (M.MonoidalIntMap (NoMoreThan 1 a))
occurrenceCounting = ifolding (Identity . over _2 (ifoldlOf' ks (flip . M.insert . fromEnum) M.empty))
  where
    ks = _Known . singleOccurrence
{-# INLINE occurrenceCounting #-}

data OccursExactly (n :: Nat) a where
    OccursTooFew :: {-# UNPACK #-} !Word8 -> OccursExactly n a
    Exactly :: {-# UNPACK #-} !Word8 -> OccursExactly n a
    OccursTooMany :: OccursExactly n a

type role OccursExactly phantom phantom

deriving instance Generic (OccursExactly n a)

deriving instance Eq (OccursExactly n a)

deriving instance Ord (OccursExactly n a)

deriving instance Show (OccursExactly n a)

instance (KnownNat n) => Semigroup (OccursExactly (n :: Nat) a) where
    OccursTooMany <> _ = OccursTooMany
    _ <> OccursTooMany = OccursTooMany
    Exactly k <> Exactly k'
        | k + k' < nmtMax @n = OccursTooFew (k + k')
        | k + k' == nmtMax @n = Exactly (k + k')
        | otherwise = OccursTooMany
    Exactly k <> OccursTooFew k'
        | k + k' < nmtMax @n = OccursTooFew (k + k')
        | k + k' == nmtMax @n = Exactly (k + k')
        | otherwise = OccursTooMany
    OccursTooFew k <> Exactly k'
        | k + k' < nmtMax @n = OccursTooFew (k + k')
        | k + k' == nmtMax @n = Exactly (k + k')
        | otherwise = OccursTooMany
    OccursTooFew k <> OccursTooFew k'
        | k + k' < nmtMax @n = OccursTooFew (k + k')
        | k + k' == nmtMax @n = Exactly (k + k')
        | otherwise = OccursTooMany
    {-# INLINE (<>) #-}

instance (KnownNat n) => Monoid (OccursExactly (n :: Nat) a) where
    -- force the right constructor to be applied
    mempty = OccursTooFew 0 <> OccursTooFew 0
    {-# INLINE mempty #-}

makePrisms ''OccursExactly

occursTooFew :: (KnownNat n) => IndexedFold a a (OccursExactly n a)
-- force the right constructor to be applied
occursTooFew = ifolding (Identity . (,OccursTooFew 1 <> OccursTooFew 0))
{-# INLINE occursTooFew #-}

possibilityCounting ::
    (VU.IsoUnbox a Word16, Enum a) => IndexedFold CellPos (CellPos, Cell a) (M.MonoidalIntMap (OccursExactly 1 a))
possibilityCounting = ifolding (Identity . over _2 (ifoldlOf' poss (flip . M.insert . fromEnum) M.empty))
  where
    poss = _Possibly . _CellSet . bsfolded . occursTooFew
{-# INLINE possibilityCounting #-}

newtype LocationAlignment a = LocationAlignment {locations :: M.MonoidalIntMap (V.Vector CellPos)}

deriving via (M.MonoidalIntMap (V.Vector CellPos)) instance (Ord a) => Semigroup (LocationAlignment a)

deriving via (M.MonoidalIntMap (V.Vector CellPos)) instance (Ord a) => Monoid (LocationAlignment a)

makeLenses ''LocationAlignment
makePrisms ''LocationAlignment

newtype CellSummaries a = CellSummaries {_byRegion :: V.Vector a}

{- | this is a very inefficient way to update the summaries as we come across them
i.e. to use this, you have to create a singleton summary where every entry is mempty except
the one you want to update, then fold. except because we're zipping over and over,
the generated machine code will involve a bunch of loops over mostly empty vectors.
so instead, there's an `addToSummary` function below that updates the summaries for
each region by indexing the vector and `(<> a)` the value to be combined.
-}
instance (Semigroup a) => Semigroup (CellSummaries a) where
    x <> y = CellSummaries $ V.zipWith (<>) (_byRegion x) (_byRegion y)
    {-# INLINE (<>) #-}

instance (Monoid a) => Monoid (CellSummaries a) where
    mempty = CellSummaries (V.replicate 9 mempty)
    {-# INLINE mempty #-}

data Contradictions a = Contradictions
    { _emptyLocs :: {-# UNPACK #-} !(Union (CellSet Word8))
    , _possLocs :: !(M.MonoidalIntMap (Union (CellSet Word8)))
    , _knownLocs :: !(M.MonoidalIntMap (Sum Int))
    }
    deriving (Eq, Ord)

instance (Ord a, Enum a) => Semigroup (Contradictions a) where
    Contradictions e p k <> Contradictions e' p' k' = Contradictions (e <> e') (p <> p') (k <> k')
    {-# INLINE CONLIKE (<>) #-}

instance (Ord a, Enum a) => Monoid (Contradictions a) where
    mappend = (<>)
    {-# INLINE mappend #-}
    mempty = Contradictions mempty mempty mempty
    {-# INLINE mempty #-}

makeLenses ''Contradictions

data RegionSummaries a = RegionSummaries
    { _rows :: {-# UNPACK #-} !(CellSummaries a)
    , _columns :: {-# UNPACK #-} !(CellSummaries a)
    , _boxes :: {-# UNPACK #-} !(CellSummaries a)
    }

instance (Monoid a) => Default (CellSummaries a) where
    def = CellSummaries (V.replicate 9 mempty)
    {-# INLINE def #-}

instance (Monoid a) => Default (RegionSummaries a) where
    def = RegionSummaries def def def
    {-# INLINE def #-}

makeLenses ''CellSummaries
makeLenses ''RegionSummaries

ixSummary :: Word8 -> Traversal' (CellSummaries a) a
ixSummary i = byRegion . vectorIx (fromIntegral (i - 1))
{-# INLINE ixSummary #-}

type instance IxValue (CellSummaries a) = a

type instance Index (CellSummaries a) = Word8

instance Ixed (CellSummaries a) where
    ix i = byRegion . vectorIx (fromIntegral i - 1)
    {-# INLINE ix #-}

type instance IxValue (RegionSummaries a) = CellSummaries a

type instance Index (RegionSummaries a) = RegionIndicator

instance Ixed (RegionSummaries a) where
    ix Row = rows
    ix Column = columns
    ix Box = boxes
    {-# INLINE ix #-}

{- | assuming the monoid instance for `m` is associative, this function provides an `ifoldl'` function that can be used over an entire
grid to calculate a summary of the grid, organized by region. rather than using the inefficient `Monoid` instance on `CellSummaries`,
it instead updates each region the cell belongs to by indexing `CellSummaries` to get the current summary of each region type and
updates it by `mappend`ing `m` on the right. this relies on the associativity of the `Monoid` instance for `m`. a non-law abiding
`Monoid` instance won't produce the expected results.
-}
addToSummary ::
    forall m v s.
    (Monoid m, MVG.MVector v m) =>
    CellPos
    -> (v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m)
    -> AcrossRegion m
    -> ST s (v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m)
addToSummary (r, c, b) (rs, cs, bs) m = upd Row rs r *> upd Column cs c *> upd Box bs b $> (rs, cs, bs)
  where
    upd ri vs i = MVG.modify vs (<> m ri i) (fromIntegral i - 1)
{-# INLINE addToSummary #-}

summaryOfM ::
    (Monoid m, MVG.MVector v m) =>
    IndexedFold CellPos s (AcrossRegion m)
    -> s
    -> (forall st. ST st (v (MVG.PrimState (ST st)) m, v (MVG.PrimState (ST st)) m, v (MVG.PrimState (ST st)) m))
summaryOfM l s = (,,) <$> initialize <*> initialize <*> initialize >>= flip (ifoldlMOf l addToSummary) s
  where
    initialize = do
        v <- MVG.new 9
        MVG.set v mempty $> v
{-# INLINE summaryOfM #-}

{- | produce a summary for an entire grid. takes the `Grid` vector and a function that maps each `Cell` to a Monoidal summary type `m`, carrying an index that
provides the necessary information about the `Cell`'s `Row`\/`Column`\/`Box`. the summary type chosen determines the information available at the end of the fold.
for example, mapping each `Cell` to a singleton `CellSet` of `Known` digits wrapped in a `Union` newtype will produce the set of already known digits
in each `Row`\/`Column`\/`Box`.
-}
summaryOf :: (Monoid m) => IndexedFold CellPos s (RegionIndicator -> Word8 -> m) -> s -> RegionSummaries m
summaryOf l s = runST (summaryOfM l s >>= frzAll <&> mk)
  where
    frzAll (rs, cs, bs) = (,,) <$> VG.unsafeFreeze rs <*> VG.unsafeFreeze cs <*> VG.unsafeFreeze bs

    mk (r, c, b) = RegionSummaries (CellSummaries r) (CellSummaries c) (CellSummaries b)
{-# INLINE summaryOf #-}

-- | apply updates derived from `RegionSummaries` to `s` as determined by the function `f` at each position (`CellPos`, `Cell`) in `s`.
applySummary ::
    (VU.Unbox a) =>
    (RegionSummaries b -> CellPos -> a -> a) -> Lens' s (VU.Vector a) -> RegionSummaries b -> s -> s
applySummary f sel summs = sel . cells %@~ f summs
{-# INLINE applySummary #-}

explainSummary :: (Ord a) => (RegionIndicator -> Int -> b -> [ExplainDesc a]) -> RegionSummaries b -> [ExplainDesc a]
explainSummary f summs = nubOrd . sort $ explainAcross Row <> explainAcross Column <> explainAcross Box
  where
    explainAcross ri = ifoldMapOf (ix ri . byRegion . traversed) (f ri) summs
{-# INLINE explainSummary #-}

cellSetList :: (Enum a) => Iso' [a] (CellSet a)
cellSetList = iso (CellSet . A.BS.fromList) (A.BS.toList . _bitSet)
{-# INLINE cellSetList #-}

-- | creates an update set by concatenating `CellSummaries` for a given cell position
updateSet :: (Monoid b) => RegionSummaries b -> CellPos -> b
updateSet summs (r, c, b) =
    (summs ^. ix Row . byRegion) V.! (fromIntegral r - 1)
        <> (summs ^. ix Column . byRegion) V.! (fromIntegral c - 1)
        <> (summs ^. ix Box . byRegion) V.! (fromIntegral b - 1)
{-# INLINE updateSet #-}

cellUpdating :: (CellPos -> RegionSummaries b -> Maybe a) -> IndexedFold CellPos (RegionSummaries b) a
cellUpdating f = ifolding (\summs -> V.mapMaybe (\loc -> fmap (loc,) (f loc summs)) allIndicesV)
{-# INLINE cellUpdating #-}

-- this doesn't include everything... need to figure out how to make the tuples search fit into this scheme
data ExplainDesc a
    = AlreadyKnown !RegionIndicator !Int !(A.BS.BitSet a)
    | SinglePoss !RegionIndicator !Int !a
    | SingleLoc !RegionIndicator !Int !(A.BS.BitSet a)
    | LookedForPointing
    | TupleDesc !RegionIndicator !Int !(A.BS.BitSet a) !(A.BS.BitSet a)
    deriving (Eq, Ord, Generic)

instance (TextShow a, Enum a) => TextShow (ExplainDesc a) where
    showb (AlreadyKnown ri i cs) = "In " <> showb ri <> " " <> showb (i + 1) <> ": " <> showb (A.BS.toList cs) <> " are already known."
    showb (SinglePoss ri i d) = "In  " <> showb ri <> " " <> showb (i + 1) <> ": cell can only take a single value, " <> showb d
    showb (SingleLoc ri i cs) = "Hidden Single in " <> showb ri <> " " <> showb (i + 1) <> ": " <> showb (A.BS.toList cs)
    showb LookedForPointing = "Looked for Pointing"
    showb (TupleDesc ri i locs cs) =
        "Found Tuple {cells="
            <> showbListWith (showLocB . toCellPos i . fromEnum) (A.BS.toList locs)
            <> ", digits="
            <> showb (A.BS.toList cs)
            <> "}"
      where
        toCellPos major minor = (ri, major, minor) ^. re _majorMinor . _2

instance (TextShow a, Enum a) => Show (ExplainDesc a) where
    show = toString . showb

explainKnowns :: (Enum a, TextShow a) => RegionIndicator -> Int -> Union (CellSet a) -> [ExplainDesc a]
explainKnowns ri i (Union (CellSet cs))
    | cs /= A.BS.empty && cs /= completeDigitSet = [AlreadyKnown ri i cs]
    | otherwise = []
{-# INLINE explainKnowns #-}

updateFromKnowns :: (Enum a, VU.Unbox a) => RegionSummaries (Union (CellSet a)) -> CellPos -> Cell a -> Cell a
updateFromKnowns summs loc = _Possibly . _CellSet %~ (A.BS.\\ updates)
  where
    !updates = updateSet summs loc ^. _Union . _CellSet
{-# INLINE updateFromKnowns #-}

maybeMarkKnown :: (VU.IsoUnbox a Word16) => Cell a -> Maybe a -> Cell a
maybeMarkKnown cell = const cell & outside _Just .~ mkKnown
{-# INLINE maybeMarkKnown #-}

updateFromNakedSingles ::
    (VU.Unbox a, Enum a, Bounded a, VU.IsoUnbox a Word16) => RegionSummaries b -> CellPos -> Cell a -> Cell a
updateFromNakedSingles _ _ = whenSingletonPoss <**> maybeMarkKnown
  where
    isSingleton cs = A.BS.size cs == 1
    whenSingletonPoss = const Nothing & outside (_Possibly . _CellSet . filtered isSingleton) .~ A.BS.first
{-# INLINE updateFromNakedSingles #-}

explainNakedSingles ::
    (Enum a, TextShow a, VU.Unbox a) =>
    RegionIndicator -> Int -> Union (CellSet a) -> [ExplainDesc a]
explainNakedSingles ri i ds = ds ^.. _Union . _CellSet . bsfolded . to (SinglePoss ri i)
{-# INLINE explainNakedSingles #-}

singlePossibility ::
    (VU.Unbox a, Enum a) => IndexedFold CellPos (CellPos, Cell a) (RegionIndicator -> Word8 -> Union (CellSet a))
singlePossibility = iconstantly $ ifolding (Identity . over _2 singlePoss)
  where
    singlePoss = Union . CellSet . A.BS.bitSetOf (_Possibly . _CellSet . filtered ((== 1) . A.BS.size) . bsfolded)
{-# INLINE singlePossibility #-}

filterSummaryByCount ::
    (Eq a) =>
    RegionIndicator
    -> a
    -> RegionSummaries (M.MonoidalIntMap (Sum a))
    -> RegionSummaries (M.MonoidalIntMap (Sum a))
filterSummaryByCount ri c = ix ri . byRegion %~ V.map (M.filter (== Sum c))
{-# INLINE filterSummaryByCount #-}

filterSummariesByCount ::
    (Eq a) => a -> RegionSummaries (M.MonoidalIntMap (Sum a)) -> RegionSummaries (M.MonoidalIntMap (Sum a))
filterSummariesByCount c = filterSummaryByCount Row c . filterSummaryByCount Column c . filterSummaryByCount Box c
{-# INLINE filterSummariesByCount #-}

findInSummary ::
    (Enum k) => RegionSummaries (M.MonoidalIntMap (Sum Word16)) -> RegionIndicator -> Word8 -> A.BS.BitSet k
findInSummary summs ri i = A.BS.bitSetOf (ix ri . ix (fromIntegral i) . itraversed . asIndex . enumerated) summs
{-# INLINE findInSummary #-}

{- | find the first value in the intersection between a `Cell`'s possibilities and a set of `Digit`s.
if the `Cell` is already `Known`, or if the intersection is empty, give back the the original `Cell`.
if a value could be found, that value is marked as `Known`.

this works because the provided set of `Digit`s is exactly those values that only have a single home in
the `Cell`'s `Row`\/`Column`\/`Box`. we ignore the possibility of multiple values needing to live in the
same `Cell` here because this is handled during contradiction checking.
-}
markUniqueKnown :: (Enum a, Bounded a, VU.IsoUnbox a Word16) => CellSet a -> Cell a -> Cell a
markUniqueKnown (CellSet cs) = intersect <**> maybeMarkKnown
  where
    intersect = const Nothing & outside (_Possibly . _CellSet) .~ A.BS.first . A.BS.intersection cs
{-# INLINE markUniqueKnown #-}

{- | find `Digit`s in each `Row`\/`Column`\/`Box` that can take only a single position and set those `Cell`s to that `Digit`.
this function assumes the `RegionSummaries` has already been filtered such that it only contains those entries where `Digit`s can
only take a single position.
-}
updateFromHiddenSingles ::
    (Enum a, VU.Unbox a, VU.IsoUnbox a Word16, Bounded a, Ord a) =>
    RegionSummaries (M.MonoidalIntMap (Sum Word16)) -> CellPos -> Cell a -> Cell a
updateFromHiddenSingles = findUpdates >>>> markUniqueKnown
  where
    -- `A.BS.union` together the values with only one possible location in this cell's regions -- if this cell
    -- can take any of these values, we'll mark it `Known` here.
    findUpdates s (r, c, b) = CellSet $! findInSummary s Row r <> findInSummary s Column c <> findInSummary s Box b
{-# INLINE updateFromHiddenSingles #-}

explainHiddenSingles ::
    (Enum a, TextShow a, Ord a) => RegionIndicator -> Int -> M.MonoidalIntMap (Sum Word16) -> [ExplainDesc a]
explainHiddenSingles ri i summ
    | not (null summ) =
        [SingleLoc ri i (A.BS.bitSetOf (ifolded . asIndex . enumerated) summ)]
    | otherwise = []
{-# INLINE explainHiddenSingles #-}

enumerated :: (Enum a) => Iso' Int a
enumerated = iso toEnum fromEnum
{-# INLINE enumerated #-}

type AcrossRegion m = RegionIndicator -> Word8 -> m

constantly :: (Monoid m) => Fold s m -> Fold s (AcrossRegion m)
constantly l = folding @Identity (pure . const . const . foldOf l)
{-# INLINE constantly #-}

iconstantly :: (Monoid m) => IndexedFold i (i, s) m -> IndexedFold i (i, s) (AcrossRegion m)
iconstantly l = ifolding @Identity (\(i, s) -> pure (i, const (const (foldOf l (i, s)))))
{-# INLINE iconstantly #-}

-- | a `Fold` that tabulates the values already `Known` across the `Cell`'s row/column/box
knowns ::
    (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (Union (CellSet a)))
knowns = iconstantly $ ifolding (Identity . over _2 (Union . CellSet . A.BS.bitSetOf _Known))
{-# INLINE knowns #-}

monoidalMapOf :: IndexedFold Int s a -> s -> M.MonoidalIntMap a
monoidalMapOf l = ifoldlOf' (inline l) (flip . M.insert) M.empty
{-# INLINE monoidalMapOf #-}

digitCounting :: (Integral c) => IndexedFold Int Int (Sum c)
digitCounting = ito (,Sum 1)
{-# INLINE digitCounting #-}

-- | a `Fold` producing a map for each `Cell` that tracks how many possible locations for a value remain, across it's Row/Column/Box
countedPoss ::
    (Enum a, Ord a, VU.IsoUnbox a Word16) =>
    IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (M.MonoidalIntMap (Sum Word16)))
countedPoss = iconstantly $ ifolding @Identity (pure . over _2 (monoidalMapOf countDigits))
  where
    countDigits = _Possibly . _CellSet . bsfolded . from enumerated . digitCounting
{-# INLINE countedPoss #-}

{- | a `Fold` producing a map for each `Cell` that tracks how many times a value has been marked as Known, across it's Row/Column/Box
this should never exceed 1!
-}
countedKnowns ::
    (Enum a, Ord a, VU.IsoUnbox a Word16) =>
    IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (M.MonoidalIntMap (Sum Int)))
countedKnowns = iconstantly $ ifolding (Identity . over _2 (monoidalMapOf (_Known . from enumerated . digitCounting)))
{-# INLINE countedKnowns #-}

-- | a `Fold` producing a map of digit locations within a region of the grid
valueAlignment ::
    forall a. (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (LocationAlignment a))
valueAlignment = iconstantly $ ifolding (uncurry mk)
  where
    mk = (.) <$> (,) <*> allocateLocs >>>> Identity
    allocateLocs loc cell =
        LocationAlignment $
            foldlOf' (_Possibly . _CellSet . bsfolded . from enumerated) (\r d -> M.insert d (V.singleton loc) r) M.empty cell
{-# INLINE valueAlignment #-}

{- | compose two `Fold`s operating on the same element type but that need access to the index from the parent `Traversal`.
the `Fold`s will need to receive the index from the parent `Traversal` via `withIndex`, so the indexed functions the
`Fold`s are built out of should be uncurried. this trick only works for `Fold`s. `IndexedFold`s that have their own
indices cannot be composed in this way as their separate internal indices need to be composed in some way, even when
they produce different numbers of elements.

because this operator folds down to `Monoid`s on both sides of the pair, the values it produces are indexed by the parent
`Traversal` -- it produces a single paired value for every element of the parent `Traversal`.
-}
(<|.|>) ::
    (Monoid m, Monoid m') =>
    Fold (i, s) (AcrossRegion m) -> Fold (i, s) (AcrossRegion m') -> IndexedFold i (i, s) (AcrossRegion (m, m'))
m <|.|> m' = ifolding @Identity (\(i, s) -> pure (i, \ri i' -> (foldOf m (i, s) ri i', foldOf m' (i, s) ri i')))
{-# INLINE (<|.|>) #-}

infixr 8 <|.|>

{- | this operator is the same as `<|.|>` but it takes a fold that doesn't need access to the parent `Traversal`'s index on
the left.
-}
(|.>) ::
    (Monoid m, Monoid m') =>
    Fold s (AcrossRegion m) -> Fold (i, s) (AcrossRegion m') -> IndexedFold i (i, s) (AcrossRegion (m, m'))
m |.> m' = ifolding @Identity (\(i, s) -> pure (i, \ri i' -> (foldOf m s ri i', foldOf m' (i, s) ri i')))
{-# INLINE (|.>) #-}

infixr 9 |.>

{- | this operator is the same as `<|.|>` but it takes a `Fold` that doesn't need access to the parent `Traversal`'s index on
the right.
-}
(<.|) ::
    (Monoid m, Monoid m') =>
    Fold (i, s) (AcrossRegion m) -> Fold s (AcrossRegion m') -> IndexedFold i (i, s) (AcrossRegion (m, m'))
m <.| m' = ifolding @Identity (\(i, s) -> pure (i, \ri i' -> (foldOf m (i, s) ri i', foldOf m' s ri i')))
{-# INLINE (<.|) #-}

infixr 9 <.|

{- | this operator is the same as `<|.|>` but it takes a `Fold` that doesn't need access to the parent `Traversal`'s index on
both sides.
-}
(|.|) ::
    (Monoid m, Monoid m') =>
    Fold s (AcrossRegion m) -> Fold s (AcrossRegion m') -> IndexedFold i (i, s) (AcrossRegion (m, m'))
m |.| m' = ifolding @Identity (\(i, s) -> pure (i, \ri i' -> (foldOf m s ri i', foldOf m' s ri i')))
{-# INLINE (|.|) #-}

infixr 8 |.|

type SummaryRecord m a = (Contradictions a, Union (CellSet a), m)

ortho :: RegionIndicator -> CellPos -> Union (CellSet Word8)
ortho ri loc = Union (CellSet (A.BS.singleton (fromIntegral minor)))
  where
    minor = (ri, loc) ^. _majorMinor . _3

noPossibilities ::
    (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (Union (CellSet Word8)))
noPossibilities = ifolding @Maybe (\(loc, cell) -> (loc, mk loc) <$ ensure cond cell)
  where
    mk loc ri _ = ortho ri loc
    cond = (&&) <$> is (_Possibly . _CellSet . filtered (== mempty)) <*> isn't _Known
{-# INLINE noPossibilities #-}

possibilitiesWithLoc ::
    (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (M.MonoidalIntMap (Union (CellSet Word8))))
possibilitiesWithLoc = ifolding (\(loc, cell) -> Identity (loc, posses loc cell))
  where
    ins loc ri r a = M.insert a (ortho ri loc) r
    posses loc cell ri = const $ foldlOf' (_Possibly . _CellSet . bsfolded . from enumerated) (ins loc ri) mempty cell
{-# INLINE possibilitiesWithLoc #-}

contradictions ::
    (Enum a, Ord a, VU.IsoUnbox a Word16) =>
    IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (Contradictions a))
contradictions = (noPossibilities <|.|> possibilitiesWithLoc <|.|> countedKnowns) <. to (>>>> mkContra)
  where
    mkContra (el, (cp, ck)) = Contradictions el cp ck
{-# INLINE contradictions #-}

factor ::
    (V.MVector st (SummaryRecord m a), V.MVector st (SummaryRecord m a), V.MVector st (SummaryRecord m a))
    -> ST st (Summary m a)
factor v = do
    ( (rs, rs', rs'')
        , (cs, cs', cs'')
        , (bs, bs', bs'')
        ) <-
        munzip33 v
    (,,) <$> mk rs cs bs <*> mk rs' cs' bs' <*> mk rs'' cs'' bs''
  where
    -- unsafeFreeze is safe here because we only read from the vector afterwards.
    mkCs v' = CellSummaries <$> V.unsafeFreeze v'
    mk r c b = RegionSummaries <$> mkCs r <*> mkCs c <*> mkCs b
{-# INLINE factor #-}

type Solved a = Union (CellSet a)

type Summary m a = (RegionSummaries (Contradictions a), RegionSummaries (Solved a), RegionSummaries m)

type ValueConstraint a =
    (VU.Unbox a, VU.IsoUnbox a Word16, VU.Unbox a, Enum a, Bounded a, Ord a, TextShow a, Hashable a, NFData a)

-- | unzip 3 mutable vectors of the same length via a single loop.
munzip33 ::
    (MVG.MVector v (a, b, c), MVG.MVector v a, MVG.MVector v b, MVG.MVector v c) =>
    (v st (a, b, c), v st (a, b, c), v st (a, b, c))
    -> ST st ((v st a, v st b, v st c), (v st a, v st b, v st c), (v st a, v st b, v st c))
munzip33 (v1, v2, v3) = do
    let len = MVG.basicLength v1
    (v1', v2', v3') <- (,,) <$> mk len <*> mk len <*> mk len
    forM_ [0 .. (len - 1)] $ \i -> do
        (a, b, c) <- (,,) <$> MVG.read v1 i <*> MVG.read v2 i <*> MVG.read v3 i
        upd v1' i a *> upd v2' i b *> upd v3' i c
    return (v1', v2', v3')
  where
    mk len = (,,) <$> MVG.new len <*> MVG.new len <*> MVG.new len
    upd (as, bs, cs) i (a, b, c) =
        MVG.write as i a *> MVG.write bs i b *> MVG.write cs i c
{-# INLINE munzip33 #-}

{- | provides three summaries:
  1. a summary to check whether the grid is contradicted
  2. a summary to check whether the grid is solved
  3. the requested summary for the simplifier
-}
summarizeWithContradictions ::
    (IsoUnbox a Word16, Ord a, Enum a, Monoid m) =>
    IndexedTraversal' CellPos s (Cell a) -> Fold (CellPos, Cell a) (AcrossRegion m) -> s -> Summary m a
summarizeWithContradictions l m s = runST $ summaryOfM (l . withIndex . (contradictions <|.|> solved <|.|> m) . to (>>>> normalize)) s >>= factor
  where
    normalize (c, (s', m')) = (c, s', m')
{-# INLINE summarizeWithContradictions #-}

solved :: (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) (AcrossRegion (Solved a))
solved = knowns
{-# INLINE solved #-}

checkSolved :: (Enum a) => RegionSummaries (Solved a) -> Bool
checkSolved summs = checkAcross Row && checkAcross Column && checkAcross Box
  where
    checkAcross ri = V.foldMap' (view (_Union . from _Intersects)) (summs ^. ix ri . byRegion) == mempty
{-# INLINE checkSolved #-}

data ContradictionDesc i a
    = DigitRepeats !a !RegionIndicator !i
    | SharedCell !a !a !CellPos
    | CannotPlace !a !RegionIndicator !i
    | NoFillForCell !CellPos
    deriving (Eq, Ord, Generic)

instance (NFData i, NFData a) => NFData (ContradictionDesc i a)

instance (Hashable i, Hashable a) => Hashable (ContradictionDesc i a)

instance (TextShow a, TextShow i, Num i) => TextShow (ContradictionDesc i a) where
    showb (DigitRepeats a ri i) = "Repeats " <> showb a <> " in " <> showb ri <> " " <> showb (i + 1)
    showb (SharedCell a a' loc) = "digits must occupy the same cell: digits: " <> showb a <> ", " <> showb a' <> ", in cell " <> showLocB loc
    showb (CannotPlace a ri i) = "Cannot place " <> showb a <> " in " <> showb ri <> " " <> showb (i + 1)
    showb (NoFillForCell loc) = "no fill for cell: " <> showLocB loc

instance (TextShow a, TextShow i, Num i) => Show (ContradictionDesc i a) where
    show = toString . showb

findDigitRepeatsOn ::
    (Ord a, Hashable a, Enum a) =>
    RegionIndicator -> Int -> Int -> Sum Int -> Maybe (ContradictionDesc Int a)
findDigitRepeatsOn !ri !l ~d !known
    | known > 1 = Just $! DigitRepeats (toEnum d) ri l
    | otherwise = Nothing

digitsForcedIntoCells ::
    forall a.
    (Ord a, Hashable a, Enum a) =>
    HS.HashSet (ContradictionDesc Int a)
    -> RegionIndicator
    -> Int
    -> M.MonoidalIntMap (Union (CellSet Word8))
    -> M.MonoidalIntMap (Sum Int)
    -> HS.HashSet (ContradictionDesc Int a)
digitsForcedIntoCells !res !ri !i !poss !known = ifoldl' describe res singletonsByLoc
  where
    sel = itraversed <. _Union . _CellSet
    whenSingleton d ps = A.BS.size ps == 1 && M.lookup d known < Just 1
    singletonSel =
        sel . ifiltered whenSingleton <. bsfolded . from enumerated . to (ri,i,) . from _majorMinor . _2
    {-# INLINE singletonSel #-}
    mergeSingletons d r loc = HM.insert loc (d : concat (HM.lookup loc r)) r
    !singletonsByLoc = ifoldlOf' (reindexed toEnum singletonSel) mergeSingletons HM.empty poss
    describe loc r = \case
        d : d' : _ -> HS.insert (SharedCell d d' loc) r
        _ -> r
{-# SPECIALIZE digitsForcedIntoCells ::
    HS.HashSet (ContradictionDesc Int Digit)
    -> RegionIndicator
    -> Int
    -> M.MonoidalIntMap (Union (CellSet Word8))
    -> M.MonoidalIntMap (Sum Int)
    -> HS.HashSet (ContradictionDesc Int Digit)
    #-}

digitContradicted ::
    (Ord a, Enum a) =>
    M.MonoidalIntMap (Union (CellSet Word8))
    -> M.MonoidalIntMap (Sum Int)
    -> a
    -> Bool
digitContradicted !poss !known ~d
    -- if the digit isn't known anywhere in the region
    | M.lookup (fromEnum d) known < Just 1 =
        -- and it's not possible anywhere in the region, then it's been contradicted
        nullOf (folded . _Union . _CellSet . bsfolded) (M.lookup (fromEnum d) poss)
    | otherwise = False

findDigitContradiction ::
    (Ord a, Hashable a, Enum a) =>
    RegionIndicator
    -> Int
    -> M.MonoidalIntMap (Union (CellSet Word8))
    -> M.MonoidalIntMap (Sum Int)
    -> a
    -> Maybe (ContradictionDesc Int a)
findDigitContradiction !ri !l !poss !known ~d
    | digitContradicted poss known d = Just $! CannotPlace d ri l
    | otherwise = Nothing

regionalContradictionsTest ::
    forall a.
    (Ord a, Enum a, Bounded a, Hashable a) =>
    RegionIndicator
    -> HS.HashSet (ContradictionDesc Int a)
    -> Int
    -> Contradictions a
    -> HS.HashSet (ContradictionDesc Int a)
regionalContradictionsTest !ri !res !l (Contradictions !empt !poss !known) = digitContradictions
  where
    sel = _Union . _CellSet . bsfolded . from enumerated . to (ri,l,) . from _majorMinor . _2
    maybeIns = flip $ maybe <*> flip HS.insert
    !sharedCell = digitsForcedIntoCells res ri l poss known
    !cellContras = foldlOf' sel (flip (HS.insert . NoFillForCell)) sharedCell empt
    !repeatedDigits = ifoldl' (\i -> flip $ maybeIns . findDigitRepeatsOn ri l i) cellContras known
    !digitContradictions = F.foldl' (flip $ maybeIns . findDigitContradiction ri l poss known) repeatedDigits [minBound .. maxBound]
{-# SPECIALIZE regionalContradictionsTest ::
    RegionIndicator
    -> HS.HashSet (ContradictionDesc Int Digit)
    -> Int
    -> Contradictions Digit
    -> HS.HashSet (ContradictionDesc Int Digit)
    #-}

testForContradictions ::
    (TextShow a, Ord a, Enum a, Bounded a, Hashable a, NFData a) =>
    RegionSummaries (Contradictions a) -> HS.HashSet (ContradictionDesc Int a)
testForContradictions !summs = testAcross Row <> testAcross Column <> testAcross Box
  where
    testAcross !ri = V.ifoldl' (regionalContradictionsTest ri) mempty (summs ^. ix ri . byRegion)
{-# INLINE testForContradictions #-}

lowerBound :: forall a. (Enum a, Bounded a) => Int
lowerBound = fromEnum (minBound @a)

upperBound :: forall a. (Enum a, Bounded a) => Int
upperBound = fromEnum (maxBound @a)

{- | if the possible locations for a digit within a set are aligned on intersecting sets (i.e. 2 only has two locations within a box and they're in the same column),
then remove that digit from other possible locations along the rest of the intersecting traversal.
-}
digitLocationsAlignedOn ::
    forall a.
    (Enum a, Bounded a, VU.Unbox a, Ord a, TextShow a) =>
    RegionIndicator -> Word8 -> LocationAlignment a -> Grid a -> Grid a
digitLocationsAlignedOn ri i (LocationAlignment possibles) g = foldl' update g [0 .. 8]
  where
    l = case ri of
        Row -> rowAt (fromIntegral i)
        Column -> colAt (fromIntegral i)
        Box -> boxAt (fromIntegral i)
    check = \cases
        Row d -> maybe False (allSame sameRow) (locs d)
        Column d -> maybe False (allSame sameCol) (locs d)
        Box d -> maybe False (allSame sameBox) (locs d)
    locs d = possibles ^? ix d
    mkL ri' =
        folding
            ( \i' ->
                Identity $
                    g & case ri' of
                        Row -> rowAt i' @\\ l
                        Column -> colAt i' @\\ l
                        Box -> boxAt i' @\\ l
            )
    disjointInds d
        | check Row d = locs d ^? _Just . _head . _1 . mkL Row
        | check Column d = locs d ^? _Just . _head . _2 . mkL Column
        | check Box d = locs d ^? _Just . _head . _3 . mkL Box
        | otherwise = Nothing
    update g' d = case disjointInds d of
        Just m -> removePossibilitiesOfOn (runIndexedTraversal m) (folded . enumerated) [d] g'
        Nothing -> g'
{-# INLINE digitLocationsAlignedOn #-}

simplifyFromDigitsAligned ::
    forall a.
    (VU.Unbox a, Enum a, Bounded a, Ord a, TextShow a) =>
    Lens' (Grid a) (VU.Vector (Cell a)) -> RegionSummaries (LocationAlignment a) -> Grid a -> Grid a
simplifyFromDigitsAligned _ summs = flip (foldl' (\g (ri, i) -> digitLocationsAlignedOn ri i (summFor ri i) g)) wholeGridBySet
  where
    summFor ri i = summs ^. ix ri . ix i
    wholeGridBySet =
        [(ri, i) | ri <- [Row, Column, Box], i <- [1 .. 9]]

{-# RULES
"unionWith/tipR" forall f t1. MS.unionWithKey f t1 MM.Nil = t1
"unionWith/tipL" forall f t1. MS.unionWithKey f MM.Nil t1 = t1
    #-}
