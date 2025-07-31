{-# OPTIONS_GHC -Wno-orphans #-}

{- | this module provides monoidal summaries of a grid. these summaries provide the workhorses we use to actually define simplifier rules
and are therefore the basic building blocks of the solver. the idea is to only process the grid twice per step. we'd love to do once per step
but applying the updates can't happen at the same time as info collection because we need non-local information when determining what update
to apply to each cell -- i.e. a summary of each cell's `Row`, `Column`, and `Box`. so we instead collect summaries monoidally, appending
a single cell's summary to the summaries for each of its `Row`/`Column`/`Box`.
-}
module Sudoku.Summaries where

import Control.Applicative ((<**>))
import Control.Lens
import Control.Monad.ST (ST, runST)
import Data.Coerce (coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.Default (Default (def))
import Data.Functor (($>))
import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Vector.Generic.Lens (vectorIx, vectorTraverse)
import Data.Vector.Unboxed (IsoUnbox)
import Data.Word (Word16, Word8)
import GHC.Base (inline)
import GHC.Generics (Generic)
import Sudoku.Cell
import Sudoku.Grid (
    Grid,
    allIndicesV,
    allSame,
    boxAt,
    colAt,
    ensure,
    grid,
    removePossibilitiesOfOn,
    rowAt,
    sameBox,
    sameCol,
    sameRow,
    (@\\),
 )
import TextShow (TextShow (showb), toString)
import TextShow.Data.List (showbListWith)

import Data.BitSet qualified as A.BS
import Data.Containers.ListUtils qualified as L
import Data.List qualified as L
import Data.Map.Monoidal.Strict qualified as M
import Data.Map.Strict.Internal qualified as MM
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as MVU

{- | flipped double composition. applies the one-argument function on the right to the result of the two-argument function
on the left after both arguments are applied to the function on the left.

i.e. `f >>>> g = \x y -> g (f x y)`
-}
(>>>>) :: (a1 -> a2 -> b) -> (b -> c) -> a1 -> a2 -> c
(>>>>) = flip $ (.) . (.)
{-# INLINE (>>>>) #-}

infixr 1 >>>>

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
    unionEmpty :: a
    intersection :: a -> a -> a
    intersectionEmpty :: a

instance (Enum a) => SetLike (CellSet a) where
    !x `union` !y = CellSet $! _bitSet x `A.BS.union` _bitSet y
    {-# INLINE union #-}
    unionEmpty = CellSet A.BS.empty
    {-# INLINE unionEmpty #-}
    !x `intersection` !y = CellSet $! _bitSet x `A.BS.intersection` _bitSet y
    {-# INLINE intersection #-}
    intersectionEmpty = CellSet $! A.BS.fromList [toEnum 0 .. toEnum 8]
    {-# INLINE intersectionEmpty #-}

instance (SetLike a) => Semigroup (Union a) where
    !x <> !y = Union $! runUnion x `union` runUnion y
    {-# INLINE CONLIKE (<>) #-}

instance (SetLike a) => Semigroup (Intersects a) where
    Intersects !x <> Intersects !y = Intersects $! x `intersection` y
    {-# INLINE (<>) #-}

instance (SetLike a) => Monoid (Union a) where
    mempty = Union unionEmpty
    {-# INLINE mempty #-}

instance (SetLike a) => Monoid (Intersects a) where
    mempty = Intersects intersectionEmpty
    {-# INLINE mempty #-}

newtype OccursN a = OccursN {occursN :: (Word8, a)}
    deriving (Generic)

makePrisms ''OccursN

instance Semigroup (OccursN a) where
    OccursN (count, x) <> OccursN (count', _) = OccursN (count + count', x)
    {-# INLINE (<>) #-}

instance (Monoid a) => Monoid (OccursN a) where
    mempty = OccursN (0, mempty)

newtype LocationAlignment a = LocationAlignment {locations :: M.MonoidalMap a (V.Vector CellPos)}

deriving via (M.MonoidalMap a (V.Vector CellPos)) instance (Ord a) => Semigroup (LocationAlignment a)

deriving via (M.MonoidalMap a (V.Vector CellPos)) instance (Ord a) => Monoid (LocationAlignment a)

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
    { _emptyLocs :: {-# UNPACK #-} ![CellPos]
    , _possLocs :: {-# UNPACK #-} !(M.MonoidalMap a [CellPos])
    , _knownLocs :: {-# UNPACK #-} !(M.MonoidalMap a (Sum Int))
    }
    deriving (Eq, Ord)

instance (Ord a) => Semigroup (Contradictions a) where
    Contradictions e p k <> Contradictions e' p' k' = Contradictions (e <> e') (p <> p') (k <> k')
    {-# INLINE CONLIKE (<>) #-}

instance (Ord a) => Monoid (Contradictions a) where
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

type instance Index (CellSummaries a) = Int

instance Ixed (CellSummaries a) where
    ix i = byRegion . vectorIx (i - 1)
    {-# INLINE ix #-}

type instance IxValue (RegionSummaries a) = CellSummaries a

type instance Index (RegionSummaries a) = RegionIndicator

instance Ixed (RegionSummaries a) where
    ix Row = rows
    ix Column = columns
    ix Box = boxes
    {-# INLINE ix #-}

riToLens :: RegionIndicator -> Lens (RegionSummaries a) (RegionSummaries a) (CellSummaries a) (CellSummaries a)
riToLens Row = rows
riToLens Column = columns
riToLens Box = boxes
{-# INLINE riToLens #-}

{- | assuming the monoid instance for `a` is associative, this function provides an `ifoldl'` function that can be used over an entire
grid to calculate a summary of the grid, organized by region. rather than using the inefficient `Monoid` instance on `CellSummaries`,
it instead updates each region the cell belongs to by indexing `CellSummaries` to get the current summary of each region type and
updates it by `mappend`ing `a` on the right. this relies on the associativity of the `Monoid` instance for `a`. a non-law abiding
`Monoid` instance won't produce the expected results.
-}
addToSummary ::
    forall m v s.
    (Monoid m, MVG.MVector v m) =>
    CellPos
    -> (v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m)
    -> m
    -> ST s (v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m, v (MVG.PrimState (ST s)) m)
addToSummary (r, c, b) (rs, cs, bs) m = upd $> (rs, cs, bs)
  where
    upd =
        MVG.modify rs (<> m) (fromIntegral r - 1)
            *> MVG.modify cs (<> m) (fromIntegral c - 1)
            *> MVG.modify bs (<> m) (fromIntegral b - 1)
{-# INLINE addToSummary #-}

summaryOfM ::
    (Monoid m, MVG.MVector v m) =>
    IndexedFold CellPos s m
    -> s
    -> (forall st. ST st (v (MVG.PrimState (ST st)) m, v (MVG.PrimState (ST st)) m, v (MVG.PrimState (ST st)) m))
summaryOfM l s = (,,) <$> initialize <*> initialize <*> initialize >>= flip (ifoldlMOf l addToSummary) s
  where
    initialize = do
        v <- MVG.new 9
        MVG.set v mempty $> v
{-# INLINE summaryOfM #-}

{- | produce a summary for an entire grid. takes the `Grid` vector and a function that maps each `Cell` to a Monoidal summary type `a`, carrying an index that
provides the necessary information about the `Cell`'s row/column/box. the summary type chosen determines the information available at the end of the fold.
for example, mapping each `Cell` to a singleton `CellSet` of `Known` digits wrapped in a `Union` newtype will produce the set of already known digits
in each row/column/box.
-}
summaryOf :: (Monoid m) => IndexedFold CellPos s m -> s -> RegionSummaries m
summaryOf l s = runST (summaryOfM l s >>= frzAll <&> mk)
  where
    frzAll (rs, cs, bs) = (,,) <$> VG.freeze rs <*> VG.freeze cs <*> VG.freeze bs

    mk (r, c, b) = RegionSummaries (CellSummaries r) (CellSummaries c) (CellSummaries b)
{-# INLINE summaryOf #-}

-- | apply updates derived from `RegionSummaries` to `s` as determined by the function `f` at each position (`CellPos`, `Cell`) in `s`.
applySummary ::
    (VU.Unbox a) =>
    (RegionSummaries b -> CellPos -> a -> a) -> Lens' s (VU.Vector a) -> RegionSummaries b -> s -> s
applySummary f sel summs = sel . cellPos %@~ f summs
{-# INLINE applySummary #-}

explainSummary :: (Ord a) => (RegionIndicator -> Int -> b -> [ExplainDesc a]) -> RegionSummaries b -> [ExplainDesc a]
explainSummary f summs = nubOrd . sort $ explainAcross Row <> explainAcross Column <> explainAcross Box
  where
    explainAcross ri = ifoldMapOf (ix ri . byRegion . vectorTraverse) (f ri) summs
{-# INLINE explainSummary #-}

cellSetList :: (Enum a) => Iso' [a] (CellSet a)
cellSetList = iso (CellSet . A.BS.fromList) (A.BS.toList . _bitSet)
{-# INLINE cellSetList #-}

-- | creates an update set by concatenating `CellSummaries` for a given cell position
updateSet :: (Monoid b) => RegionSummaries b -> CellPos -> b
updateSet summs (r, c, b) =
    summs ^. ix Row . ix (fromIntegral r)
        <> summs ^. ix Column . ix (fromIntegral c)
        <> summs ^. ix Box . ix (fromIntegral b)
{-# INLINE updateSet #-}

cellUpdating :: (CellPos -> RegionSummaries b -> Maybe a) -> IndexedFold CellPos (RegionSummaries b) a
cellUpdating f = ifolding (\summs -> V.mapMaybe (\loc -> fmap (loc,) (f loc summs)) allIndicesV)
{-# INLINE cellUpdating #-}

-- this doesn't include everything... need to figure out how to make the tuples search fit into this scheme
data ExplainDesc a
    = AlreadyKnown !RegionIndicator !Int !(A.BS.BitSet Word16 a)
    | SinglePoss !CellPos !a
    | SingleLoc !RegionIndicator !Int !(A.BS.BitSet Word16 a)
    | LookedForPointing
    | TupleDesc !RegionIndicator !Int !(A.BS.BitSet Word16 a) !(A.BS.BitSet Word16 a)
    deriving (Eq, Ord, Generic)

instance (TextShow a, Enum a) => TextShow (ExplainDesc a) where
    showb (AlreadyKnown ri i cs) = "In " <> showb ri <> " " <> showb (i + 1) <> ": " <> showb (A.BS.toList cs) <> " are already known."
    showb (SinglePoss loc d) = "cell " <> showLocB loc <> " can only take the value " <> showb d
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
    | cs /= A.BS.empty && cs /= allDigits =
        [AlreadyKnown ri i cs]
    | otherwise = []
  where
    allDigits = A.BS.fromList [toEnum 0 .. toEnum 8]
{-# INLINE explainKnowns #-}

updateFromKnowns :: (Enum a, VU.Unbox a) => RegionSummaries (Union (CellSet a)) -> CellPos -> Cell a -> Cell a
updateFromKnowns summs loc = _Possibly . _CellSet %~ (A.BS.\\ updates)
  where
    updates = updateSet summs loc ^. _Union . _CellSet
{-# INLINE updateFromKnowns #-}

updateFromNakedSingles ::
    (VU.Unbox a, Enum a, Bounded a, VU.IsoUnbox a Word16) => RegionSummaries b -> CellPos -> Cell a -> Cell a
updateFromNakedSingles _ _ (Possibly (CellSet cs)) | A.BS.size cs == 1 = mkKnown $ fromJust (A.BS.first cs)
updateFromNakedSingles _ _ cell = cell
{-# INLINE updateFromNakedSingles #-}

explainNakedSingles ::
    (Enum a, TextShow a, VU.Unbox a) => RegionIndicator -> Int -> [(CellPos, a)] -> [ExplainDesc a]
explainNakedSingles _ _ = foldMap (\(loc, d) -> [SinglePoss loc d])
{-# INLINE explainNakedSingles #-}

singlePossibility :: (VU.Unbox a, Enum a) => IndexedFold CellPos (CellPos, Cell a) [(CellPos, a)]
singlePossibility = ifolding (\(loc, cell) -> Identity . (loc,) $ singlePoss loc cell)
  where
    singlePoss loc = \cases
        (Possibly (CellSet cs)) | A.BS.size cs == 1 -> [(loc, A.BS.toList cs ^. singular _head)]
        _ -> []
{-# INLINE singlePossibility #-}

filterSummaryByCount ::
    (Eq a) => RegionIndicator -> a -> RegionSummaries (M.MonoidalMap k (Sum a)) -> RegionSummaries (M.MonoidalMap k (Sum a))
filterSummaryByCount ri c = riToLens ri . byRegion . vectorTraverse %~ M.filter (== Sum c)
{-# INLINE filterSummaryByCount #-}

filterSummariesByCount ::
    Int -> RegionSummaries (M.MonoidalMap k (Sum Int)) -> RegionSummaries (M.MonoidalMap k (Sum Int))
filterSummariesByCount c = filterSummaryByCount Row c . filterSummaryByCount Column c . filterSummaryByCount Box c
{-# INLINE filterSummariesByCount #-}

findInSummary ::
    (Enum k) => RegionSummaries (M.MonoidalMap k (Sum Int)) -> RegionIndicator -> Word8 -> A.BS.BitSet Word16 k
findInSummary summs ri i = A.BS.fromList $ summs ^.. ix ri . ix (fromIntegral i) . itraversed . asIndex
{-# INLINE findInSummary #-}

{- | find the first value in the intersection between a `Cell`'s possibilities and a set of `Digit`s.
if the `Cell` is already `Known`, or if the intersection is empty, give back the the original `Cell`.
if a value could be found, that value is marked as `Known`.

this works because the provided set of `Digit`s is exactly those values that only have a single home in
the `Cell`'s `Row`\/`Column`\/`Box`. we ignore the possibility of multiple values needing to live in the
same `Cell` here because this is handled during contradiction checking.
-}
markUniqueKnown :: (Enum a, Bounded a, VU.IsoUnbox a Word16) => CellSet a -> Cell a -> Cell a
markUniqueKnown (CellSet cs) = intersect (const Nothing) <**> maybeMarkKnown . const
  where
    intersect = outside (_Possibly . _CellSet) .~ A.BS.first . A.BS.intersection cs
    maybeMarkKnown = outside _Just .~ mkKnown
{-# INLINE markUniqueKnown #-}

{- | find `Digit`s in each `Row`\/`Column`\/`Box` that can take only a single position and set those `Cell`s to that `Digit`.
this function assumes the `RegionSummaries` has already been filtered such that it only contains those entries where `Digit`s can
only take a single position.
-}
updateFromHiddenSingles ::
    (Enum a, VU.Unbox a, VU.IsoUnbox a Word16, Bounded a) =>
    RegionSummaries (M.MonoidalMap a (Sum Int)) -> CellPos -> Cell a -> Cell a
updateFromHiddenSingles = findUpdates >>>> markUniqueKnown
  where
    -- `A.BS.union` together the values with only one possible location in this cell's regions -- if this cell
    -- can take any of these values, we'll mark it `Known` here.
    findUpdates s (r, c, b) = CellSet $! foldMap (uncurry (findInSummary s)) [(Row, r), (Column, c), (Box, b)]
{-# INLINE updateFromHiddenSingles #-}

explainHiddenSingles ::
    (Enum a, TextShow a, Ord a) => RegionIndicator -> Int -> M.MonoidalMap a (Sum Int) -> [ExplainDesc a]
explainHiddenSingles ri i summ
    | not (null summ) =
        [SingleLoc ri i (A.BS.fromList (summ ^.. itraversed . asIndex))]
    | otherwise = []
{-# INLINE explainHiddenSingles #-}

-- | a `Fold` that tabulates the values already `Known` across the `Cell`'s row/column/box
knowns :: (Enum a, VU.IsoUnbox a Word16) => Fold (Cell a) (Union (CellSet a))
knowns = _Known . to A.BS.singleton . from (_Union . _CellSet)
{-# INLINE knowns #-}

-- | a `Fold` producing a map for each `Cell` that tracks how many possible location for a value remain, across it's Row/Column/Box
countedPoss :: (Enum a, Ord a) => Fold (Cell a) (M.MonoidalMap a (Sum Int))
countedPoss = _Possibly . _CellSet . to (foldlOf' bsfolded ins mempty)
  where
    ins m d = M.insert d (Sum 1) m
{-# INLINE countedPoss #-}

{- | a `Fold` producing a map for each `Cell` that tracks how many times a value has been marked as Known, across it's Row/Column/Box
this should never exceed 1!
-}
countedKnowns :: (Enum a, Ord a, VU.IsoUnbox a Word16) => Fold (Cell a) (M.MonoidalMap a (Sum Int))
countedKnowns = _Known . to (`M.singleton` Sum 1)
{-# INLINE countedKnowns #-}

-- | a `Fold` producing a map of digit locations within a region of the grid
valueAlignment :: (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (LocationAlignment a)
valueAlignment = ifolding (\(loc, cell) -> Just (loc, allocateLocs loc cell))
  where
    allocateLocs loc cell = LocationAlignment $ foldlOf' (_Possibly . _CellSet . bsfolded) (\r d -> M.insert d (V.singleton loc) r) M.empty cell
{-# INLINE valueAlignment #-}

{- | compose two `Fold`s operating on the same element type but that need access to the index from the parent `Traversal`.
the `Fold`s will need to receive the index from the parent `Traversal` via `withIndex`, so the indexed functions the
`Fold`s are built out of should be uncurried. this trick only works for `Fold`s. `IndexedFold`s that have their own
indices cannot be composed in this way as their separate internal indices need to be composed in some way, even when
they produce different numbers of elements.

because this operator folds down to `Monoid`s on both sides of the pair, the values it produces are indexed by the parent
`Traversal` -- it produces a single paired value for every element of the parent `Traversal`.
-}
(<|.|>) :: (Monoid m, Monoid m') => Fold (i, s) m -> Fold (i, s) m' -> IndexedFold i (i, s) (m, m')
m <|.|> m' = ifolding @Identity (\(i, s) -> pure (i, (inline (foldOf m (i, s)), inline (foldOf m' (i, s)))))
{-# INLINE (<|.|>) #-}

infixr 8 <|.|>

{- | this operator is the same as `<|.|>` but it takes a fold that doesn't need access to the parent `Traversal`'s index on
the left.
-}
(|.>) :: (Monoid m, Monoid m') => Fold s m -> Fold (i, s) m' -> IndexedFold i (i, s) (m, m')
m |.> m' = ifolding @Identity (\(i, s) -> pure (i, (inline (foldOf m s), inline (foldOf m' (i, s)))))
{-# INLINE (|.>) #-}

infixr 9 |.>

{- | this operator is the same as `<|.|>` but it takes a `Fold` that doesn't need access to the parent `Traversal`'s index on
the right.
-}
(<.|) :: (Monoid m, Monoid m') => Fold (i, s) m -> Fold s m' -> IndexedFold i (i, s) (m, m')
m <.| m' = ifolding @Identity (\(i, s) -> pure (i, (inline (foldOf m (i, s)), inline (foldOf m' s))))
{-# INLINE (<.|) #-}

infixr 9 <.|

{- | this operator is the same as `<|.|>` but it takes a `Fold` that doesn't need access to the parent `Traversal`'s index on
both sides.
-}
(|.|) :: (Monoid m, Monoid m') => Fold s m -> Fold s m' -> IndexedFold i (i, s) (m, m')
m |.| m' = ifolding @Identity (\(i, s) -> pure (i, (inline (foldOf m s), inline (foldOf m' s))))
{-# INLINE (|.|) #-}

infixr 8 |.|

type SummaryRecord m a = (Contradictions a, Union (CellSet a), m)

noPossibilities :: (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) [CellPos]
noPossibilities = ifolding @Maybe (\(loc, cell) -> (loc, [loc]) <$ ensure cond cell)
  where
    cond = (&&) <$> hasn't (_Possibly . _CellSet . bsfolded) <*> hasn't _Known
{-# INLINE noPossibilities #-}

possibilitiesWithLoc :: (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (M.MonoidalMap a [CellPos])
possibilitiesWithLoc = ifolding (\(loc, cell) -> Identity (loc, posses loc cell))
  where
    ins loc r a = M.insert a [loc] r
    posses loc = foldlOf' (_Possibly . _CellSet . bsfolded) (ins loc) mempty
{-# INLINE possibilitiesWithLoc #-}

contradictions :: (Enum a, Ord a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) (Contradictions a)
contradictions = (noPossibilities <|.|> possibilitiesWithLoc <.| countedKnowns) . to mkContra
  where
    mkContra (el, (cp, ck)) = Contradictions el cp ck
{-# INLINE contradictions #-}

factor ::
    (V.MVector st (SummaryRecord m a), V.MVector st (SummaryRecord m a), V.MVector st (SummaryRecord m a))
    -> ST st (Summary m a)
factor (vr, vc, vb) = do
    (rs, rs', rs'') <- munzip3 vr
    (cs, cs', cs'') <- munzip3 vc
    (bs, bs', bs'') <- munzip3 vb
    (,,) <$> mk rs cs bs <*> mk rs' cs' bs' <*> mk rs'' cs'' bs''
  where
    mkCs v = CellSummaries <$> V.freeze v
    mk r c b = RegionSummaries <$> mkCs r <*> mkCs c <*> mkCs b
{-# INLINE factor #-}

type Solved a = Union (CellSet a)

type Summary m a = (RegionSummaries (Contradictions a), RegionSummaries (Solved a), RegionSummaries m)

type ValueConstraint a = (VU.Unbox a, VU.IsoUnbox a Word16, VU.Unbox a, Enum a, Bounded a, Ord a, TextShow a)

munzip3 ::
    (MVG.MVector v (a, b, c), MVG.MVector v a, MVG.MVector v b, MVG.MVector v c) =>
    v st (a, b, c)
    -> ST st (v st a, v st b, v st c)
munzip3 v = do
    let len = MVG.basicLength v
    v' <- (,,) <$> MVG.new len <*> MVG.new len <*> MVG.new len
    MVG.ifoldM' upd v' v
  where
    upd (as', bs', cs') i (a, b, c) = MVG.write as' i a *> MVG.write bs' i b *> MVG.write cs' i c $> (as', bs', cs')
{-# INLINE munzip3 #-}

{- | provides three summaries:
  1. a summary to check whether the grid is contradicted
  2. a summary to check whether the grid is solved
  3. the requested summary for the simplifier
-}
summarizeWithContradictions ::
    (IsoUnbox a Word16, Ord a, Enum a, Monoid m) =>
    IndexedTraversal' CellPos s (Cell a) -> Fold (CellPos, Cell a) m -> s -> Summary m a
summarizeWithContradictions l m s = runST $ summaryOfM (l . withIndex . summ . to normalize) s >>= factor
  where
    summ = contradictions <.| solved <|.|> m
    normalize ((c, s'), m') = (c, s', m')
{-# INLINE summarizeWithContradictions #-}

solved :: (Enum a, VU.IsoUnbox a Word16) => Fold (Cell a) (Solved a)
solved = knowns
{-# INLINE solved #-}

ixSolved :: (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (Grid a) (Solved a)
ixSolved = reindexed rowColumn . indexing $ grid . vectorTraverse . solved
{-# INLINE ixSolved #-}

-- | `A.BS.BitSet` with all digits marked
completeDigitSet :: A.BS.BitSet Word16 a
completeDigitSet = A.BS.BitSet 0b111111111

checkSolved :: (Enum a) => RegionSummaries (Solved a) -> Bool
checkSolved summs = checkAcross Row && checkAcross Column && checkAcross Box
  where
    checkAcross ri = V.all ((== completeDigitSet) . coerce) (summs ^. ix ri . byRegion)
{-# INLINE checkSolved #-}

data ContradictionDesc i a
    = DigitRepeats !a !RegionIndicator !i
    | SharedCell !a !a !CellPos
    | CannotPlace !a !RegionIndicator !i
    | NoFillForCell !CellPos
    deriving (Eq, Ord, Generic)

instance (TextShow a, TextShow i, Num i) => TextShow (ContradictionDesc i a) where
    showb (DigitRepeats a ri i) = "Repeats " <> showb a <> " in " <> showb ri <> " " <> showb (i + 1)
    showb (SharedCell a a' loc) = "digits must occupy the same cell: digits: " <> showb a <> ", " <> showb a' <> ", in cell" <> showLocB loc
    showb (CannotPlace a ri i) = "Cannot place " <> showb a <> " in " <> showb ri <> " " <> showb (i + 1)
    showb (NoFillForCell loc) = "no fill for cell: " <> showLocB loc

instance (TextShow a, TextShow i, Num i) => Show (ContradictionDesc i a) where
    show = toString . showb

findDigitRepeatsOn :: (TextShow a) => RegionIndicator -> Int -> a -> Sum Int -> [ContradictionDesc Int a]
findDigitRepeatsOn ri l d (Sum ks)
    | ks > 1 = L.singleton (DigitRepeats d ri l)
    | otherwise = mempty

digitsInSameCell :: (TextShow a) => (a, a, CellPos) -> ContradictionDesc i a
digitsInSameCell (d, d', loc) = SharedCell d d' loc

digitsForcedIntoCells ::
    (TextShow a, Ord a) => M.MonoidalMap a [CellPos] -> M.MonoidalMap a (Sum Int) -> [ContradictionDesc i a]
digitsForcedIntoCells poss known = fmap digitsInSameCell singlePoss
  where
    singleLoc = poss ^@.. itraversed . ifiltered (\d ps -> length ps == 1 && known M.!? d < Just 1)
    singlePoss = [(d, d', loc ^. singular _head) | (d, loc) <- singleLoc, (d', loc') <- singleLoc, d < d', loc == loc']

digitContradicted :: (TextShow a, Ord a) => M.MonoidalMap a [CellPos] -> M.MonoidalMap a (Sum Int) -> a -> Bool
digitContradicted poss known d
    | known M.!? d < Just 1 = nullOf (folded . folded) (poss M.!? d)
    | otherwise = False

findDigitContradiction ::
    (TextShow a, Ord a) =>
    RegionIndicator -> Int -> M.MonoidalMap a [CellPos] -> M.MonoidalMap a (Sum Int) -> a -> Maybe (ContradictionDesc Int a)
findDigitContradiction ri l poss known d
    | digitContradicted poss known d = Just (CannotPlace d ri l)
    | otherwise = Nothing

regionalContradictionsTest ::
    (TextShow a, Ord a, Enum a) => RegionIndicator -> Int -> Contradictions a -> [ContradictionDesc Int a]
regionalContradictionsTest ri l (Contradictions empt poss known) = cellContras <> repeatedDigits <> digitContradictions <> sharedCell
  where
    sharedCell = digitsForcedIntoCells poss known
    cellContras = foldMap (\loc -> [NoFillForCell loc]) empt
    repeatedDigits = ifoldMap (findDigitRepeatsOn ri l) known
    digitContradictions = mapMaybe (findDigitContradiction ri l poss known) [toEnum 0 .. toEnum 8]
{-# INLINE regionalContradictionsTest #-}

testForContradictions :: (TextShow a, Ord a, Enum a) => RegionSummaries (Contradictions a) -> [ContradictionDesc Int a]
testForContradictions summs = L.nubOrd . L.sort $ foldMap testAcross [Row, Column, Box]
  where
    testAcross ri = ifoldMapOf (riToLens ri . byRegion . vectorTraverse) (regionalContradictionsTest ri) summs
{-# SPECIALIZE testForContradictions :: RegionSummaries (Contradictions Digit) -> [ContradictionDesc Int Digit] #-}

{- | if the possible locations for a digit within a set are aligned on intersecting sets (i.e. 2 only has two locations within a box and they're in the same column),
then remove that digit from other possible locations along the rest of the intersecting traversal.
-}
digitLocationsAlignedOn ::
    forall a.
    (Enum a, VU.Unbox a, Ord a, TextShow a) => RegionIndicator -> Word8 -> LocationAlignment a -> Grid a -> Grid a
digitLocationsAlignedOn ri i (LocationAlignment possibles) g = foldl' update g [toEnum 0 .. toEnum 8]
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
    inSet d
        | check Row d = locs d ^? _Just . _head . _1 . to rowAt . to (@\\ l) . to ($ g)
        | check Column d = locs d ^? _Just . _head . _2 . to colAt . to (@\\ l) . to ($ g)
        | check Box d = locs d ^? _Just . _head . _3 . to boxAt . to (@\\ l) . to ($ g)
        | otherwise = Nothing
    disjointInds m = foldMapOf (runIndexedTraversal m . asIndex) (L.singleton . showLoc) g
    update g' d = case ensure (not . null . disjointInds) =<< inSet d of
        Just m -> removePossibilitiesOfOn (runIndexedTraversal m) folded [d] g'
        Nothing -> g'
{-# INLINE digitLocationsAlignedOn #-}

simplifyFromDigitsAligned ::
    (VU.Unbox a, Enum a, Ord a, TextShow a) =>
    Lens' (Grid a) (VU.Vector (Cell a)) -> RegionSummaries (LocationAlignment a) -> Grid a -> Grid a
simplifyFromDigitsAligned _ summs = flip (foldl' (\g (ri, i) -> digitLocationsAlignedOn ri i (summFor ri i) g)) wholeGridBySet
  where
    summFor ri i = summs ^. riToLens ri . ixSummary (fromIntegral i)
    wholeGridBySet = [(ri, i) | ri <- [Row, Column, Box], i <- [1 .. 9]]

{-# RULES "unionWith/tipR" forall _f t1. MM.unionWith _f t1 MM.Tip = t1 #-}

{-# RULES "unionWith/tipL" forall _f t1. MM.unionWith _f MM.Tip t1 = t1 #-}
