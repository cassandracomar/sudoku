{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Sudoku.Summaries where

import Control.Lens
import Data.Containers.ListUtils (nubOrd)
import Data.Default (Default (def))
import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Text.Lazy (Text)
import Data.Vector.Generic.Lens (vectorIx, vectorTraverse)
import Data.Vector.Unboxed (IsoUnbox)
import Data.Word (Word16, Word8)
import Sudoku.Cell
import Sudoku.Grid (
    Grid,
    RegionIndicator (..),
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
import TextShow (TextShow (showb), toLazyText)

import Data.BitSet qualified as A.BS
import Data.Containers.ListUtils qualified as L
import Data.List qualified as L
import Data.Map.Monoidal.Strict qualified as M
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VG
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as MVU

newtype Union a = Union {runUnion :: a}
    deriving (Eq, Ord)

instance IsoUnbox (Union (CellSet a)) (CellSet a) where
    fromURepr = Union
    toURepr = runUnion

newtype instance VU.MVector s (Union a) = MV_Union (VU.MVector s Word16)

newtype instance VU.Vector (Union a) = V_Union (VU.Vector Word16)

deriving via (Union (CellSet a) `VU.As` CellSet a) instance VG.MVector MVU.MVector (Union (CellSet a))

deriving via (Union (CellSet a) `VU.As` CellSet a) instance VG.Vector VU.Vector (Union (CellSet a))

instance VU.Unbox (Union (CellSet a))

makePrisms ''Union

newtype Intersects a = Intersects {runIntersection :: a} deriving (Eq, Ord, VP.Prim)

instance IsoUnbox (Intersects (CellSet a)) (CellSet a) where
    fromURepr = Intersects
    toURepr = runIntersection

newtype instance VU.MVector s (Intersects a) = MV_Intersects (VU.MVector s Word16)

newtype instance VU.Vector (Intersects a) = V_Intersects (VU.Vector Word16)

deriving via (Intersects (CellSet a) `VU.As` CellSet a) instance VG.MVector MVU.MVector (Intersects (CellSet a))

deriving via (Intersects (CellSet a) `VU.As` CellSet a) instance VG.Vector VU.Vector (Intersects (CellSet a))

makePrisms ''Intersects

class SetLike a where
    union :: a -> a -> a
    unionEmpty :: a
    intersection :: a -> a -> a
    intersectionEmpty :: a

instance (Enum a) => SetLike (CellSet a) where
    x `union` y = CellSet $ _bitSet x `A.BS.union` _bitSet y
    unionEmpty = CellSet A.BS.empty
    x `intersection` y = CellSet $ _bitSet x `A.BS.intersection` _bitSet y
    intersectionEmpty = CellSet $ A.BS.fromList [toEnum 0 ..]

instance (SetLike a) => Semigroup (Union a) where
    x <> y = Union $ runUnion x `union` runUnion y
    {-# INLINE (<>) #-}

instance (SetLike a) => Semigroup (Intersects a) where
    x <> y = Intersects $ runIntersection x `union` runIntersection y
    {-# INLINE (<>) #-}

instance (SetLike a) => Monoid (Union a) where
    mempty = Union unionEmpty

instance (SetLike a) => Monoid (Intersects a) where
    mempty = Intersects intersectionEmpty

newtype OccursN a = OccursN {occursN :: (Word8, a)}

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

-- this is a very inefficient way to update the summaries as we come across them
-- i.e. to use this, you have to create a singleton summary where every entry is mempty except
-- the one you want to update, then fold. except because we're zipping over and over,
-- the generated machine code will involve a bunch of loops over mostly empty vectors.
-- so instead, there's an `addToSummary` function below that updates the summaries for
-- each region by indexing the vector and (<> a) the value to be combined.
instance (Semigroup a) => Semigroup (CellSummaries a) where
    x <> y = CellSummaries $ V.zipWith (<>) (_byRegion x) (_byRegion y)
    {-# INLINE (<>) #-}

instance (Monoid a) => Monoid (CellSummaries a) where
    mempty = CellSummaries (V.replicate 9 mempty)

data Contradictions a = Contradictions
    { _emptyLocs :: ![CellPos]
    , _possLocs :: !(M.MonoidalMap a [CellPos])
    , _knownLocs :: !(M.MonoidalMap a (Sum Int))
    }
    deriving (Eq, Ord)

instance (Ord a) => Semigroup (Contradictions a) where
    Contradictions e p k <> Contradictions e' p' k' = Contradictions (e <> e') (p <> p') (k <> k')
    {-# INLINE (<>) #-}

instance (Ord a) => Monoid (Contradictions a) where
    mempty = Contradictions mempty mempty mempty

makeLenses ''Contradictions

data RegionSummaries a = RegionSummaries
    { _rows :: !(CellSummaries a)
    , _columns :: !(CellSummaries a)
    , _boxes :: !(CellSummaries a)
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

riToLens :: RegionIndicator -> Lens (RegionSummaries a) (RegionSummaries a) (CellSummaries a) (CellSummaries a)
riToLens Row = rows
riToLens Column = columns
riToLens Box = boxes

{- | assuming the monoid instance for `a` is associative, this function provides an `ifoldl'` function that can be used over an entire
grid to calculate a summary of the grid, organized by region. rather than using the inefficient `Monoid` instance on `CellSummaries`,
it instead updates each region the cell belongs to by indexing `CellSummaries` to get the current summary of each region type and
updates it by `mappend`ing `a` on the right. this relies on the associativity of the `Monoid` instance for `a`. a non-law abiding
`Monoid` instance won't produce the expected results.
-}
addToSummary :: (Monoid a) => CellPos -> RegionSummaries a -> a -> RegionSummaries a
addToSummary (r, c, b) regions a = regions & rows . ixSummary r %~ (<> a) & columns . ixSummary c %~ (<> a) & boxes . ixSummary b %~ (<> a)
{-# INLINE addToSummary #-}

{- | produce a summary for an entire grid. takes an `IndexedFold` that maps each `Cell` to a Monoidal summary type `a`, carrying an index that
provides the necessary information about the `Cell`'s row/column/box. the summary type chosen determines the information available at the end of the fold.
for example, mapping each `Cell` to a singleton `CellSet` of `Known` digits wrapped in a `Union` newtype will produce the set of already known digits
in each row/column/box.
-}
summaryOf :: (Monoid a) => IndexedFold CellPos s a -> s -> RegionSummaries a
summaryOf l = ifoldlOf' l addToSummary (RegionSummaries mempty mempty mempty)
{-# INLINE summaryOf #-}

applySummaryOf ::
    (VU.Unbox a) =>
    IndexedFold CellPos (RegionSummaries b) c -> Lens' s (VU.Vector a) -> (a -> c -> a) -> RegionSummaries b -> s -> s
applySummaryOf l sel f summaries = sel %~ flip (VU.unsafeAccum f) updates
  where
    updates = itoListOf (reindexed vindex l) summaries
{-# INLINE applySummaryOf #-}

-- | apply updates derived from `RegionSummaries` to `s` as determined by the function `f` at each position (`CellPos`, `Cell`) in `s`.
applySummary ::
    (VU.Unbox a) =>
    (RegionSummaries b -> CellPos -> a -> a) -> Lens' s (VU.Vector a) -> RegionSummaries b -> s -> s
applySummary f sel summs g = g & sel %~ VU.imap (f summs . rowColumn)
-- where
--   upd = uncurry $ VU.zipWith (f summs . rowColumn)
{-# INLINE applySummary #-}

explainSummary :: (RegionIndicator -> Int -> b -> [Text]) -> RegionSummaries b -> [Text]
explainSummary f summs = nubOrd . sort $ explainAcross Row <> explainAcross Column <> explainAcross Box
  where
    explainAcross ri = ifoldMapOf (riToLens ri . byRegion . vectorTraverse) (f ri) summs

cellSetList :: (Enum a) => Iso' [a] (CellSet a)
cellSetList = iso (CellSet . A.BS.fromList) (A.BS.toList . _bitSet)
{-# INLINE cellSetList #-}

-- | creates an update set by concatenating `CellSummaries` for a given cell position
updateSet :: (Monoid b) => RegionSummaries b -> CellPos -> b
updateSet summs (r, c, b) = summs ^. rows . ixSummary r <> summs ^. columns . ixSummary c <> summs ^. boxes . ixSummary b
{-# INLINE updateSet #-}

cellUpdating :: (CellPos -> RegionSummaries b -> Maybe a) -> IndexedFold CellPos (RegionSummaries b) a
cellUpdating f = ifolding (\summs -> V.mapMaybe (\loc -> fmap (loc,) (f loc summs)) allIndicesV)
{-# INLINE cellUpdating #-}

explainKnowns :: (Enum a, TextShow a) => RegionIndicator -> Int -> Union (CellSet a) -> [Text]
explainKnowns ri i (Union (CellSet cs))
    | cs /= A.BS.empty && cs /= allDigits =
        [toLazyText $ "In " <> showb ri <> " " <> showb (i + 1) <> ": " <> showb (A.BS.toList cs) <> " are already known."]
    | otherwise = []
  where
    allDigits = A.BS.fromList [toEnum 0 ..]
{-# INLINE explainKnowns #-}

simplifyAllKnowns ::
    (VU.Unbox (Cell a), Enum a) => Lens' s (VU.Vector (Cell a)) -> RegionSummaries (Union (CellSet a)) -> s -> s
simplifyAllKnowns = applySummary updateFromKnowns
{-# INLINE simplifyAllKnowns #-}

updateFromKnowns :: (Enum a, VU.Unbox (Cell a)) => RegionSummaries (Union (CellSet a)) -> CellPos -> Cell a -> Cell a
updateFromKnowns summs loc = _Possibly . _CellSet %~ (A.BS.\\ (updateSet summs loc ^. _Union . _CellSet))

knownsUpdateSet ::
    (Enum a, VU.Unbox (Cell a)) => CellPos -> RegionSummaries (Union (CellSet a)) -> Maybe (Union (CellSet a))
knownsUpdateSet loc summs = ensure (/= mempty) (updateSet summs loc)

applyUpdateFromKnowns :: Cell a -> Union (CellSet a) -> Cell a
applyUpdateFromKnowns cell (Union (CellSet cs)) = cell & _Possibly . _CellSet %~ (A.BS.\\ cs)

simplifyNakedSingles ::
    (VU.Unbox (Cell a), Enum a) => Lens' s (VU.Vector (Cell a)) -> RegionSummaries b -> s -> s
simplifyNakedSingles = applySummary updateFromNakedSingles
{-# INLINE simplifyNakedSingles #-}

updateFromNakedSingles ::
    (VU.Unbox (Cell a), Enum a) => RegionSummaries b -> CellPos -> Cell a -> Cell a
updateFromNakedSingles _ _ (Possibly (CellSet cs)) | A.BS.size cs == 1 = mkKnown $ fromJust (A.BS.first cs)
updateFromNakedSingles _ _ cell = cell

nakedSinglesUpdateSet :: CellPos -> RegionSummaries b -> Maybe ()
nakedSinglesUpdateSet _ _ = Just ()

applyUpdateFromNakedSingles :: (Enum a, VU.Unbox (Cell a)) => Cell a -> () -> Cell a
applyUpdateFromNakedSingles (Possibly (CellSet cs)) _ | A.BS.size cs == 1 = mkKnown $ fromJust (A.BS.first cs)
applyUpdateFromNakedSingles cell _ = cell

explainNakedSingles :: (Enum a, TextShow a, VU.Unbox a) => RegionIndicator -> Int -> VU.Vector (CellPos, a) -> [Text]
explainNakedSingles _ _ = VU.foldMap (\(loc, d) -> [toLazyText $ "cell " <> showLocB loc <> " can only take the value " <> showb d])
{-# INLINE explainNakedSingles #-}

singlePossibility :: (VU.Unbox a, Enum a) => IndexedFold CellPos (CellPos, Cell a) (VU.Vector (CellPos, a))
singlePossibility = ifolding (\(loc, cell) -> Identity . (loc,) $ singlePoss loc cell)
  where
    singlePoss loc (Possibly (CellSet cs)) | A.BS.size cs == 1 = VU.singleton (loc, A.BS.toList cs ^. singular _head)
    singlePoss _ _ = VU.empty
{-# INLINE singlePossibility #-}

filterSummaryByCount ::
    (Eq a) => RegionIndicator -> a -> RegionSummaries (M.MonoidalMap k (Sum a)) -> RegionSummaries (M.MonoidalMap k (Sum a))
filterSummaryByCount ri c = riToLens ri . byRegion . vectorTraverse %~ M.filter (== Sum c)

filterSummariesByCount ::
    Int -> RegionSummaries (M.MonoidalMap k (Sum Int)) -> RegionSummaries (M.MonoidalMap k (Sum Int))
filterSummariesByCount c = filterSummaryByCount Row c . filterSummaryByCount Column c . filterSummaryByCount Box c

findInSummary ::
    (Enum k) => RegionSummaries (M.MonoidalMap k (Sum Int)) -> RegionIndicator -> Word8 -> A.BS.BitSet Word16 k
findInSummary summs ri i = A.BS.fromList $ summs ^.. riToLens ri . ixSummary i . itraversed . asIndex
{-# INLINE findInSummary #-}

{- | find `Digit`s in each `Row`\/`Column`\/`Box` that can take only a single position and set those `Cell`s to that `Digit`.
this function assumes the `RegionSummaries` has already been filtered such that it only contains those entries where `Digit`s can
only take a single position.
-}
simplifyHiddenSingles ::
    (VU.Unbox (Cell a), Enum a, Ord a) =>
    Lens' s (VU.Vector (Cell a)) -> RegionSummaries (M.MonoidalMap a (Sum Int)) -> s -> s
simplifyHiddenSingles = applySummary mkUpdate
  where
    -- `possiblyOf` ensures we only target cells that can take some value in the update set so the use of `singular` is safe.
    upd ds = possiblyOf ds %~ view (singular _Just . re _Known) . findValue ds
    findValue (CellSet ds) = findOf (_Possibly . _CellSet . bsfolded) (`A.BS.member` ds)
    findUpdates summs (r, c, b) = CellSet $ foldMap (uncurry (findInSummary summs)) [(Row, r), (Column, c), (Box, b)]
    mkUpdate summs loc = upd (findUpdates summs loc)
{-# INLINE simplifyHiddenSingles #-}

hiddenSinglesUpdateSet ::
    (VU.Unbox (Cell a), Enum a) => CellPos -> RegionSummaries (M.MonoidalMap a (Sum Int)) -> Maybe (CellSet a)
hiddenSinglesUpdateSet (r, c, b) summs = Just . CellSet $ foldMap (uncurry (findInSummary summs)) [(Row, r), (Column, c), (Box, b)]

applyUpdatesFromHiddenSingles :: (VU.Unbox (Cell a), Enum a) => CellSet a -> Cell a -> Cell a
applyUpdatesFromHiddenSingles cs = possiblyOf cs %~ view (singular _Just . re _Known) . findValue cs
  where
    findValue (CellSet ds) = findOf (_Possibly . _CellSet . bsfolded) (`A.BS.member` ds)

updateFromHiddenSingles ::
    (Enum a, VU.Unbox (Cell a)) => RegionSummaries (M.MonoidalMap a (Sum Int)) -> CellPos -> Cell a -> Cell a
updateFromHiddenSingles summs loc = upd (findUpdates summs loc)
  where
    upd ds = possiblyOf ds %~ view (singular _Just . re _Known) . findValue ds
    findValue (CellSet ds) = findOf (_Possibly . _CellSet . bsfolded) (`A.BS.member` ds)
    findUpdates s (r, c, b) = CellSet $ foldMap (uncurry (findInSummary s)) [(Row, r), (Column, c), (Box, b)]

explainHiddenSingles :: (Enum a, TextShow a, Ord a) => RegionIndicator -> Int -> M.MonoidalMap a (Sum Int) -> [Text]
explainHiddenSingles ri i summ
    | not (null summ) =
        [toLazyText $ "Hidden Single in " <> showb ri <> " " <> showb (i + 1) <> ": " <> showb (summ ^.. itraversed . asIndex)]
    | otherwise = []
{-# INLINE explainHiddenSingles #-}

-- | a `Fold` that tabulates the values already `Known` across the `Cell`'s row/column/box
knowns :: (Enum a) => Fold (Cell a) (Union (CellSet a))
knowns = _Known . to A.BS.singleton . from (_Union . _CellSet)
{-# INLINE knowns #-}

-- | a `Fold` producing a map for each `Cell` that tracks how many possible location for a value remain, across it's Row/Column/Box
countedPoss :: (Enum a, Ord a) => Fold (Cell a) (M.MonoidalMap a (Sum Int))
countedPoss = _Possibly . _CellSet . to A.BS.toList . to (M.fromList . fmap (,Sum 1))
{-# INLINE countedPoss #-}

{- | a `Fold` producing a map for each `Cell` that tracks how many times a value has been marked as Known, across it's Row/Column/Box
this should never exceed 1!
-}
countedKnowns :: (Enum a, Ord a) => Fold (Cell a) (M.MonoidalMap a (Sum Int))
countedKnowns = _Known . to (M.fromList . pure . (,Sum 1))
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
m <|.|> m' = ifolding @Identity (\(i, s) -> pure (i, (foldOf m (i, s), foldOf m' (i, s))))
{-# INLINE (<|.|>) #-}

infixr 9 <|.|>

{- | this operator is the same as `<|.|>` but it takes a fold that doesn't need access to the parent `Traversal`'s index on
the left.
-}
(|.>) :: (Monoid m, Monoid m') => Fold s m -> Fold (i, s) m' -> IndexedFold i (i, s) (m, m')
m |.> m' = ifolding @Identity (\(i, s) -> pure (i, (foldOf m s, foldOf m' (i, s))))
{-# INLINE (|.>) #-}

infixr 9 |.>

{- | this operator is the same as `<|.|>` but it takes a `Fold` that doesn't need access to the parent `Traversal`'s index on
the right.
-}
(<.|) :: (Monoid m, Monoid m') => Fold (i, s) m -> Fold s m' -> IndexedFold i (i, s) (m, m')
m <.| m' = ifolding @Identity (\(i, s) -> pure (i, (foldOf m (i, s), foldOf m' s)))
{-# INLINE (<.|) #-}

infixr 9 <.|

{- | this operator is the same as `<|.|>` but it takes a `Fold` that doesn't need access to the parent `Traversal`'s index on
both sides.
-}
(|.|) :: (Monoid m, Monoid m') => Fold s m -> Fold s m' -> IndexedFold i (i, s) (m, m')
m |.| m' = ifolding @Identity (\(i, s) -> pure (i, (foldOf m s, foldOf m' s)))
{-# INLINE (|.|) #-}

infixr 9 |.|

data SummaryRecord m a where
    SummaryRecord ::
        (Monoid m) => !(Contradictions a) -> !(Union (CellSet a)) -> !m -> SummaryRecord m a

instance (Ord a, Enum a) => Semigroup (SummaryRecord m a) where
    (SummaryRecord contra solvedSumms step) <> (SummaryRecord contra' solvedSumms' step') = SummaryRecord (contra <> contra') (solvedSumms <> solvedSumms') (step <> step')

instance (Ord a, Enum a, Monoid m) => Monoid (SummaryRecord m a) where
    mempty = SummaryRecord mempty mempty mempty

noPossibilities :: (Enum a) => IndexedFold CellPos (CellPos, Cell a) [CellPos]
noPossibilities = ifolding @Maybe (\(loc, cell) -> (loc, [loc]) <$ ensure cond cell)
  where
    cond c = hasn't (_Possibly . _CellSet . to A.BS.toList . folded) c && hasn't _Known c
{-# INLINE noPossibilities #-}

possibilitiesWithLoc :: (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (M.MonoidalMap a [CellPos])
possibilitiesWithLoc = ifolding (\(loc, cell) -> Identity (loc, posses loc cell))
  where
    ins loc r a = M.insert a [loc] r
    posses loc = foldlOf' (_Possibly . _CellSet . bsfolded) (ins loc) mempty
{-# INLINE possibilitiesWithLoc #-}

contradictions :: (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (Contradictions a)
contradictions = (noPossibilities <|.|> possibilitiesWithLoc <.| countedKnowns) . to mkContra
  where
    mkContra (el, (cp, ck)) = Contradictions el cp ck
{-# INLINE contradictions #-}

toContradictions :: (Enum a, Ord a) => CellPos -> Cell a -> Contradictions a
toContradictions loc cell = Contradictions noPossF posses countedKnownsF
  where
    noPossP c = hasn't (_Possibly . _CellSet . to A.BS.toList . folded) c && hasn't _Known c
    noPossF = loc <$ ensure @[] noPossP cell
    ins loc' r a = M.insert a [loc'] r
    posses = foldlOf' (_Possibly . _CellSet . bsfolded) (ins loc) mempty cell
    countedKnownsF = foldOf countedKnowns cell

summary ::
    (Enum a, Ord a, Monoid m) => (CellPos -> Cell a -> m) -> IndexedFold CellPos (CellPos, Cell a) (SummaryRecord m a)
summary f =
    ifolding @Identity
        (\(loc, cell) -> pure (loc, SummaryRecord (toContradictions loc cell) (foldOf knowns cell) (f loc cell)))
{-# INLINE summary #-}

factor :: (Monoid m, Monoid m') => RegionSummaries (m, m') -> (RegionSummaries m, RegionSummaries m')
factor summs = (mk (rs, cs, bs), mk (rs', cs', bs'))
  where
    (rs, rs') = V.unzip (summs ^. rows . byRegion)
    (cs, cs') = V.unzip (summs ^. columns . byRegion)
    (bs, bs') = V.unzip (summs ^. boxes . byRegion)
    mk (r, c, b) = def & rows . byRegion .~ r & columns . byRegion .~ c & boxes . byRegion .~ b
{-# INLINE factor #-}

-- | get a summary for a `Simplifier` along with a summary to check for contradictions
summarizeWithContradictions ::
    (Monoid m, Enum a, Ord a) =>
    IndexedTraversal' CellPos s (Cell a)
    -> Fold (CellPos, Cell a) m
    -> s
    -> (RegionSummaries (Contradictions a), RegionSummaries m)
summarizeWithContradictions l m = factor . summaryOf (l . withIndex . contradictions <|.|> m)
{-# INLINE summarizeWithContradictions #-}

solved :: (Enum a) => Fold (Cell a) (Union (CellSet a))
solved = knowns
{-# INLINE solved #-}

checkSolved :: (Enum a) => RegionSummaries (Union (CellSet a)) -> Bool
checkSolved summs = checkAcross Row && checkAcross Column && checkAcross Box
  where
    completeSet = A.BS.fromList [toEnum 0 ..]
    checkAcross ri = allOf (riToLens ri . byRegion . traversed . _Union . _CellSet) (== completeSet) summs
{-# INLINE checkSolved #-}

completelySummarize ::
    (Monoid m, Enum a, Ord a) =>
    IndexedTraversal' CellPos s (Cell a)
    -> (CellPos -> Cell a -> m)
    -> s
    -> RegionSummaries (SummaryRecord m a)
completelySummarize l f = summaryOf (l . withIndex . summary f)
{-# INLINE completelySummarize #-}

findDigitRepeatsOn :: (TextShow a) => RegionIndicator -> Int -> a -> Sum Int -> [Text]
findDigitRepeatsOn ri l d (Sum ks)
    | ks > 1 = L.singleton . toLazyText $ "Repeats " <> showb d <> " in " <> showb ri <> " " <> showb (l + 1)
    | otherwise = mempty

digitsInSameCell :: (TextShow a) => (a, a, CellPos) -> Text
digitsInSameCell (d, d', loc) =
    toLazyText $ "digits must occupy the same cell: digits: " <> showb d <> ", " <> showb d' <> ", in cell " <> showLocB loc

digitsForcedIntoCells :: (TextShow a, Ord a) => M.MonoidalMap a [CellPos] -> M.MonoidalMap a (Sum Int) -> [Text]
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
    RegionIndicator -> Int -> M.MonoidalMap a [CellPos] -> M.MonoidalMap a (Sum Int) -> a -> Maybe Text
findDigitContradiction ri l poss known d
    | digitContradicted poss known d =
        Just . toLazyText $ "Cannot place " <> showb d <> " in " <> showb ri <> " " <> showb (l + 1)
    | otherwise = Nothing

regionalContradictionsTest :: (TextShow a, Ord a, Enum a) => RegionIndicator -> Int -> Contradictions a -> [Text]
regionalContradictionsTest ri l (Contradictions empt poss known) = cellContras <> repeatedDigits <> digitContradictions <> sharedCell
  where
    sharedCell = digitsForcedIntoCells poss known
    cellContras = foldMap (\loc -> [toLazyText $ "no fill for cell: " <> showLocB loc]) empt
    repeatedDigits = ifoldMap (findDigitRepeatsOn ri l) known
    digitContradictions = mapMaybe (findDigitContradiction ri l poss known) [toEnum 0 ..]
{-# SPECIALIZE regionalContradictionsTest :: RegionIndicator -> Int -> Contradictions Digit -> [Text] #-}

testForContradictions :: (TextShow a, Ord a, Enum a) => RegionSummaries (Contradictions a) -> [Text]
testForContradictions summs = L.nubOrd . L.sort $ foldMap testAcross [Row, Column, Box]
  where
    testAcross ri = ifoldMapOf (riToLens ri . byRegion . vectorTraverse) (regionalContradictionsTest ri) summs
{-# SPECIALIZE testForContradictions :: RegionSummaries (Contradictions Digit) -> [Text] #-}

{- | if the possible locations for a digit within a set are aligned on intersecting sets (i.e. 2 only has two locations within a box and they're in the same column),
then remove that digit from other possible locations along the rest of the intersecting traversal.
-}
digitLocationsAlignedOn ::
    forall a.
    (Enum a, VU.Unbox (Cell a), Ord a, TextShow a) => RegionIndicator -> Word8 -> LocationAlignment a -> Grid a -> Grid a
digitLocationsAlignedOn ri i (LocationAlignment possibles) g = foldl' update g [toEnum 0 ..]
  where
    l = case ri of
        Row -> rowAt (fromIntegral i)
        Column -> colAt (fromIntegral i)
        Box -> boxAt (fromIntegral i)
    check Row d = maybe False (allSame sameRow) (locs d)
    check Column d = maybe False (allSame sameCol) (locs d)
    check Box d = maybe False (allSame sameBox) (locs d)
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
    (VU.Unbox (Cell a), Enum a, Ord a, TextShow a) =>
    Lens' (Grid a) (VU.Vector (Cell a)) -> RegionSummaries (LocationAlignment a) -> Grid a -> Grid a
simplifyFromDigitsAligned _ summs = flip (foldl' (\g (ri, i) -> digitLocationsAlignedOn ri i (summFor ri i) g)) wholeGridBySet
  where
    summFor ri i = summs ^. riToLens ri . ixSummary (fromIntegral i)
    wholeGridBySet = [(ri, i) | ri <- [Row, Column, Box], i <- [1 .. 9]]
