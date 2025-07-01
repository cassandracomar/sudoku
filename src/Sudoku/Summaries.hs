{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-overlapping-patterns #-}

{- | this module provides monoidal summaries of a grid. these summaries provide the workhorses we use to actually define simplifier rules
and are therefore the basic building blocks of the solver. the idea is to only process the grid twice per step. we'd love to do once per step
but applying the updates can't happen at the same time as info collection because we need non-local information when determining what update
to apply to each cell -- i.e. a summary of each cell's `Row`, `Column`, and `Box`. so we instead collect summaries monoidally, appending
a single cell's summary to the summaries for each of its `Row`/`Column`/`Box`.
-}
module Sudoku.Summaries where

import Control.Applicative ((<**>))
import Control.Lens
-- import Data.Array.Accelerate (Elt, Exp, Lift, Plain, Unlift, mkPattern, pattern Pattern)
-- import Data.Array.Accelerate.Control.Lens.Tuple ()
-- import Data.BitSet (pattern BitSet_)
import Data.Containers.ListUtils (nubOrd)
import Data.Default (Default (def))
import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Vector.Generic.Lens (vectorIx, vectorTraverse)
import Data.Vector.Unboxed (IsoUnbox)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
-- import Sudoku.Accelerate.Cell
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

-- import Data.Accelerate.BitSet qualified as A.BS
-- import Data.Array.Accelerate qualified as A
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

{- | flipped double composition. applies the one-argument function on the right to the result of the two-argument function
on the left after both arguments are applied to the function on the left.

i.e. `f >>>> g = \x y -> g (f x y)`
-}
(>>>>) :: (a1 -> a2 -> b) -> (b -> c) -> a1 -> a2 -> c
(>>>>) = flip $ (.) . (.)

infixr 1 >>>>

newtype Union a = Union {runUnion :: a}
    deriving (Eq, Ord, Generic)

-- instance (Elt a) => Elt (Union a)

-- {-# COMPLETE Union_ #-}

-- pattern Union_ :: (Elt a) => Exp a -> Exp (Union a)
-- pattern Union_{a} = Pattern a

-- instance (Lift Exp m, Elt (Plain m)) => Lift Exp (Union m) where
--     type Plain (Union m) = Union (Plain m)
--     lift (Union m) = Union_ (A.lift m)

-- instance (Unlift Exp m, Elt m, Elt (Plain m)) => Unlift Exp (Union m) where
--     unlift = \case
--         (Union_ m) -> Union (A.unlift m)

instance IsoUnbox (Union (CellSet a)) Word16 where
    fromURepr = Union . CellSet . A.BS.BitSet
    toURepr (Union (CellSet (A.BS.BitSet bs))) = bs

newtype instance VU.MVector s (Union a) = MV_Union (VU.MVector s Word16)

newtype instance VU.Vector (Union a) = V_Union (VU.Vector Word16)

deriving via (Union (CellSet a) `VU.As` Word16) instance VG.MVector MVU.MVector (Union (CellSet a))

deriving via (Union (CellSet a) `VU.As` Word16) instance VG.Vector VU.Vector (Union (CellSet a))

instance VU.Unbox (Union (CellSet a))

makePrisms ''Union

newtype Intersects a = Intersects {runIntersection :: a} deriving (Eq, Ord, VP.Prim, Generic)

-- instance (Elt a) => Elt (Intersects a)

-- pattern Intersects_ :: (Elt a) => Exp a -> Exp (Intersects a)
-- pattern Intersects_ a = Pattern a

-- {-# COMPLETE Intersects_ #-}

instance IsoUnbox (Intersects (CellSet a)) Word16 where
    fromURepr = Intersects . CellSet . A.BS.BitSet
    toURepr (Intersects (CellSet (A.BS.BitSet bs))) = bs

newtype instance VU.MVector s (Intersects a) = MV_Intersects (VU.MVector s Word16)

newtype instance VU.Vector (Intersects a) = V_Intersects (VU.Vector Word16)

deriving via (Intersects (CellSet a) `VU.As` Word16) instance VG.MVector MVU.MVector (Intersects (CellSet a))

deriving via (Intersects (CellSet a) `VU.As` Word16) instance VG.Vector VU.Vector (Intersects (CellSet a))

makePrisms ''Intersects

class SetLike a where
    union :: a -> a -> a
    unionEmpty :: a
    intersection :: a -> a -> a
    intersectionEmpty :: a

instance (Enum a) => SetLike (CellSet a) where
    !x `union` !y = CellSet $! _bitSet x `A.BS.union` _bitSet y
    unionEmpty = CellSet A.BS.empty
    !x `intersection` !y = CellSet $! _bitSet x `A.BS.intersection` _bitSet y
    intersectionEmpty = CellSet $! A.BS.fromList [toEnum 0 .. toEnum 8]

-- instance (A.BS.IsIntegralExp Word16 a) => SetLike (Exp (CellSet a)) where
--     -- `CellSets` can only contain 9 entries at most
--     intersectionEmpty = CellSet_ (BitSet_ (A.constant 0b111111111 :: Exp Word16))
--     unionEmpty = CellSet_ (BitSet_ 0)
--     intersection = A.match \case
--         (CellSet_ x) -> A.match \case
--             (CellSet_ y) -> CellSet_ $ x `A.BS.intersection_` y
--     union = A.match \case
--         (CellSet_ x) -> A.match \case
--             (CellSet_ y) -> CellSet_ $ x `A.BS.union_` y

instance (SetLike a) => Semigroup (Union a) where
    !x <> !y = Union $! runUnion x `union` runUnion y
    {-# INLINE (<>) #-}

-- runUnion_ :: (A.Elt m) => A.Exp (Union m) -> A.Exp m
-- runUnion_ = A.match \case
--     Union_ m -> m

-- runIntersection_ :: (A.Elt m) => A.Exp (Intersects m) -> A.Exp m
-- runIntersection_ = A.match \case
--     Intersects_ m -> m

-- instance (Elt m, SetLike (Exp m)) => Semigroup (Exp (Union m)) where
--     m <> m' = Union_ $ runUnion_ m `union` runUnion_ m'
--     {-# INLINE (<>) #-}

instance (SetLike a) => Semigroup (Intersects a) where
    Intersects !x <> Intersects !y = Intersects $! x `intersection` y
    {-# INLINE (<>) #-}

-- instance (Elt m, SetLike (Exp m)) => Semigroup (Exp (Intersects m)) where
--     m <> m' = Intersects_ $ runIntersection_ m `intersection` runIntersection_ m'
--     {-# INLINE (<>) #-}

instance (SetLike a) => Monoid (Union a) where
    mempty = Union unionEmpty

-- instance (SetLike (Exp m), Elt m) => Monoid (Exp (Union m)) where
--     mempty = Union_ unionEmpty

instance (SetLike a) => Monoid (Intersects a) where
    mempty = Intersects intersectionEmpty

-- instance (SetLike (Exp m), Elt m) => Monoid (Exp (Intersects m)) where
--     mempty = Intersects_ intersectionEmpty

newtype OccursN a = OccursN {occursN :: (Word8, a)}
    deriving (Generic)

-- instance (Elt a) => Elt (OccursN a)

makePrisms ''OccursN

-- mkPattern ''OccursN

instance Semigroup (OccursN a) where
    OccursN (count, x) <> OccursN (count', _) = OccursN (count + count', x)
    {-# INLINE (<>) #-}

-- instance (Elt a) => Semigroup (Exp (OccursN a)) where
--     (<>) = A.match \case
--         (OccursN_ (A.T2 count x)) -> A.match \case
--             (OccursN_ (A.T2 count' _)) -> OccursN_ . A.lift $ (count + count', x)

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
    {-# INLINE mempty #-}

data Contradictions a = Contradictions
    { _emptyLocs :: {-# UNPACK #-} ![CellPos]
    , _possLocs :: {-# UNPACK #-} !(M.MonoidalMap a [CellPos])
    , _knownLocs :: {-# UNPACK #-} !(M.MonoidalMap a (Sum Int))
    }
    deriving (Eq, Ord)

instance (Ord a) => Semigroup (Contradictions a) where
    Contradictions e p k <> Contradictions e' p' k' = Contradictions (e <> e') (p <> p') (k <> k')
    {-# INLINE (<>) #-}

instance (Ord a) => Monoid (Contradictions a) where
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

-- newtype SummaryU m = SummaryU (m, m, m, m, m, m, m, m, m)
--     deriving (Generic)

-- instance (A.Elt m) => A.Elt (SummaryU m)

-- makePrisms ''SummaryU

-- A.mkPattern ''SummaryU

-- _SummaryU_ :: (A.Elt m) => Iso' (A.Exp (SummaryU m)) (A.Exp (m, m, m, m, m, m, m, m, m))
-- _SummaryU_ =
--     iso
--         ( A.match \case
--             SummaryU_ t -> t
--         )
--         SummaryU_

-- instance (Semigroup m) => Semigroup (SummaryU m) where
--     SummaryU (!m1, !m2, !m3, !m4, !m5, !m6, !m7, !m8, !m9) <> SummaryU (!m1', !m2', !m3', !m4', !m5', !m6', !m7', !m8', !m9') =
--         SummaryU (m1 <> m1', m2 <> m2', m3 <> m3', m4 <> m4', m5 <> m5', m6 <> m6', m7 <> m7', m8 <> m8', m9 <> m9')
--     {-# INLINE (<>) #-}

-- instance (Semigroup (A.Exp m), A.Elt m) => Semigroup (A.Exp (SummaryU m)) where
--     (<>) = A.match \case
--         SummaryU_ (A.T9 m1 m2 m3 m4 m5 m6 m7 m8 m9) -> A.match \case
--             SummaryU_ (A.T9 m1' m2' m3' m4' m5' m6' m7' m8' m9') ->
--                 SummaryU_
--                     (A.T9 (m1 <> m1') (m2 <> m2') (m3 <> m3') (m4 <> m4') (m5 <> m5') (m6 <> m6') (m7 <> m7') (m8 <> m8') (m9 <> m9'))
--     {-# INLINE (<>) #-}

-- instance (Monoid m) => Monoid (SummaryU m) where
--     mempty = SummaryU (mempty, mempty, mempty, mempty, mempty, mempty, mempty, mempty, mempty)
--     {-# INLINE mempty #-}

-- instance (Monoid (A.Exp m), A.Elt m) => Monoid (A.Exp (SummaryU m)) where
--     mempty = SummaryU_ (A.T9 mempty mempty mempty mempty mempty mempty mempty mempty mempty)
--     {-# INLINE mempty #-}

-- type instance IxValue (SummaryU m) = m

-- type instance Index (SummaryU m) = Int

-- instance Ixed (SummaryU m) where
--     ix i = _SummaryU . ix (i - 1)
--     {-# INLINE ix #-}

-- instance (Elt m) => Field1 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _1 = _SummaryU_ . _1

-- instance (Elt m) => Field2 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _2 = _SummaryU_ . _2

-- instance (Elt m) => Field3 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _3 = _SummaryU_ . _3

-- instance (Elt m) => Field4 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _4 = _SummaryU_ . _4

-- instance (Elt m) => Field5 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _5 = _SummaryU_ . _5

-- instance (Elt m) => Field6 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _6 = _SummaryU_ . _6

-- instance (Elt m) => Field7 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _7 = _SummaryU_ . _7

-- instance (Elt m) => Field8 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _8 = _SummaryU_ . _8

-- instance (Elt m) => Field9 (A.Exp (SummaryU m)) (A.Exp (SummaryU m)) (A.Exp m) (A.Exp m) where
--     _9 = _SummaryU_ . _9

-- type instance IxValue (A.Exp (SummaryU m)) = A.Exp m

-- type instance Index (A.Exp (SummaryU m)) = A.Exp Int

-- instance (A.Elt m) => Ixed (A.Exp (SummaryU m)) where
--     ix i =
--         _SummaryU_
--             -- there's no `Ixed` instance for T9 so we have to provide the lens manually
--             -- we can't just map to the field lens because that would require returning
--             -- a non-Exp value via `A.match` -- this is ruled out categorically.
--             . lens
--                 ( A.match \case
--                     A.T9 m1 m2 m3 m4 m5 m6 m7 m8 m9 ->
--                         A.BS.fromRepr i & A.match \case
--                             One_ -> m1
--                             Two_ -> m2
--                             Three_ -> m3
--                             Four_ -> m4
--                             Five_ -> m5
--                             Six_ -> m6
--                             Seven_ -> m7
--                             Eight_ -> m8
--                             Nine_ -> m9
--                 )
--                 ( \t m' ->
--                     A.BS.fromRepr i & A.match \case
--                         One_ -> t & _1 .~ m'
--                         Two_ -> t & _2 .~ m'
--                         Three_ -> t & _3 .~ m'
--                         Four_ -> t & _4 .~ m'
--                         Five_ -> t & _5 .~ m'
--                         Six_ -> t & _6 .~ m'
--                         Seven_ -> t & _7 .~ m'
--                         Eight_ -> t & _8 .~ m'
--                         Nine_ -> t & _9 .~ m'
--                 )
--     {-# INLINE ix #-}

-- summuSingleton_ :: (A.Elt m, Monoid (A.Exp m)) => A.Exp m -> A.Exp Int -> A.Exp (SummaryU m)
-- summuSingleton_ m i =
--     A.BS.fromRepr i & A.match \case
--         One_ -> SummaryU_ (A.T9 m mempty mempty mempty mempty mempty mempty mempty mempty)
--         Two_ -> SummaryU_ (A.T9 mempty m mempty mempty mempty mempty mempty mempty mempty)
--         Three_ -> SummaryU_ (A.T9 mempty mempty m mempty mempty mempty mempty mempty mempty)
--         Four_ -> SummaryU_ (A.T9 mempty mempty mempty m mempty mempty mempty mempty mempty)
--         Five_ -> SummaryU_ (A.T9 mempty mempty mempty mempty m mempty mempty mempty mempty)
--         Six_ -> SummaryU_ (A.T9 mempty mempty mempty mempty mempty m mempty mempty mempty)
--         Seven_ -> SummaryU_ (A.T9 mempty mempty mempty mempty mempty mempty m mempty mempty)
--         Eight_ -> SummaryU_ (A.T9 mempty mempty mempty mempty mempty mempty mempty m mempty)
--         Nine_ -> SummaryU_ (A.T9 mempty mempty mempty mempty mempty mempty mempty mempty m)

-- concat9 :: (Monoid m) => SummaryU m -> m
-- concat9 (SummaryU (!m1, !m2, !m3, !m4, !m5, !m6, !m7, !m8, !m9)) = m1 <> (m2 <> (m3 <> (m4 <> (m5 <> (m6 <> (m7 <> (m8 <> m9)))))))
-- {-# INLINE concat9 #-}

{- | assuming the monoid instance for `a` is associative, this function provides an `ifoldl'` function that can be used over an entire
grid to calculate a summary of the grid, organized by region. rather than using the inefficient `Monoid` instance on `CellSummaries`,
it instead updates each region the cell belongs to by indexing `CellSummaries` to get the current summary of each region type and
updates it by `mappend`ing `a` on the right. this relies on the associativity of the `Monoid` instance for `a`. a non-law abiding
`Monoid` instance won't produce the expected results.
-}
addToSummary :: (Monoid m) => CellPos -> RegionSummaries m -> m -> RegionSummaries m
addToSummary (r, c, b) regions m =
    regions
        & ix Row . ix (fromIntegral r) %~ (<> m)
        & ix Column . ix (fromIntegral c) %~ (<> m)
        & ix Box . ix (fromIntegral b) %~ (<> m)
{-# INLINE addToSummary #-}

{- | produce a summary for an entire grid. takes the `Grid` vector and a function that maps each `Cell` to a Monoidal summary type `a`, carrying an index that
provides the necessary information about the `Cell`'s row/column/box. the summary type chosen determines the information available at the end of the fold.
for example, mapping each `Cell` to a singleton `CellSet` of `Known` digits wrapped in a `Union` newtype will produce the set of already known digits
in each row/column/box.
-}
summaryOf :: (Monoid m) => IndexedFold CellPos s m -> s -> RegionSummaries m
summaryOf l = ifoldlOf' l addToSummary (RegionSummaries mempty mempty mempty)
{-# INLINE summaryOf #-}

-- applySummaryOf ::
--     (VU.Unbox a) =>
--     IndexedFold CellPos (RegionSummaries b) c -> Lens' s (VU.Vector a) -> (a -> c -> a) -> RegionSummaries b -> s -> s
-- applySummaryOf l sel f summaries = sel %~ flip (VU.unsafeAccum f) updates
--   where
--     updates = itoListOf (reindexed vindex l) summaries
-- {-# INLINE applySummaryOf #-}

-- | apply updates derived from `RegionSummaries` to `s` as determined by the function `f` at each position (`CellPos`, `Cell`) in `s`.
applySummary ::
    (VU.Unbox a) =>
    (RegionSummaries b -> CellPos -> a -> a) -> Lens' s (VU.Vector a) -> RegionSummaries b -> s -> s
applySummary f sel summs g = g & sel . cellPos %@~ f summs
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

-- instance (A.Elt a) => A.Elt (ExplainDesc a)

-- A.mkPattern ''ExplainDesc

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

-- simplifyAllKnowns ::
--     (VU.Unbox (Cell a), Enum a) => Lens' s (VU.Vector (Cell a)) -> RegionSummaries (Union (CellSet a)) -> s -> s
-- simplifyAllKnowns = applySummary updateFromKnowns
-- {-# INLINE simplifyAllKnowns #-}

explainKnowns :: (Enum a, TextShow a) => RegionIndicator -> Int -> Union (CellSet a) -> [ExplainDesc a]
explainKnowns ri i (Union (CellSet cs))
    | cs /= A.BS.empty && cs /= allDigits =
        [AlreadyKnown ri i cs]
    | otherwise = []
  where
    allDigits = A.BS.fromList [toEnum 0 .. toEnum 8]
{-# INLINE explainKnowns #-}

updateFromKnowns :: (Enum a, VU.Unbox (Cell a)) => RegionSummaries (Union (CellSet a)) -> CellPos -> Cell a -> Cell a
updateFromKnowns summs loc = _Possibly . _CellSet %~ (A.BS.\\ (updateSet summs loc ^. _Union . _CellSet))
{-# INLINE updateFromKnowns #-}

-- knownsUpdateSet ::
--     (Enum a, VU.Unbox (Cell a)) => CellPos -> RegionSummaries (Union (CellSet a)) -> Maybe (Union (CellSet a))
-- knownsUpdateSet loc summs = ensure (/= mempty) (updateSet summs loc)

-- applyUpdateFromKnowns :: Cell a -> Union (CellSet a) -> Cell a
-- applyUpdateFromKnowns cell (Union (CellSet cs)) = cell & _Possibly . _CellSet %~ (A.BS.\\ cs)

-- simplifyNakedSingles ::
--     (VU.Unbox (Cell a), Enum a, Bounded a, VU.IsoUnbox a Word16) =>
--     Lens' s (VU.Vector (Cell a)) -> RegionSummaries b -> s -> s
-- simplifyNakedSingles = applySummary updateFromNakedSingles
-- {-# INLINE simplifyNakedSingles #-}

-- nakedSinglesUpdateSet :: CellPos -> RegionSummaries b -> Maybe ()
-- nakedSinglesUpdateSet _ _ = Just ()

-- applyUpdateFromNakedSingles :: (Enum a, Bounded a, VU.Unbox (Cell a), VU.IsoUnbox a Word16) => Cell a -> () -> Cell a
-- applyUpdateFromNakedSingles (Possibly (CellSet cs)) _ | A.BS.size cs == 1 = mkKnown $ fromJust (A.BS.first cs)
-- applyUpdateFromNakedSingles cell _ = cell

updateFromNakedSingles ::
    (VU.Unbox (Cell a), Enum a, Bounded a, VU.IsoUnbox a Word16) => RegionSummaries b -> CellPos -> Cell a -> Cell a
updateFromNakedSingles _ _ (Possibly (CellSet cs)) | A.BS.size cs == 1 = mkKnown $ fromJust (A.BS.first cs)
updateFromNakedSingles _ _ cell = cell

explainNakedSingles ::
    (Enum a, TextShow a, VU.Unbox a) => RegionIndicator -> Int -> VU.Vector (CellPos, a) -> [ExplainDesc a]
explainNakedSingles _ _ = VU.foldMap (\(loc, d) -> [SinglePoss loc d])
{-# INLINE explainNakedSingles #-}

singlePossibility :: (VU.Unbox a, Enum a) => IndexedFold CellPos (CellPos, Cell a) (VU.Vector (CellPos, a))
singlePossibility = ifolding (\(loc, cell) -> Identity . (loc,) $ singlePoss loc cell)
  where
    singlePoss loc = \cases
        (Possibly (CellSet cs)) | A.BS.size cs == 1 -> VU.singleton (loc, A.BS.toList cs ^. singular _head)
        _ -> VU.empty
{-# INLINE singlePossibility #-}

filterSummaryByCount ::
    (Eq a) => RegionIndicator -> a -> RegionSummaries (M.MonoidalMap k (Sum a)) -> RegionSummaries (M.MonoidalMap k (Sum a))
filterSummaryByCount ri c = riToLens ri . byRegion . vectorTraverse %~ M.filter (== Sum c)

filterSummariesByCount ::
    Int -> RegionSummaries (M.MonoidalMap k (Sum Int)) -> RegionSummaries (M.MonoidalMap k (Sum Int))
filterSummariesByCount c = filterSummaryByCount Row c . filterSummaryByCount Column c . filterSummaryByCount Box c

findInSummary ::
    (Enum k) => RegionSummaries (M.MonoidalMap k (Sum Int)) -> RegionIndicator -> Word8 -> A.BS.BitSet Word16 k
findInSummary summs ri i = A.BS.fromList $ summs ^.. ix ri . ix (fromIntegral i) . itraversed . asIndex

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
    (Enum a, VU.Unbox (Cell a), VU.IsoUnbox a Word16, Bounded a) =>
    RegionSummaries (M.MonoidalMap a (Sum Int)) -> CellPos -> Cell a -> Cell a
updateFromHiddenSingles = findUpdates >>>> markUniqueKnown
  where
    -- `A.BS.union` together the values with only one possible location in this cell's regions -- if this cell
    -- can take any of these values, we'll mark it `Known` here.
    findUpdates s (r, c, b) = CellSet $! foldMap (uncurry (findInSummary s)) [(Row, r), (Column, c), (Box, b)]
{-# INLINE updateFromHiddenSingles #-}

-- {-# SPECIALIZE updateFromHiddenSingles ::
--     RegionSummaries (M.MonoidalMap Digit (Sum Int)) -> CellPos -> Cell Digit -> Cell Digit
--     #-}

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
countedPoss = _Possibly . _CellSet . to A.BS.toList . to (M.fromList . fmap (,Sum 1))
{-# INLINE countedPoss #-}

{- | a `Fold` producing a map for each `Cell` that tracks how many times a value has been marked as Known, across it's Row/Column/Box
this should never exceed 1!
-}
countedKnowns :: (Enum a, Ord a, VU.IsoUnbox a Word16) => Fold (Cell a) (M.MonoidalMap a (Sum Int))
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
        (Monoid m) => {-# UNPACK #-} !(Contradictions a) -> {-# UNPACK #-} !(Union (CellSet a)) -> !m -> SummaryRecord m a

instance (Ord a, Enum a) => Semigroup (SummaryRecord m a) where
    (SummaryRecord contra solvedSumms step) <> (SummaryRecord contra' solvedSumms' step') =
        SummaryRecord (contra <> contra') (solvedSumms <> solvedSumms') (step <> step')
    {-# INLINE (<>) #-}

instance (Ord a, Enum a, Monoid m) => Monoid (SummaryRecord m a) where
    mempty = SummaryRecord mempty mempty mempty
    {-# INLINE mempty #-}

noPossibilities :: (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) [CellPos]
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

contradictions :: (Enum a, Ord a, VU.IsoUnbox a Word16) => IndexedFold CellPos (CellPos, Cell a) (Contradictions a)
contradictions = (noPossibilities <|.|> possibilitiesWithLoc <.| countedKnowns) . to mkContra
  where
    mkContra (el, (cp, ck)) = Contradictions el cp ck
{-# INLINE contradictions #-}

-- toContradictions :: (Enum a, Ord a, VU.IsoUnbox a Word16) => CellPos -> Cell a -> Contradictions a
-- toContradictions loc cell = Contradictions noPossF posses countedKnownsF
--   where
--     noPossP c = hasn't (_Possibly . _CellSet . to A.BS.toList . folded) c && hasn't _Known c
--     noPossF = loc <$ ensure @[] noPossP cell
--     ins loc' r a = M.insert a [loc'] r
--     posses = foldlOf' (_Possibly . _CellSet . bsfolded) (ins loc) mempty cell
--     countedKnownsF = foldOf countedKnowns cell

-- summary ::
--     (Enum a, Ord a, Monoid m, VU.IsoUnbox a Word16) =>
--     (CellPos -> Cell a -> m) -> CellPos -> Cell a -> SummaryRecord m a
-- summary f loc cell = SummaryRecord (toContradictions loc cell) (cell ^. knowns) (f loc cell)
-- {-# INLINE summary #-}

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
    (Monoid m, Enum a, Ord a, VU.IsoUnbox a Word16, VU.Unbox a) =>
    IndexedTraversal' CellPos s (Cell a)
    -> Fold (CellPos, Cell a) m
    -> s
    -> (RegionSummaries (Contradictions a), RegionSummaries m)
summarizeWithContradictions l m = factor . summaryOf (l . withIndex . contradictions <|.|> m)
{-# INLINE summarizeWithContradictions #-}

solved :: (Enum a, VU.IsoUnbox a Word16) => Fold (Cell a) (Union (CellSet a))
solved = knowns
{-# INLINE solved #-}

ixSolved :: (Enum a, VU.IsoUnbox a Word16) => IndexedFold CellPos (Grid a) (Union (CellSet a))
ixSolved = reindexed rowColumn . indexing $ grid . vectorTraverse . solved
{-# INLINE ixSolved #-}

checkSolved :: (Enum a) => RegionSummaries (Union (CellSet a)) -> Bool
checkSolved summs = checkAcross Row && checkAcross Column && checkAcross Box
  where
    completeSet = A.BS.fromList [toEnum 0 .. toEnum 8]
    checkAcross ri = allOf (riToLens ri . byRegion . traversed . _Union . _CellSet) (== completeSet) summs
{-# INLINE checkSolved #-}

-- completelySummarize' ::
--     (Monoid m, Enum a, Ord a, VU.IsoUnbox a Word16) =>
--     IndexedTraversal' CellPos s (Cell a)
--     -> IndexedFold CellPos (CellPos, Cell a) m
--     -> s
--     -> RegionSummaries (SummaryRecord m a)
-- completelySummarize' l f = summaryOf' (l . withIndex . contradictions <|.|> m)
-- {-# INLINE completelySummarize' #-}

-- completelySummarize ::
--     (Monoid m, Enum a, Ord a, VU.IsoUnbox a Word16) =>
--     Lens' s (VU.Vector (Cell a))
--     -> (CellPos -> Cell a -> m)
--     -> s
--     -> RegionSummaries (SummaryRecord m a)
-- completelySummarize l f s = summaryOf (s ^. l) (summary f)
-- {-# INLINE completelySummarize #-}

data ContradictionDesc i a
    = DigitRepeats !a !RegionIndicator !i
    | SharedCell !a !a !CellPos
    | CannotPlace !a !RegionIndicator !i
    | NoFillForCell !CellPos
    deriving (Eq, Ord, Generic)

-- instance (A.Elt a, A.Elt i) => A.Elt (ContradictionDesc i a)

-- A.mkPattern ''ContradictionDesc

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
{-# SPECIALIZE regionalContradictionsTest ::
    RegionIndicator -> Int -> Contradictions Digit -> [ContradictionDesc Int Digit]
    #-}

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
    (Enum a, VU.Unbox (Cell a), Ord a, TextShow a) => RegionIndicator -> Word8 -> LocationAlignment a -> Grid a -> Grid a
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
    (VU.Unbox (Cell a), Enum a, Ord a, TextShow a) =>
    Lens' (Grid a) (VU.Vector (Cell a)) -> RegionSummaries (LocationAlignment a) -> Grid a -> Grid a
simplifyFromDigitsAligned _ summs = flip (foldl' (\g (ri, i) -> digitLocationsAlignedOn ri i (summFor ri i) g)) wholeGridBySet
  where
    summFor ri i = summs ^. riToLens ri . ixSummary (fromIntegral i)
    wholeGridBySet = [(ri, i) | ri <- [Row, Column, Box], i <- [1 .. 9]]
