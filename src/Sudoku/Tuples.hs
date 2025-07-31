{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Sudoku.Tuples where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Fix (fix)
import Control.Monad.Logic (observeAll, observeMany)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Set.Lens
import Data.Word (Word16)
import Sudoku.Cell (
    Cell (Possibly),
    CellPos,
    CellSet (CellSet),
    RegionIndicator (..),
    bsfolded,
    _CellSet,
    _Known,
    _Possibly,
    _majorMinor,
 )
import Sudoku.Grid
import Sudoku.Summaries (
    ExplainDesc (TupleDesc),
    RegionSummaries,
    applySummary,
    byRegion,
    ixSummary,
    riToLens,
 )
import TextShow (TextShow)

import Data.BitSet qualified as A.BS
import Data.Map.Monoidal.Strict qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

newtype CellPartitions a = CellPartitions [CommonPossibilities a]

makePrisms ''CellPartitions
makeLenses ''CellPartitions

newtype DigitPartitions a = DigitPartitions [CommonPossibilities a]

makePrisms ''DigitPartitions
makeLenses ''DigitPartitions

instance (Enum a, Ord a) => Semigroup (CellPartitions a) where
    CellPartitions cp <> CellPartitions cp' = CellPartitions $ cp ++ cp'

instance (Enum a, Ord a) => Monoid (CellPartitions a) where
    mempty = CellPartitions mempty

instance (Enum a, Ord a) => Semigroup (DigitPartitions a) where
    -- keep each digit in a separate partition for `mergePartitions` to do its thing.
    DigitPartitions cp <> DigitPartitions cp' = DigitPartitions $ cp ++ cp'

instance (Enum a, Ord a) => Monoid (DigitPartitions a) where
    mempty = DigitPartitions mempty

cellPartitionSingleton :: (Enum a, Ord a) => CellPos -> Cell a -> Maybe (CellPos, CellPartitions a)
cellPartitionSingleton loc cell@(Possibly (CellSet cs)) = Just (loc, CellPartitions [CommonPossibilities (S.singleton (loc, cell)) (S.fromList $ A.BS.toList cs)])
cellPartitionSingleton _ _ = Nothing

digitPartitionsFor :: (Enum a, Ord a) => CellPos -> Cell a -> [DigitPartitions a]
digitPartitionsFor loc cell@(Possibly (CellSet cs)) = (\d -> DigitPartitions [CommonPossibilities (S.singleton (loc, cell)) (S.singleton d)]) <$> A.BS.toList cs
digitPartitionsFor _ _ = []

cellPartitions :: (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (CellPartitions a)
cellPartitions = ifolding (uncurry cellPartitionSingleton)

digitPartitions :: (Enum a, Ord a) => IndexedFold CellPos (CellPos, Cell a) (DigitPartitions a)
digitPartitions = ifolding (\(loc, cell) -> Just (loc, fold $ digitPartitionsFor loc cell))

cellUpdateFromPartitions ::
    (Ord a, Enum a) =>
    Lens' s [CommonPossibilities a] -> CellPos -> RegionSummaries s -> A.BS.BitSet Word16 a
cellUpdateFromPartitions sel pos summs =
    foldMapByOf
        (folded . sharedPossibilities)
        A.BS.union
        A.BS.empty
        (A.BS.fromList . S.toList)
        (S.fromList (newTuplesAt pos summs) S.\\ inSet pos summs)
  where
    findTuples ri i s = s ^. riToLens ri . ixSummary i . sel
    newTuplesAt (r, c, b) s = findTuples Row r s <> findTuples Column c s <> findTuples Box b s
    checkCellInTuple loc = S.member loc . view (commonCells . to (S.map fst))
    inSet loc s = S.fromList . filter (checkCellInTuple loc) $ newTuplesAt loc s

partitionMerge :: (Ord a, TextShow a) => Lens' s [CommonPossibilities a] -> S.Set (CommonPossibilities a) -> s -> s
partitionMerge sel kts = sel %~ filterTooSmall . mergePartitions kts . nubOrd . sort . nonEmpty
  where
    nonEmpty = filter (\cp -> nPoss cp > 0 && nCells cp > 0)
    filterTooSmall = filter (\cp -> nPoss cp > 1 && nCells cp > 1)

mergePartitionsAcross ::
    (Ord a, TextShow a) =>
    Lens' s [CommonPossibilities a]
    -> RegionIndicator
    -> S.Set (CommonPossibilities a)
    -> RegionSummaries s
    -> RegionSummaries s
mergePartitionsAcross sel ri kts = riToLens ri . byRegion %~ V.map (partitionMerge sel kts)

mergePartitionsInSummary ::
    (Ord a, TextShow a) =>
    Lens' s [CommonPossibilities a] -> S.Set (CommonPossibilities a) -> RegionSummaries s -> RegionSummaries s
mergePartitionsInSummary sel kts = mergePartitionsAcross sel Row kts . mergePartitionsAcross sel Column kts . mergePartitionsAcross sel Box kts

calculateNewTuplesAcross ::
    (Ord a, Show a) =>
    Lens' s [CommonPossibilities a] -> RegionIndicator -> RegionSummaries s -> S.Set (CommonPossibilities a)
calculateNewTuplesAcross sel ri = foldMapOf (riToLens ri . byRegion . folded . sel) S.fromList

applyPartitions ::
    forall s a.
    (VU.Unbox a, Enum a, Eq a, Ord a, Show a) =>
    Lens' s [CommonPossibilities a] -> Lens' (Grid a) (VU.Vector (Cell a)) -> RegionSummaries s -> Grid a -> Grid a
applyPartitions l sel summs g = g & knownTuples .~ newTuples & applySummary mkUpdate sel summs
  where
    kts = g ^. knownTuples
    -- updatedSumms = mergePartitionsInSummary _CellPartitions kts summs
    newTuplesAcross = flip (calculateNewTuplesAcross l) summs
    newTuples = kts <> newTuplesAcross Row <> newTuplesAcross Column <> newTuplesAcross Box
    upd cell outs = cell & _Possibly . _CellSet %~ (A.BS.\\ outs)
    mkUpdate s loc cell = upd cell (cellUpdateFromPartitions l loc s)

explainPartitions ::
    forall a s.
    (TextShow a, Enum a) => Grid a -> Lens' s [CommonPossibilities a] -> RegionIndicator -> Int -> s -> [ExplainDesc a]
explainPartitions _ sel ri i cps
    | not (null parts') = fmap partToDesc parts'
    | otherwise = []
  where
    parts' = filterTooSmall (cps ^. sel)
    locToMinorIdx loc = (ri, loc) ^. _majorMinor . _3 . to toEnum
    partToDesc (CommonPossibilities locs poss) = TupleDesc ri i (A.BS.fromList (locs ^.. folded . _1 . to locToMinorIdx)) (A.BS.fromList $ poss ^.. folded)
    filterTooSmall = filter (\cp -> nPoss cp > 1 && nCells cp > 1)

{- | a cell can be added to a partition iff its possible `Digit`s are a subset of the `Digit`s already in the partition
(and vice versa for `Digit`s).
-}
matchingSubset :: (Ord a) => S.Set a -> S.Set a -> Bool
matchingSubset a a' = a `S.isSubsetOf` a'

-- | add a `Cell` to a given partition
joinMatchingCell :: (Ord a) => CellPos -> Cell a -> S.Set a -> CommonPossibilities a -> CommonPossibilities a
joinMatchingCell i cell poss cp = cp & sharedPossibilities %~ S.union poss & commonCells %~ S.insert (i, cell)

-- | add a `Digit` to a given partition
joinMatchingDigit :: (Ord a) => a -> [(CellPos, Cell a)] -> CommonPossibilities a -> CommonPossibilities a
joinMatchingDigit d cs cp = cp & sharedPossibilities %~ S.insert d & commonCells %~ \cs' -> S.fromList cs `S.union` cs'

{- | match a cell to a partition by comparing the set of `Digit`s that are possible at that location.
if the cell's possible digits are a subset of those already in the partition, add it to the partition.
-}
findMatchingPoss :: (Ord a) => S.Set a -> Traversal' (CellPartitions a) (CommonPossibilities a)
findMatchingPoss poss = _CellPartitions . traversed . filteredBy (sharedPossibilities . filtered (matchingSubset poss))
{-# INLINE findMatchingPoss #-}

{- | match a cell to a partition by comparing the set of `Digit`s that are possible at that location.
if the cell's possible digits are a subset of those already in the partition, add it to the partition.
-}
findMatchingPoss' :: (Ord a) => S.Set a -> Traversal' [CommonPossibilities a] (CommonPossibilities a)
findMatchingPoss' poss = traversed . filteredBy (sharedPossibilities . filtered (matchingSubset poss))
{-# INLINE findMatchingPoss' #-}

{- | match a digit to a partition by comparing the cells in which its possible. if the digit's possible locations
are a subset of those within the partition, add it to the partition.
-}
findMatchingLocs :: (Ord a) => [(CellPos, Cell a)] -> Traversal' (DigitPartitions a) (CommonPossibilities a)
findMatchingLocs cs = _DigitPartitions . traversed . filteredBy (commonCells . filtered (matchingSubset (S.fromList cs)))

{- | match a digit to a partition by comparing the cells in which its possible. if the digit's possible locations
are a subset of those within the partition, add it to the partition.
-}
findMatchingLocs' :: (Ord a) => [(CellPos, Cell a)] -> Traversal' [CommonPossibilities a] (CommonPossibilities a)
findMatchingLocs' cs = traversed . filteredBy (commonCells . filtered (matchingSubset (S.fromList cs)))

cellPartition ::
    forall a.
    (Ord a, VU.IsoUnbox a Word16, Enum a) => CellPos -> PartitionedPossibilities a -> Cell a -> PartitionedPossibilities a
cellPartition _ pp cell | has _Known cell = pp
cellPartition i pp cell = updatePartitions pp
  where
    poss = setOf (_Possibly . _CellSet . bsfolded) cell

    updatePartitions :: PartitionedPossibilities a -> PartitionedPossibilities a
    updatePartitions pp' | lengthOf (findMatchingPoss' poss) pp' > 0 = pp' & singular (findMatchingPoss' poss) %~ joinMatchingCell i cell poss
    updatePartitions pp' =
        CommonPossibilities{_commonCells = S.fromList [(i, cell)], _sharedPossibilities = poss} : pp'

{- | partition digits into groups of cells, based on their possible locations. if two digits each have the same two cells
as their only available locations, then we immediately know that in the completed puzzle, each of these cells must contain
one of those two digits, and no others. otherwise, we won't be able to place one or more digits into a given, complete set
of the digits.
-}
digitPartition ::
    forall a.
    (Ord a) =>
    SudokuSetTraversal a
    -> Grid a
    -> M.MonoidalMap a [CellPos]
    -> M.MonoidalMap a [CellPos]
    -> PartitionedPossibilities a
    -> a
    -> PartitionedPossibilities a
digitPartition l g ~knowns ~possibilities pp d = updatePartitions pp
  where
    locs = fromMaybe [] (possibilities M.!? d)
    cellsWithLocs = locs <&> cellAtLoc l g
    check pp' = hasn't (ix d . traversed) knowns && lengthOf (findMatchingLocs' cellsWithLocs) pp' > 0
    update :: Lens' [CommonPossibilities a] (CommonPossibilities a)
    update = singular (findMatchingLocs' cellsWithLocs)

    updatePartitions :: PartitionedPossibilities a -> PartitionedPossibilities a
    updatePartitions pp' | check pp' = pp' & update %~ joinMatchingDigit d cellsWithLocs
    updatePartitions pp' = CommonPossibilities{_commonCells = S.fromList cellsWithLocs, _sharedPossibilities = S.singleton d} : pp'

digitLocationsAllOn :: (Ord a) => SudokuSetTraversal a -> Fold (Cell a) a -> Grid a -> M.MonoidalMap a [CellPos]
digitLocationsAllOn l m = ifoldlOf' (runIndexedTraversal l) cellFold M.empty
  where
    cellFold loc = foldlOf' m (\r d -> r & at d %~ Just . maybe [loc] (loc :))
{-# INLINE digitLocationsAllOn #-}

cellAtLoc :: SudokuSetTraversal a -> Grid a -> CellPos -> (CellPos, Cell a)
cellAtLoc l' g loc = (loc, g ^. singular (runIndexedTraversal l' . index loc))

-- | partition cells that share `CommonPossibilities`.
partitionCells :: (Ord a, Enum a, VU.IsoUnbox a Word16) => SudokuSetTraversal a -> Grid a -> PartitionedPossibilities a
partitionCells l = ifoldlOf' (runIndexedTraversal l) cellPartition []

-- | partition a complete set of the digits by the locations they can exist in.
partitionDigits :: (Ord a, Enum a, VU.IsoUnbox a Word16) => SudokuSetTraversal a -> Grid a -> PartitionedPossibilities a
partitionDigits l g = foldl' (digitPartition l g knowns possibilities) [] [toEnum 0 .. toEnum 8]
  where
    ~knowns = digitLocationsAllOn l _Known g
    ~possibilities = digitLocationsAllOn l (_Possibly . _CellSet . bsfolded) g

{- because we added cells/digits into partitions one at a time and only checked that each cell/digit's possibilities/locations were subsets of
the partition, we missed cases like 3 cells where the possible digits are '1, 5', '5, 6', '1, 6' -- this is a triple and the completed puzzle
will have 1, 5, and 6 distributed over just these three cells. to account for this, merge subsets of partitions together where each partition
is a subset of the other partitions in the subset. we do this by partitioning our set of partitions into disjoint subsets that
meet that merge criterion and merging the partitions within those subsets.

i.e. we check that the partition with single cell marking the digits '1, 6' as possibilities is a subset of the union of the partitions holding
'1, 5' and '1, 6'.

however, we don't want to keep merging until all partitions become one giant partition -- to avoid this, we check if we've found a tuple by
comparing the number of digits in the partition and the number of cells in the partition. if they're the same, that partition cannot be merged.
-}
type MergeSet a = S.Set (CommonPossibilities a)

type MergeSets a = S.Set (MergeSet a)

nCells :: CommonPossibilities a -> Int
nCells = lengthOf (commonCells . folded)
{-# INLINE nCells #-}

nPoss :: CommonPossibilities a -> Int
nPoss = lengthOf (sharedPossibilities . folded)
{-# INLINE nPoss #-}

{- | a tuple is a subset of cells in a complete set of the digits that has the same number of possibilities as cells

for instance, if two cells in the same row can be either `1` or `5` (called a pair), but have had all other digits excluded,
then we know that in the completed puzzle, one of these cells will be `1` and the other will be `5`. if any other
cell along the same row has a `1` placed into it, we will wind up with two `5`s in the same row, breaking the puzzle.
therefore, we should notice that we have such a tuple and remove `1` and `5` as possibilities from all other cells in the
same row.
-}
isTuple :: CommonPossibilities a -> Bool
isTuple cp = nCells cp == nPoss cp
{-# INLINE isTuple #-}

-- | cp can be merged into cp' if its digits and cells are subsets of cp'
mergeable :: (Ord a) => CommonPossibilities a -> CommonPossibilities a -> Bool
mergeable cp cp' =
    (cp ^. sharedPossibilities) `S.isSubsetOf` (cp' ^. sharedPossibilities)
        && setOf commonCells cp `S.isSubsetOf` setOf commonCells cp'
{-# INLINE mergeable #-}

{- | check that a partition can be merged with all the others in the `MergeSet` by checking that
if we merge all other members of the `MergeSet` together, that `cp` is a subset of that merged.
however, if `cp` is a tuple already, we can't merge it into the set.
-}
mergeCondition :: (Eq a, Ord a) => MergeSet a -> CommonPossibilities a -> Bool
mergeCondition ms cp = not (isTuple cp) && mergeable cp (fold $ setOf (folded . filtered (/= cp)) ms)
{-# INLINE mergeCondition #-}

{- | score the new partition that would result if we merged this set by looking at how close to a tuple it is.
a score of zero means merging this subset would result in a tuple.
-}
tupleScore :: (Ord a) => MergeSet a -> Int
tupleScore ms = let m = fold ms in abs (nPoss m - nCells m)
{-# INLINE tupleScore #-}

sizeScore :: (Ord a) => MergeSet a -> Int
sizeScore ms = let m = fold ms in nPoss m + nCells m
{-# INLINE sizeScore #-}

-- | test if the provided partition is contained in any member of the `MergeSet`
inMergeSet :: (Ord a) => MergeSets a -> CommonPossibilities a -> Bool
inMergeSet toMerge = flip any toMerge . S.member

-- | gather partitions not present in the `MergeSet`
notMerged' :: (Ord a) => MergeSets a -> Fold (PartitionedPossibilities a) (CommonPossibilities a)
notMerged' toMerge = folded . filtered (not . inMergeSet toMerge)
{-# INLINE notMerged' #-}

{- | we need to merge partitions recursively until there's no change to the set of partitions -- this always happens, either because we've
wound up with a single giant partition with all cells in it, or when all cells are partitioned into tuples (which we very carefully don't
update). duplicates are pruned automatically because we track mergeable and unmergeable partitions in `S.Set`s, only turning it back into
`[CommonPossibilities a]` at the end.

in order to avoid issues blowing the stack, we use `Data.Function.fix` to calculate the fixed point, rather than recursing explicitly.
cycles that lead to infinite recursion should be impossible because we can only ever merge partitions -- we never split them. at each step,
we either choose to do nothing to a partition, or we choose to merge them with some other set of partitions.

when we can't make progress because there's no way to partition the set with fewer partitions, this function will stop recursing.

we decide which partitions to merge by looking at all possible subsets of the set of partitions we start from.
-}
mergePartitions ::
    (Eq a, Ord a, TextShow a) => S.Set (CommonPossibilities a) -> PartitionedPossibilities a -> PartitionedPossibilities a
mergePartitions kps = fix $ \rec pp -> let pp' = go pp in if pp == pp' then pp else rec pp'
  where
    isDuplicate next = flip any next . flip S.member
    chooseCount l u = foldr ((<|>) . pure) empty [l .. u]
    chooseNCond count cond a las = do
        as <- return . S.fromList $ observeMany count las
        guard . not $ S.member a as
        guard $ cond as a
        return as
    lps = foldr ((<|>) . pure) empty

    mkMergeSet pp = do
        cp <- lps pp
        count <- chooseCount 1 (length pp - 1)
        cps <- chooseNCond count mergeCondition cp (lps pp)
        guard . not $ S.member (fold (S.insert cp cps)) kps
        return (S.insert cp cps)

    deduplicatedMergeSets mms = do
        count <- chooseCount 1 (length (observeAll mms) - 1)
        ms <- mms
        mss <- chooseNCond count (\mms' ms' -> not $ anyOf (folded . folded) isTuple (S.insert ms' mms')) ms mms
        let mss' = S.insert ms mss
        guard . not $ any (\ms' -> any (isDuplicate ms') (S.delete ms' mss')) mss'
        guard . not $ any (`S.member` kps) (merge mss')
        return mss'

    choose1 scoring ~ms = fromMaybe S.empty $ scoreMs ^? _head
      where
        ~scoreMs = sortBy scoring (observeAll ms)

    -- merge each subset by folding over the `Monoid` instance for `CommonPossibilities`
    -- we have to go through a list because `S.Set` isn't a functor.
    merge = S.map fold
    scoreOf scoring ~ms = sumOf (folded . to scoring) ms
    scoreByTuples ~ms ~ms' = scoreOf tupleScore ms `compare` scoreOf tupleScore ms'
    scoreBySize ~ms ~ms' = scoreOf sizeScore ms `compare` scoreOf sizeScore ms'
    score ~ms ~ms' = scoreBySize ms ms' <> scoreByTuples ms ms'

    go cps = S.toList (merge mergeSet `S.union` unmergedSubset mergeSet)
      where
        mergeSet =
            choose1 score . deduplicatedMergeSets $
                mkMergeSet cps
        unmergedSubset ms = setOf (notMerged' ms) cps

{- | given a tuple, remove the possibilities in the tuple from all other cells along the given complete set.
and remove digits not contained in the tuple from the set of possibilities of the cells within the tuple.
-}
simplifyFromTupleOf ::
    (Ord a, Enum a, VU.Unbox a) => Grid a -> CommonPossibilities a -> Grid a
simplifyFromTupleOf g cp = cleanAll & knownTuples %~ S.insert cp
  where
    poss = cp ^. sharedPossibilities
    notInTuple i = notElemOf (commonCells . folded . _1) i cp
    inTuple i = elemOf (commonCells . folded . _1) i cp
    locs :: S.Set CellPos
    locs = setOf (commonCells . folded . _1) cp
    intersectingInds l' = setOf (runIndexedTraversal l' . indices (`S.member` locs) . asIndex) g
    intersecting = wholeGridBySet ^.. folded . filtered ((locs ==) . intersectingInds)
    cleanAll = foldl' (\g' l' -> cleanIntersect l' (cleanDisjoint l' g')) g intersecting
    -- update all cells outside the tuple to remove the digits in the tuple from their possibilities
    cleanDisjoint l' = removePossibilitiesOfOn (runIndexedTraversal l' . indices notInTuple) folded poss
    -- remove poss from [1..9] so we get the inverse set (i.e. all digits other than those in poss)
    -- then remove that set from the set of possible digits in each cell in the tuple.
    cleanIntersect l' =
        removePossibilitiesOfOn
            (runIndexedTraversal l' . indices inTuple)
            (folded . filtered (not . flip S.member poss))
            [toEnum 0 .. toEnum 8]

{- | update possibilities along a complete set of the digits by finding tuples and removing their digits from the
rest of the set.
-}
simplifyFromTuplesOf ::
    (Ord a, TextShow a, VU.Unbox a, Enum a) =>
    (SudokuSetTraversal a -> Grid a -> PartitionedPossibilities a) -> Grid a -> SudokuSetTraversal a -> Grid a
simplifyFromTuplesOf partitionF g l = foldlOf' (traversed . filtered isTuple) simplifyFromTupleOf g partitions
  where
    nonEmpty = filter (\cp -> nPoss cp > 0 && nCells cp > 0)
    filterTooSmall = filter (\cp -> nPoss cp > 1 && nCells cp > 1)
    partitions = filterTooSmall . mergePartitions (g ^. knownTuples) . nonEmpty $ partitionF l g

wholeGridBySet :: (Ord a, VU.Unbox a) => [SudokuSetTraversal a]
wholeGridBySet =
    [r | i <- [1 .. 9], let r = rowAt i]
        ++ [c | i <- [1 .. 9], let c = colAt i]
        ++ [b | i <- [1 .. 9], let b = boxAt i]

{- | work across the entire grid and remove possible digits from all cells by looking for tuples formed by cells
that must contain a complete set of the digits.
-}
simplifyFromCellTuples :: (Ord a, TextShow a, VU.Unbox a, Enum a, VU.IsoUnbox a Word16) => Grid a -> Grid a
simplifyFromCellTuples g = foldl' (simplifyFromTuplesOf partitionCells) g wholeGridBySet

{- | work across the entire grid and remove possible digits from all cells by looking for tuples formed by digits
along traversals where all digits must occur.
-}
simplifyFromDigitTuples :: (Ord a, TextShow a, VU.Unbox a, Enum a, VU.IsoUnbox a Word16) => Grid a -> Grid a
simplifyFromDigitTuples g = foldl' (simplifyFromTuplesOf partitionDigits) g wholeGridBySet
