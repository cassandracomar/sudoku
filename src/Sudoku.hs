{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Sudoku where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Category ((>>>))
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad (foldM, guard, join, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logic (observeAll, observeMany)
import Control.Monad.LogicState (
    LogicStateT,
    MonadLogic (interleave, lnot, (>>-)),
    MonadLogicState (backtrack),
    MonadTrans (lift),
    TransLogicState (observeT),
 )
import Control.Monad.State.Lazy (MonadState (put))
import Control.Monad.Trans.Writer.Lazy (WriterT (runWriterT))
import Control.Monad.Writer.Lazy (MonadWriter (tell))
import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON,
    defaultOptions,
    eitherDecodeStrict,
    genericParseJSON,
 )
import Data.Array (Array, listArray, (//))
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Foldable
import Data.Function (fix)
import Data.List (intersperse, sortBy)
import Data.List.Split (splitOn)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid (Sum (Sum))
import Data.Set ((\\))
import Data.Set qualified as S
import Data.Set.Lens (setOf)
import Data.Word (Word8)
import GHC.Generics (Generic)

newtype Unquoted = Unquoted String

instance Show Unquoted where
    show (Unquoted s) = s

type Digit = Word8

type RowIx = Word8

type ColIx = Word8

type BoxIx = Word8

data Cell
    = Known !Digit
    | Possibly
        { possibilities :: !(S.Set Digit)
        }
    deriving stock (Ord, Eq, Generic)

instance NFData Cell

instance Show Cell where
    show c = case c of
        Possibly ps ->
            if S.size ps < 5
                then
                    "("
                        ++ replicate (4 - S.size ps + 1) ' '
                        ++ intersperse ' ' (foldMap show $ S.toList ps)
                        ++ replicate (4 - S.size ps + 1) ' '
                        ++ ")"
                else "           "
        Known d -> "     " ++ show d ++ "     "

makePrisms ''Cell
makeLenses ''Cell

type CellPos = (RowIx, ColIx)

data Constraint = Given !CellPos !Digit
    -- \| KillerCage {_sum :: !Word8, _cells :: !Cell}
    deriving stock (Ord, Eq, Generic)

instance NFData Constraint

makePrisms ''Constraint
makeLenses ''Constraint

newtype RawConstraintRows = RawConstraintRows {_rawrows :: [RawConstraintRow]} deriving stock (Ord, Eq, Show, Generic)

instance ToJSON RawConstraintRows

fixFields :: String -> String
fixFields "_rawrows" = "rows"
fixFields "_row" = "row"
fixFields "_columns" = "columns"
fixFields "_column" = "column"
fixFields "_value" = "value"
fixFields field = error $ "unknown field: " ++ field

instance FromJSON RawConstraintRows where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = fixFields}

data RawConstraintRow = RawConstraintRow {_row :: !RowIx, _columns :: ![RawConstraintColumn]}
    deriving stock (Ord, Eq, Show, Generic)

instance ToJSON RawConstraintRow

instance FromJSON RawConstraintRow where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = fixFields}

data RawConstraintColumn = RawConstraintColumn {_column :: !ColIx, _value :: !Digit}
    deriving stock (Ord, Eq, Show, Generic)

instance ToJSON RawConstraintColumn

instance FromJSON RawConstraintColumn where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = fixFields}

makeLenses ''RawConstraintRows
makeLenses ''RawConstraintRow
makeLenses ''RawConstraintColumn

mkConstraint :: RowIx -> RawConstraintColumn -> Constraint
mkConstraint rix c = Given (rix, c ^. column) (c ^. value)

mkGivens :: RawConstraintRow -> [Constraint]
mkGivens r = over mapped (mkConstraint (r ^. row)) (r ^. columns)

newtype Rules = Rules
    { _constraints :: [Constraint]
    }
    deriving stock (Ord, Eq, Generic)

instance NFData Rules

makeLenses ''Rules

mkRules :: RawConstraintRows -> Rules
mkRules rs = Rules{_constraints = fold $ over mapped mkGivens (rs ^. rawrows)}

mkCell :: Cell
mkCell = Possibly{possibilities = S.fromList [1 .. 9]}

data CommonPossibilities = CommonPossibilities
    { _commonCells :: !(S.Set (CellPos, Cell))
    , _sharedPossibilities :: !(S.Set Digit)
    }
    deriving (Eq, Ord, Generic)

instance NFData CommonPossibilities

makeLenses ''CommonPossibilities

instance Semigroup CommonPossibilities where
    (<>) :: CommonPossibilities -> CommonPossibilities -> CommonPossibilities
    cp <> cp' =
        cp
            & sharedPossibilities %~ S.union (cp' ^. sharedPossibilities)
            & commonCells %~ S.union (cp' ^. commonCells)
    {-# INLINE (<>) #-}

instance Monoid CommonPossibilities where
    mempty = CommonPossibilities mempty mempty

type PartitionedPossibilities = [CommonPossibilities]

data Grid = Grid
    { _grid :: !(Array CellPos Cell)
    , _rules :: !Rules
    , _knownTuples :: !(S.Set CommonPossibilities)
    , _contradictionSearched :: !(S.Set CellPos)
    }
    deriving stock (Ord, Eq, Generic)
instance NFData Grid

makeLenses ''Grid

type Contradicted = S.Set Digit

type SudokuT m a = LogicStateT Contradicted Grid m a

type Sudoku a = SudokuT (WriterT BacktrackStateLog IO) a

data BacktrackState = BacktrackState {_bs :: Grid, _location :: CellPos, _contradicted :: Digit}

type BacktrackStateLog = [BacktrackState]

makeLenses ''BacktrackState

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

data CellDiff = CDiff
    { _cellPos :: CellPos
    , _origCell :: Cell
    , _newCell :: Cell
    }

makeLenses ''CellDiff

instance Show CommonPossibilities where
    show cp = "Tuple {cells=" ++ show (Unquoted . showLoc <$> locs) ++ ", digits=" ++ show poss ++ "}\n"
      where
        locs = cp ^.. commonCells . folded . _1
        poss = cp ^.. sharedPossibilities . folded

type SetIndex = Word8

data SudokuSet = Row | Column | Box | Other deriving (Show, Eq)

data SudokuSetDesc where
    SetDesc :: SudokuSet -> SetIndex -> [(Word8, Word8)] -> SudokuSetDesc
    SetDescUnknown :: [CellPos] -> SudokuSetDesc

instance Show SudokuSetDesc where
    show desc = case desc of
        SetDesc Row i cs -> "r" ++ show i ++ renderRanges "c" cs
        SetDesc Column i rs -> "c" ++ show i ++ renderRanges "r" rs
        SetDesc Box i _ -> "box " ++ show i
        SetDescUnknown ixes -> "cells: " ++ show (showLoc <$> ixes)
        _ -> "BUG: invalid set"
      where
        renderRange p (i, i')
            | i == i' = p ++ show i
            | otherwise = p ++ show i ++ "-" ++ show i'
        renderRanges _ ((1, 9) : _) = ""
        renderRanges p es = " (" ++ (fold . intersperse "," $ renderRange p <$> es) ++ ")"

matchesSet :: (CellPos -> CellPos -> Bool) -> [CellPos] -> Bool
matchesSet p (e : es) = all (p e) es
matchesSet _ _ = False

allSame :: (CellPos -> CellPos -> Bool) -> [CellPos] -> Bool
allSame p (loc : locs) = all (p loc) locs
allSame _ [] = False

sameBox :: CellPos -> CellPos -> Bool
sameBox (r, c) (r', c') = boxNumber r c == boxNumber r' c'

sameRow :: CellPos -> CellPos -> Bool
sameRow (r, _) (r', _) = r == r'

sameCol :: CellPos -> CellPos -> Bool
sameCol (_, c) (_, c') = c == c'

describeSudokuSet :: SudokuSetTraversal -> SudokuSetDesc
describeSudokuSet l
    | matchesSet sameRow setIndices = SetDesc Row (rowColN _1) (rowColRanges _2)
    | matchesSet sameCol setIndices = SetDesc Column (rowColN _2) (rowColRanges _1)
    | matchesSet sameBox setIndices = SetDesc Box (g ^. singular (runIndexedTraversal l . asIndex) & uncurry boxNumber) []
    | otherwise = SetDescUnknown (g ^.. runIndexedTraversal l . asIndex)
  where
    g = def
    setIndices = g ^.. runIndexedTraversal l . asIndex
    rowColN sel = g ^. singular (runIndexedTraversal l . asIndex . sel)
    rowColRanges sel = foldlOf' (runIndexedTraversal l . asIndex . sel) mergeIx [] g
    mergeIx ((s, e) : rs) i
        | i == s - 1 = (i, e) : rs
        | i == e + 1 = (s, i) : rs
        | i > s - 1 && i <= e = (s, e) : rs
        | otherwise = (i, i) : (s, e) : rs
    mergeIx _ i = [(i, i)]

describeIndices :: Fold s CellPos -> s -> [SudokuSetDesc]
describeIndices f ls = indsInSameSet sameRow ++ indsInSameSet sameCol ++ indsInSameSet sameBox & foldMap indsToDesc
  where
    loc l = l ^. singular _head
    addIndToSet p rs l = case find (all (p l)) rs of
        Just r -> (l : r) : rs
        Nothing -> [l] : rs
    indsInSameSet p = foldlOf' f (addIndToSet p) [] ls
    indsToDesc inds
        | all (sameRow (loc inds)) inds = [SetDesc Row (loc inds ^. _1) inds]
        | all (sameCol (loc inds)) inds = [SetDesc Row (loc inds ^. _2) inds]
        | all (sameBox (loc inds)) inds = [SetDesc Box (uncurry boxNumber (loc inds)) inds]
        | otherwise = [SetDescUnknown inds]

setDescTraversalExact :: SudokuSetDesc -> SudokuSetTraversal
setDescTraversalExact s@(SetDesc _ _ es) = IndexedTraversal $ runIndexedTraversal (setDescTraversal s) . indices (`elem` es)
setDescTraversalExact s = setDescTraversal s

setDescTraversal :: SudokuSetDesc -> SudokuSetTraversal
setDescTraversal (SetDesc Row i _) = rowAt i
setDescTraversal (SetDesc Column i _) = colAt i
setDescTraversal (SetDesc Box i _) = boxAt i
setDescTraversal (SetDesc Other _ es) = IndexedTraversal $ grid . itraversed . indices (`elem` es)
setDescTraversal (SetDescUnknown es) = IndexedTraversal $ grid . itraversed . indices (`elem` es)

instance Default Grid where
    def = Grid emptyGrid (Rules []) mempty S.empty

instance Show Grid where
    show g = fold showRows ++ kps
      where
        boxIndicator Row = dashRow
        boxIndicator Column = "|"
        boxIndicator _ = ""
        addBoxes b = fold . intersperse [boxIndicator b] . chunksOf 6
        showRows = [dashRow] ++ addBoxes Row (intersperse underlineRow ([1 .. 9] <&> showRow)) ++ [dashRow]
        showRow i =
            "|" ++ fold (addBoxes Column . intersperse "|" $ g ^.. runIndexedTraversal (rowAt i) . to show) ++ "|" ++ "\n"
        underlineRow = replicate (9 * 12 + 3) '_' ++ "\n"
        dashRow = replicate (9 * 12 + 3) '-' ++ "\n"
        kps
            | lengthOf (knownTuples . folded) g > 0 =
                "\n" ++ "Known Tuples:" ++ "\n" ++ fold (setOf (knownTuples . folded . to show) g)
            | otherwise = ""

diffCell :: CellPos -> Cell -> Cell -> Maybe CellDiff
diffCell loc cell cell'
    | cell /= cell' = Just CDiff{_cellPos = loc, _origCell = cell, _newCell = cell'}
    | otherwise = Nothing

diffCells :: Grid -> Grid -> [CellDiff]
diffCells g g' = getDiff (aggregate g) (aggregate g')
  where
    aggregate b = b ^@.. grid . itraversed
    getDiff cells cells' = [diffCell loc cell cell' | (loc, cell) <- cells, (loc', cell') <- cells', loc == loc'] ^.. traversed . _Just

showLoc :: CellPos -> String
showLoc (r, c) = "r" ++ show r ++ "c" ++ show c

showCellForDiff :: Cell -> String
showCellForDiff (Known d) = show d
showCellForDiff (Possibly poss) = show (S.toList poss)

renderCellDiff :: CellDiff -> String
renderCellDiff (CDiff loc cell cell') = showLoc loc ++ " " ++ showCellForDiff cell ++ " -> " ++ showCellForDiff cell'

applyLineWidth :: Char -> Int -> String -> String
applyLineWidth splitAtChar width str = fold . intersperse "\n" . reverse $ rest : rev
  where
    splitStr = intersperse [splitAtChar] $ splitOn [splitAtChar] str
    refold = foldl' (\(str', res) s -> if length str' < width then (str' ++ s, res) else (s, str' : res)) ("", [])
    (rest, rev) = refold splitStr

diffGrid :: Grid -> Grid -> String
diffGrid g g'
    | has traversed cellDiffs = "Applying Updates: \n" ++ cellDiffs ++ "\n"
    | otherwise = ""
  where
    cellDiffs = applyLineWidth ',' (9 * 12 + 3) . fold . intersperse ", " $ renderCellDiff <$> diffCells g g'

{- | a traversal across a subset of cells that must contain a complete set of the digits 1-9, once each
in more complex variant Sudokus, there will be extra regions in the puzzle -- frequently that must be discovered
-- that meet this criterion as well. TBD if those puzzles can be solved by a logical (non-human) solver.
-}
type SudokuSetTraversal = ReifiedIndexedTraversal' CellPos Grid Cell

gridIndices :: [CellPos]
gridIndices = [(r, c) | r <- [1 .. 9], c <- [1 .. 9]]

emptyGrid :: Array (Word8, Word8) Cell
emptyGrid = listArray ((1, 1), (9, 9)) $ repeat mkCell

collectIndices :: IndexedTraversal' CellPos s a -> s -> [CellPos]
collectIndices l g = g ^.. l . asIndex

cellAtLoc :: SudokuSetTraversal -> Grid -> CellPos -> (CellPos, Cell)
cellAtLoc l' g loc = (loc, g ^. singular (runIndexedTraversal l' . index loc))

ixCell :: CellPos -> IndexedTraversal' CellPos Grid Cell
ixCell loc = grid . itraversed . index loc
{-# INLINE ixCell #-}

mkGrid :: Rules -> Grid
mkGrid givenRules = Grid{_grid = mkBoard, _rules = givenRules, _knownTuples = S.empty, _contradictionSearched = S.empty}
  where
    givenDigits :: [(CellPos, Cell)]
    givenDigits = (givenRules ^.. constraints . traversed . _Given) <&> _2 %~ Known
    mkBoard :: Array CellPos Cell
    mkBoard = emptyGrid // givenDigits

allSetsOf ::
    (Digit -> SudokuSetTraversal)
    -> [SudokuSetTraversal]
allSetsOf ixOf = over traversed ixOf [1 .. 9]
{-# INLINE allSetsOf #-}

sudokuSetsOf ::
    (forall b. Traversal [Digit] [b] Digit b)
    -> Traversal [Digit] [SudokuSetTraversal] Digit SudokuSetTraversal
sudokuSetsOf l = l
{-# INLINE sudokuSetsOf #-}

rowAt :: RowIx -> SudokuSetTraversal
rowAt rowIx = IndexedTraversal $ grid . itraversed . indices (\(r, _c) -> r == rowIx)
{-# INLINE rowAt #-}

rows :: [SudokuSetTraversal]
rows = allSetsOf rowAt
{-# INLINE rows #-}

colAt :: ColIx -> SudokuSetTraversal
colAt colIx = IndexedTraversal $ grid . itraversed . indices (\(_r, c) -> c == colIx)
{-# INLINE colAt #-}

cols :: [SudokuSetTraversal]
cols = allSetsOf colAt
{-# INLINE cols #-}

boxNumber :: RowIx -> ColIx -> BoxIx
boxNumber rowIx colIx = (((rowIx - 1) `div` 3) * 3) + ((colIx - 1) `div` 3) + 1
{-# INLINE boxNumber #-}

boxAt :: BoxIx -> SudokuSetTraversal
boxAt boxIx = IndexedTraversal $ grid . itraversed . indices (\(r, c) -> boxNumber r c == boxIx)
{-# INLINE boxAt #-}

boxes :: [SudokuSetTraversal]
boxes = allSetsOf boxAt
{-# INLINE boxes #-}

wholeGridBySet :: [SudokuSetTraversal]
wholeGridBySet = rows ++ cols ++ boxes
{-# INLINE wholeGridBySet #-}

intersectionOn ::
    Grid
    -> SudokuSetTraversal
    -> SudokuSetTraversal
    -> SudokuSetTraversal
intersectionOn g l m =
    IndexedTraversal $
        runIndexedTraversal l
            . indices (\(lr, lc) -> flip has g $ runIndexedTraversal m . indices (\(mr, mc) -> lr == mr && lc == mc))
{-# INLINE intersectionOn #-}

intersectsOn :: Grid -> SudokuSetTraversal -> SudokuSetTraversal -> Bool
intersectsOn g l m = flip has g $ runIndexedTraversal (intersectionOn g l m)
{-# INLINE intersectsOn #-}

intersectingOn ::
    Grid
    -> SudokuSetTraversal
    -> [SudokuSetTraversal]
    -> [SudokuSetTraversal]
intersectingOn g l ms = ms ^.. traversed . filtered (intersectsOn g l)
{-# INLINE intersectingOn #-}

neighborhoodOf :: Grid -> CellPos -> [SudokuSetTraversal]
neighborhoodOf g loc = intersectingOn g (IndexedTraversal $ grid . itraversed . index loc) wholeGridBySet
{-# INLINE neighborhoodOf #-}

indexEqualityOn :: Grid -> SudokuSetTraversal -> SudokuSetTraversal -> Bool
indexEqualityOn g l m = collectIndices (runIndexedTraversal l) g == collectIndices (runIndexedTraversal m) g
{-# INLINE indexEqualityOn #-}

(@==) :: SudokuSetTraversal -> SudokuSetTraversal -> Grid -> Bool
l @== m = \g -> indexEqualityOn g l m
{-# INLINE (@==) #-}

infixl 4 @==

differenceOn ::
    Grid
    -> SudokuSetTraversal
    -> SudokuSetTraversal
    -> SudokuSetTraversal
differenceOn g l m = IndexedTraversal $ runIndexedTraversal l . indices (not . flip S.member s)
  where
    s = setOf (runIndexedTraversal m . asIndex) g
{-# INLINE differenceOn #-}

(@\\) ::
    SudokuSetTraversal
    -> SudokuSetTraversal
    -> Grid
    -> SudokuSetTraversal
l @\\ m = \g -> differenceOn g l m
{-# INLINE (@\\) #-}

infixl 4 @\\

disjoint :: Grid -> SudokuSetTraversal -> SudokuSetTraversal -> Bool
disjoint g l m = g & l @== (l @\\ m) g
{-# INLINE disjoint #-}

solved :: Grid -> Bool
solved g = all completedOf wholeGridBySet
  where
    completeSet = S.fromList [1 .. 9]

    completedOf :: SudokuSetTraversal -> Bool
    completedOf l = setOf (runIndexedTraversal l . _Known) g == completeSet
{-# INLINE solved #-}

removePossibilitiesOf :: Fold s Digit -> s -> Cell -> Cell
removePossibilitiesOf l s = over _Possibly (\\ setOf l s)
{-# INLINE removePossibilitiesOf #-}

removePossibilitiesOfOn :: IndexedTraversal' CellPos Grid Cell -> Fold s Digit -> s -> Grid -> Grid
removePossibilitiesOfOn l d s = over l (removePossibilitiesOf d s)
{-# INLINE removePossibilitiesOfOn #-}

removePossibilities :: (Foldable t) => t Digit -> Cell -> Cell
removePossibilities = removePossibilitiesOf folded
{-# INLINE removePossibilities #-}

removePossibility :: Digit -> Cell -> Cell
removePossibility d = removePossibilities (S.singleton d)
{-# INLINE removePossibility #-}

-- | if there are no digits to place in a cell, we've reached a contradiction
cellContradiction :: Cell -> Bool
cellContradiction c = hasn't (_Possibly . folded) c && hasn't _Known c
{-# INLINE cellContradiction #-}

cellContradictionOn :: SudokuSetTraversal -> SudokuSetTraversal
cellContradictionOn l = IndexedTraversal $ runIndexedTraversal l . filtered cellContradiction
{-# INLINE cellContradictionOn #-}

findDigitContradictionOn :: SudokuSetDesc -> Digit -> [CellPos] -> Maybe String
findDigitContradictionOn ~s d ~locs
    | null locs =
        Just $ "Cannot place " ++ show d ++ " in " ++ show s
    | otherwise = Nothing
{-# INLINE findDigitContradictionOn #-}

findDigitRepeatsOn :: [CellPos] -> Digit -> Maybe String
findDigitRepeatsOn knowns d
    | length knowns > 1 =
        Just $
            "Repeats "
                ++ show d
                ++ " in "
                ++ show (showLoc <$> knowns)
    | otherwise = Nothing
{-# INLINE findDigitRepeatsOn #-}

digitsMustOccupySameCellOn :: SudokuSetTraversal -> Grid -> [(Digit, Digit, CellPos)]
digitsMustOccupySameCellOn l g = singlePoss
  where
    ~allPossibleLocs = digitLocationsAllOn l (_Possibly . folded) g
    ~allKnownLocs = digitLocationsAllOn l _Known g
    locs = fromMaybe [] . (allPossibleLocs !?)
    ~singleLoc =
        [(d, loc) | d <- [1 .. 9], let loc = locs d, length loc == 1, hasn't (ix d . traversed) allKnownLocs]
    ~singlePoss = [(d, d', loc ^. singular _head) | (d, loc) <- singleLoc, (d', loc') <- singleLoc, loc == loc', d < d']
{-# INLINE digitsMustOccupySameCellOn #-}

findAnyContradictionsAlong :: Grid -> SudokuSetTraversal -> [String]
findAnyContradictionsAlong g l = digitRepeats ++ digitContradictions ++ digitsInSameCell
  where
    ~allPossibleLocs = digitLocationsAllOn l (_Possibly . folded) g
    ~allKnownLocs = digitLocationsAllOn l _Known g
    ~desc = describeSudokuSet l
    repeatedDigits d = findDigitRepeatsOn (fromMaybe [] (allKnownLocs !? d)) d
    ~digitRepeats = [1 .. 9] ^.. traversed . to repeatedDigits . _Just
    digitContradicted d
        | null $ allKnownLocs !? d = findDigitContradictionOn desc d (fromMaybe [] (allPossibleLocs !? d))
        | otherwise = Nothing
    ~digitContradictions = [1 .. 9] ^.. traversed . to digitContradicted . _Just

    ~digitsInSameCell =
        ( \(d, d', loc) -> "digits must occupy the same cell: digits: " ++ show d ++ ", " ++ show d' ++ ", in cell " ++ showLoc loc
        )
            <$> digitsMustOccupySameCellOn l g
{-# INLINE findAnyContradictionsAlong #-}

findAnyContradictionsOn :: Grid -> [String]
findAnyContradictionsOn g = foldMap (findAnyContradictionsAlong g) wholeGridBySet ++ foldMap cellContradictions rows
  where
    cellContradictions l = (\loc -> "no fill for cell: " ++ showLoc loc) <$> g ^.. runIndexedTraversal (cellContradictionOn l) . asIndex
{-# INLINE findAnyContradictionsOn #-}

-- | remove knowns from possibilities in a given traversal over a row, column, or box
simplifyKnownsOf :: SudokuSetTraversal -> Grid -> Grid
simplifyKnownsOf l g = over (runIndexedTraversal l) (removePossibilitiesOf (runIndexedTraversal l . _Known) g) g
{-# INLINE simplifyKnownsOf #-}

simplifyAllKnownsOf :: Grid -> Grid
simplifyAllKnownsOf g = foldl' (flip simplifyKnownsOf) g wholeGridBySet
{-# INLINE simplifyAllKnownsOf #-}

-- | locate cells that have just one possibility
singlePossibilityOf :: SudokuSetTraversal -> SudokuSetTraversal
singlePossibilityOf l = IndexedTraversal $ runIndexedTraversal l . filtered (\cell -> lengthOf (_Possibly . folded) cell == 1)
{-# INLINE singlePossibilityOf #-}

-- | find cells where there's only one possibility and mark them as known
singlePossibilityMarkKnownOf :: Grid -> SudokuSetTraversal -> Grid
singlePossibilityMarkKnownOf g l = over grid (// collect) g & knownTuples %~ flip removeAll collect
  where
    poss !cell = cell ^. singular (_Possibly . folded)
    collect = g ^@.. runIndexedTraversal (singlePossibilityOf l) . to (Known . poss)
    remove cps loc = setOf (folded . filtered (notElemOf (commonCells . folded) loc)) cps
    removeAll = foldl' remove
{-# INLINE singlePossibilityMarkKnownOf #-}

-- | simplify the grid by marking all cells where there's only a single possibility as `Known`
simplifySinglePossibilityMarkKnown :: Grid -> Grid
simplifySinglePossibilityMarkKnown !g = foldl' singlePossibilityMarkKnownOf g wholeGridBySet

type DigitLocations = [CellPos]

{- | find all possible and `Known` locations for a digit. if the digit is already known in this set, return just that location.
filter the `Traversal` passed to this function to just `Possibly` if you need to exclude `Known`s, or vice versa.
e.g.:

```haskell
  digitLocations (rowAt 1 . filtered (has _Possibly)) g 1
```
-}
digitLocations :: SudokuSetTraversal -> Grid -> Digit -> DigitLocations
digitLocations l !g !d
    | has (runIndexedTraversal l . _Known . filtered (== d)) g =
        g ^.. runIndexedTraversal l . filtered (elemOf _Known d) . asIndex
digitLocations l !g !d = g ^.. runIndexedTraversal l . filtered (elemOf (_Possibly . folded) d) . asIndex
{-# INLINE digitLocations #-}

digitLocationsAllOn :: SudokuSetTraversal -> Fold Cell Digit -> Grid -> M.Map Digit [CellPos]
digitLocationsAllOn l m = ifoldlOf' (runIndexedTraversal l) cellFold M.empty
  where
    cellFold loc = foldlOf' m (\r d -> r & at d %~ Just . maybe [loc] (loc :))
{-# INLINE digitLocationsAllOn #-}

digitAlreadyKnown :: SudokuSetTraversal -> Digit -> SudokuSetTraversal
digitAlreadyKnown l d = IndexedTraversal $ runIndexedTraversal l . filtered (elemOf _Known d)
{-# INLINE digitAlreadyKnown #-}

-- | if a digit is only possible in a single place in this complete set, mark it as `Known`
singleLocationMarkKnownOf ::
    Grid -> SudokuSetTraversal -> Grid
singleLocationMarkKnownOf g l = foldl' update g [1 .. 9]
  where
    allPossibleLocs = digitLocationsAllOn l (_Possibly . folded) g
    allKnownLocs = digitLocationsAllOn l _Known g
    locs d = allPossibleLocs ^.. ix d . traversed
    check d = length (locs d) == 1 && hasn't (ix d . traversed) allKnownLocs
    -- this is only called if `check d` above is satisfied
    dLoc d = locs d ^. singular _head
    matchingTuple d = notElemOf (commonCells . folded . _1) (dLoc d)
    clearMatchingTuples !d !cps = S.fromList $ cps ^.. folded . filtered (matchingTuple d)
    update g' d | check d = g' & runIndexedTraversal l . index (dLoc d) .~ Known d & knownTuples %~ clearMatchingTuples d
    update g' _ = g'
{-# INLINE singleLocationMarkKnownOf #-}

simplifySingleLocationForDigit :: Grid -> Grid
simplifySingleLocationForDigit g = foldl' singleLocationMarkKnownOf g wholeGridBySet

{- | if the possible locations for a digit within a set are aligned on intersecting sets (i.e. 2 only has two locations within a box and they're in the same column),
then remove that digit from other possible locations along the rest of the intersecting traversal.
-}
digitLocationsAlignedOn :: Grid -> SudokuSetTraversal -> Grid
digitLocationsAlignedOn g l = foldl' update g [1 .. 9]
  where
    possibles = digitLocationsAllOn l (_Possibly . folded) g
    locs d = fromMaybe [] $ possibles ^? ix d
    inSet d
        | allSame sameRow (locs d) = locs d ^? _head . _1 . to rowAt . to (@\\ l) . to ($ g)
        | allSame sameCol (locs d) = locs d ^? _head . _2 . to colAt . to (@\\ l) . to ($ g)
        | allSame sameBox (locs d) = locs d ^? _head . to (uncurry boxNumber) . to boxAt . to (@\\ l) . to ($ g)
        | otherwise = Nothing
    update g' d = case inSet d of
        Just m -> removePossibilitiesOfOn (runIndexedTraversal m) folded [d] g'
        Nothing -> g'
{-# INLINE digitLocationsAlignedOn #-}

simplifyFromDigitsAligned :: Grid -> Grid
simplifyFromDigitsAligned = flip (foldl' digitLocationsAlignedOn) wholeGridBySet

-- find alignment of digit locations in multiple rows/columns/boxes

{- | a cell can be added to a partition iff its possible `Digit`s are a subset of the `Digit`s already in the partition
(and vice versa for `Digit`s).
-}
matchingSubset :: (Ord a) => S.Set a -> S.Set a -> Bool
matchingSubset a a' = a `S.isSubsetOf` a'

-- | add a `Cell` to a given partition
joinMatchingCell :: CellPos -> Cell -> S.Set Digit -> CommonPossibilities -> CommonPossibilities
joinMatchingCell i cell poss cp = cp & sharedPossibilities %~ S.union poss & commonCells %~ S.insert (i, cell)

-- | add a `Digit` to a given partition
joinMatchingDigit :: Digit -> [(CellPos, Cell)] -> CommonPossibilities -> CommonPossibilities
joinMatchingDigit d cs cp = cp & sharedPossibilities %~ S.insert d & commonCells %~ \cs' -> S.fromList cs `S.union` cs'

{- | match a cell to a partition by comparing the set of `Digit`s that are possible at that location.
if the cell's possible digits are a subset of those already in the partition, add it to the partition.
-}
findMatchingPoss :: S.Set Digit -> Traversal' [CommonPossibilities] CommonPossibilities
findMatchingPoss poss = traversed . filteredBy (sharedPossibilities . filtered (matchingSubset poss))
{-# INLINE findMatchingPoss #-}

{- | match a digit to a partition by comparing the cells in which its possible. if the digit's possible locations
are a subset of those within the partition, add it to the partition.
-}
findMatchingLocs :: [(CellPos, Cell)] -> Traversal' [CommonPossibilities] CommonPossibilities
findMatchingLocs cs = traversed . filteredBy (commonCells . filtered (matchingSubset (S.fromList cs)))

cellPartition :: CellPos -> PartitionedPossibilities -> Cell -> PartitionedPossibilities
cellPartition _ pp cell | has _Known cell = pp
cellPartition i pp cell = updatePartitions pp
  where
    poss = setOf (_Possibly . folded) cell

    updatePartitions :: PartitionedPossibilities -> PartitionedPossibilities
    updatePartitions pp' | lengthOf (findMatchingPoss poss) pp' > 0 = pp' & singular (findMatchingPoss poss) %~ joinMatchingCell i cell poss
    updatePartitions pp' =
        CommonPossibilities{_commonCells = S.fromList [(i, cell)], _sharedPossibilities = poss} : pp'

{- | partition digits into groups of cells, based on their possible locations. if two digits each have the same two cells
as their only available locations, then we immediately know that in the completed puzzle, each of these cells must contain
one of those two digits, and no others. otherwise, we won't be able to place one or more digits into a given, complete set
of the digits.
-}
digitPartition ::
    SudokuSetTraversal
    -> Grid
    -> M.Map Digit [CellPos]
    -> M.Map Digit [CellPos]
    -> PartitionedPossibilities
    -> Digit
    -> PartitionedPossibilities
digitPartition l g ~knowns ~possibilities pp d = updatePartitions pp
  where
    locs = fromMaybe [] (possibilities !? d)
    cellsWithLocs = locs <&> cellAtLoc l g
    check pp' = hasn't (ix d . traversed) knowns && lengthOf (findMatchingLocs cellsWithLocs) pp' > 0

    updatePartitions :: PartitionedPossibilities -> PartitionedPossibilities
    updatePartitions pp' | check pp' = pp' & singular (findMatchingLocs cellsWithLocs) %~ joinMatchingDigit d cellsWithLocs
    updatePartitions pp' = CommonPossibilities{_commonCells = S.fromList cellsWithLocs, _sharedPossibilities = S.singleton d} : pp'

-- | partition cells that share `CommonPossibilities`.
partitionCells :: SudokuSetTraversal -> Grid -> PartitionedPossibilities
partitionCells l = ifoldlOf' (runIndexedTraversal l) cellPartition []

-- | partition a complete set of the digits by the locations they can exist in.
partitionDigits :: SudokuSetTraversal -> Grid -> PartitionedPossibilities
partitionDigits l g = foldl' (digitPartition l g knowns possibilities) [] [1 .. 9]
  where
    ~knowns = digitLocationsAllOn l _Known g
    ~possibilities = digitLocationsAllOn l (_Possibly . folded) g

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
type MergeSet = S.Set CommonPossibilities

type MergeSets = S.Set MergeSet

nCells :: CommonPossibilities -> Int
nCells = lengthOf (commonCells . folded)
{-# INLINE nCells #-}

nPoss :: CommonPossibilities -> Int
nPoss = lengthOf (sharedPossibilities . folded)
{-# INLINE nPoss #-}

{- | a tuple is a subset of cells in a complete set of the digits that has the same number of possibilities as cells

for instance, if two cells in the same row can be either `1` or `5` (called a pair), but have had all other digits excluded,
then we know that in the completed puzzle, one of these cells will be `1` and the other will be `5`. if any other
cell along the same row has a `1` placed into it, we will wind up with two `5`s in the same row, breaking the puzzle.
therefore, we should notice that we have such a tuple and remove `1` and `5` as possibilities from all other cells in the
same row.
-}
isTuple :: CommonPossibilities -> Bool
isTuple cp = nCells cp == nPoss cp
{-# INLINE isTuple #-}

-- | cp can be merged into cp' if its digits and cells are subsets of cp'
mergeable :: CommonPossibilities -> CommonPossibilities -> Bool
mergeable cp cp' =
    (cp ^. sharedPossibilities) `S.isSubsetOf` (cp' ^. sharedPossibilities)
        && setOf commonCells cp `S.isSubsetOf` setOf commonCells cp'
{-# INLINE mergeable #-}

{- | check that a partition can be merged with all the others in the `MergeSet` by checking that
if we merge all other members of the `MergeSet` together, that `cp` is a subset of that merged.
however, if `cp` is a tuple already, we can't merge it into the set.
-}
mergeCondition :: MergeSet -> CommonPossibilities -> Bool
mergeCondition ms cp = not (isTuple cp) && mergeable cp (fold $ setOf (folded . filtered (/= cp)) ms)
{-# INLINE mergeCondition #-}

{- | score the new partition that would result if we merged this set by looking at how close to a tuple it is.
a score of zero means merging this subset would result in a tuple.
-}
tupleScore :: MergeSet -> Int
tupleScore ms = let m = fold ms in abs (nPoss m - nCells m)
{-# INLINE tupleScore #-}

sizeScore :: MergeSet -> Int
sizeScore ms = let m = fold ms in nPoss m + nCells m
{-# INLINE sizeScore #-}

-- | test if the provided partition is contained in any member of the `MergeSet`
inMergeSet :: MergeSets -> CommonPossibilities -> Bool
inMergeSet toMerge = flip any toMerge . S.member

-- | gather partitions not present in the `MergeSet`
notMerged' :: MergeSets -> Fold PartitionedPossibilities CommonPossibilities
notMerged' toMerge = folded . filtered (not . inMergeSet toMerge)
{-# INLINE notMerged' #-}

{- | we need to merge partitions recursively until there's no change to the set of partitions -- this always happens, either because we've
wound up with a single giant partition with all cells in it, or when all cells are partitioned into tuples (which we very carefully don't
update). duplicates are pruned automatically because we track mergeable and unmergeable partitions in `S.Set`s, only turning it back into
`[CommonPossibilities]` at the end.

in order to avoid issues blowing the stack, we use `Data.Function.fix` to calculate the fixed point, rather than recursing explicitly.
cycles that lead to infinite recursion should be impossible because we can only ever merge partitions -- we never split them. at each step,
we either choose to do nothing to a partition, or we choose to merge them with some other set of partitions.

when we can't make progress because there's no way to partition the set with fewer partitions, this function will stop recursing.

we decide which partitions to merge by looking at all possible subsets of the set of partitions we start from.
-}
mergePartitions :: S.Set CommonPossibilities -> PartitionedPossibilities -> PartitionedPossibilities
mergePartitions kps = fix $ \rec pp -> let pp' = go pp in if pp' == pp then pp' else rec pp'
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

    go cps = S.toList $ merge mergeSet `S.union` unmergedSubset mergeSet
      where
        mergeSet =
            choose1 score . deduplicatedMergeSets $
                mkMergeSet cps
        unmergedSubset ms = setOf (notMerged' ms) cps

{- | given a tuple, remove the possibilities in the tuple from all other cells along the given complete set.
and remove digits not contained in the tuple from the set of possibilities of the cells within the tuple.
-}
simplifyFromTupleOf :: SudokuSetTraversal -> Grid -> CommonPossibilities -> Grid
simplifyFromTupleOf l g cp = cleanIntersect (cleanDisjoint g) & knownTuples %~ S.insert cp
  where
    poss = cp ^. sharedPossibilities
    notInTuple i = notElemOf (commonCells . folded . _1) i cp
    inTuple i = elemOf (commonCells . folded . _1) i cp
    -- update all cells outside the tuple to remove the digits in the tuple from their possibilities
    cleanDisjoint = removePossibilitiesOfOn (runIndexedTraversal l . indices notInTuple) folded poss
    -- remove poss from [1..9] so we get the inverse set (i.e. all digits other than those in poss)
    -- then remove that set from the set of possible digits in each cell in the tuple.
    cleanIntersect =
        removePossibilitiesOfOn
            (runIndexedTraversal l . indices inTuple)
            (folded . filtered (not . flip S.member poss))
            [1 .. 9]

{- | update possibilities along a complete set of the digits by finding tuples and removing their digits from the
rest of the set.
-}
simplifyFromTuplesOf :: (SudokuSetTraversal -> Grid -> PartitionedPossibilities) -> Grid -> SudokuSetTraversal -> Grid
simplifyFromTuplesOf partitionF g l = foldlOf' (traversed . filtered isTuple) (simplifyFromTupleOf l) g partitions
  where
    nonEmpty = filter (\cp -> nPoss cp > 0 && nCells cp > 0)
    filterTooSmall = filter (\cp -> nPoss cp > 1 && nCells cp > 1)
    partitions = filterTooSmall . mergePartitions (g ^. knownTuples) . nonEmpty $ partitionF l g

{- | work across the entire grid and remove possible digits from all cells by looking for tuples formed by cells
that must contain a complete set of the digits.
-}
simplifyFromCellTuples :: Grid -> Grid
simplifyFromCellTuples g = foldl' (simplifyFromTuplesOf partitionCells) g wholeGridBySet

{- | work across the entire grid and remove possible digits from all cells by looking for tuples formed by digits
along traversals where all digits must occur.
-}
simplifyFromDigitTuples :: Grid -> Grid
simplifyFromDigitTuples g = foldl' (simplifyFromTuplesOf partitionDigits) g wholeGridBySet

class Simplifier a where
    simplify :: a -> Grid -> Grid
    simplifierName :: a -> String

data Simplify = forall a. (Simplifier a) => Simplify a

instance Simplifier Simplify where
    simplify (Simplify a) = simplify a
    {-# INLINE simplify #-}
    simplifierName (Simplify a) = simplifierName a
    {-# INLINE simplifierName #-}

data SinglePossibilityMarkKnown = SinglePossibilityMarkKnown

data SingleLocationForDigit = SingleLocationForDigit

data AlreadyKnown = AlreadyKnown

data DigitTuples = DigitTuples

data CellTuples = CellTuples

data DigitLocsAligned = DigitLocsAligned

instance Simplifier SinglePossibilityMarkKnown where
    simplify _ = simplifySinglePossibilityMarkKnown
    {-# INLINE simplify #-}
    simplifierName _ = "Set cells with just one option to Known"
    {-# INLINE simplifierName #-}

instance Simplifier SingleLocationForDigit where
    simplify _ = simplifySingleLocationForDigit
    {-# INLINE simplify #-}
    simplifierName _ = "Place digit with just one location"
    {-# INLINE simplifierName #-}

instance Simplifier AlreadyKnown where
    simplify _ = simplifyAllKnownsOf
    {-# INLINE simplify #-}
    simplifierName _ = "Remove Knowns from possible cell values"
    {-# INLINE simplifierName #-}

instance Simplifier DigitTuples where
    simplify _ = simplifyFromDigitTuples
    {-# INLINE simplify #-}
    simplifierName _ = "Locate tuples based on where digits can live"
    {-# INLINE simplifierName #-}

instance Simplifier CellTuples where
    simplify _ = simplifyFromCellTuples
    {-# INLINE simplify #-}
    simplifierName _ = "Locate tuples based on possible values for cells"
    {-# INLINE simplifierName #-}

instance Simplifier DigitLocsAligned where
    simplify _ = simplifyFromDigitsAligned
    {-# INLINE simplify #-}
    simplifierName _ = "Check if possible digit locations are aligned and remove them from other positions along the aligned set"
    {-# INLINE simplifierName #-}

cheapSimplifiers :: [Simplify]
cheapSimplifiers = [Simplify SingleLocationForDigit, Simplify SinglePossibilityMarkKnown]
{-# INLINE cheapSimplifiers #-}

expensiveSimplifiers :: [Simplify]
expensiveSimplifiers = [Simplify DigitLocsAligned, Simplify DigitTuples, Simplify CellTuples]
{-# INLINE expensiveSimplifiers #-}

printStep :: (Simplifier a, MonadIO m) => a -> Grid -> Grid -> m ()
printStep f g g' = unless (diff == "") $ do
    printUnquoted . fold $ replicate (9 * 12 + 3) "-"
    printUnquoted $ "Step: " ++ simplifierName f
    printUnquoted "Starting Grid: "
    liftIO . print $ g
    printUnquoted diff
    printUnquoted "Resulting In: "
    liftIO $ print g'
  where
    diff = diffGrid g g'
{-# SPECIALIZE INLINE printStep :: (Simplifier a) => a -> Grid -> Grid -> Sudoku () #-}

checkContradictions :: (MonadIO m, Alternative m) => Grid -> m ()
checkContradictions g = do
    let contradictions = findAnyContradictionsOn g
    unless (null contradictions) $ do
        printUnquoted "Found a Contradiction:"
        forM_ contradictions $ \c -> do
            printUnquoted c
    guard (null contradictions)
{-# SPECIALIZE INLINE checkContradictions :: Grid -> Sudoku () #-}

runSimplifierOnce :: (MonadIO m, Alternative m, Simplifier a) => a -> Grid -> m Grid
runSimplifierOnce f g = do
    let g' = simplify f g
    printStep f g g'
    checkContradictions g'
    return g'
{-# SPECIALIZE INLINE runSimplifierOnce :: (Simplifier a) => a -> Grid -> Sudoku Grid #-}

runSimplifier :: (MonadIO m, Alternative m, Simplifier a) => a -> (a -> Grid -> m Grid) -> m Grid -> m Grid
runSimplifier f act = fix $ \rec mg -> do
    g <- mg
    g' <- act f g
    if solved g || g' == g then return g' else rec (return g')
{-# SPECIALIZE INLINE runSimplifier ::
    (Simplifier a) => a -> (a -> Grid -> Sudoku Grid) -> Sudoku Grid -> Sudoku Grid
    #-}

runManySimplifiers :: (MonadIO m, Alternative m) => (Grid -> m Grid) -> m Grid -> m Grid
runManySimplifiers act = fix $ \rec mg -> do
    g <- mg
    g' <- act g
    checkContradictions g'
    if solved g || g' == g then return g' else rec (return g')
{-# SPECIALIZE INLINE runManySimplifiers :: (Grid -> Sudoku Grid) -> Sudoku Grid -> Sudoku Grid #-}

reportSolved :: (MonadIO m) => Grid -> m ()
reportSolved g = when (solved g) . printUnquoted $ "solved!"
{-# SPECIALIZE INLINE reportSolved :: Grid -> Sudoku () #-}

{- | apply a computationally cheap simplifier recursively until it produce no updates and stop applying a simplifier if it's about
to break the puzzle.
-}
applyCheapSimpliferM :: (MonadIO m, Alternative m, Simplifier a) => a -> m Grid -> m Grid
applyCheapSimpliferM f = runSimplifier f $ \_ g -> runSimplifierOnce AlreadyKnown g >>= runSimplifierOnce f
{-# SPECIALIZE INLINE applyCheapSimpliferM :: (Simplifier a) => a -> Sudoku Grid -> Sudoku Grid #-}

-- | repeatedly apply each computationally cheap simplifier until none produce any updates
applyAllCheapSimplifiersM :: (MonadIO m, Alternative m) => m Grid -> m Grid
applyAllCheapSimplifiersM = runManySimplifiers $ \g -> join $ foldM (\mg' f -> return $ applyCheapSimpliferM f mg') (return g) cheapSimplifiers
{-# SPECIALIZE INLINE applyAllCheapSimplifiersM :: Sudoku Grid -> Sudoku Grid #-}

-- | run a computationally expensive simplifier one time, then run cheap simplifiers in case they produced updates that are amenable to simplification
applyExpensiveSimplifierM :: (Simplifier a, MonadIO m, Alternative m) => a -> Grid -> m Grid
applyExpensiveSimplifierM f g = runSimplifierOnce f g & applyAllCheapSimplifiersM
{-# SPECIALIZE INLINE applyExpensiveSimplifierM :: (Simplifier a) => a -> Grid -> Sudoku Grid #-}

-- | apply all computationally expensive simplifiers recursively until they no longer produce updates
applyAllExpensiveSimplifiersM :: (MonadIO m, Alternative m) => m Grid -> m Grid
applyAllExpensiveSimplifiersM = runManySimplifiers (\g -> foldM (flip applyExpensiveSimplifierM) g expensiveSimplifiers & applyAllCheapSimplifiersM)
{-# SPECIALIZE INLINE applyAllExpensiveSimplifiersM :: Sudoku Grid -> Sudoku Grid #-}

-- | recursively apply cheap and expensive simplifiers, one after the other, until the puzzle is solved, or we can no longer make progress
runBasicSolver :: Sudoku Grid -> Sudoku Grid
runBasicSolver mg = do
    g <- applyAllCheapSimplifiersM mg
    checkContradictions g
    if solved g
        then return g
        else do
            g' <- applyAllExpensiveSimplifiersM (return g)
            checkContradictions g'
            return g'
{-# INLINE runBasicSolver #-}

{- | use the basic solver until it fails to make progress, then try to prove what values cells cannot take. this uses backtracking to find contradictions.
this solver may actually solve the puzzle while looking for contradictions. however, because we're searching for contradictions, such lucky guesses aren't used.
instead, the solver tracks what values cells cannot take, removes those values from those cells, and attempts to resolve using the basic solver.
candidate cells are ordered by 1) the number of available values for the cell, followed by 2) how restricted the cell's neighborhood is, where we sum the
number of values each cell in the neighborhood can take as a proxy for restriction. a neighborhood, here, refers to the cells that can see the cell in question --
i.e. it's row/column/box.

this is a very slow process so we don't want to run it unless we have to. even the "expensive" solvers are much faster than trying a value, taking the puzzle to
a stable state where we can determine whether we were able to derive a contradiction or not, and then repeating that process for another value takes multiple seconds
where the basic solver will blitz out a whole puzzle in ~200ms.
-}
runBacktrackingSolver :: Sudoku Grid -> Sudoku Grid
runBacktrackingSolver = fix $ \rec mg -> do
    g <- runBasicSolver mg
    reportSolved g
    if solved g
        then return g
        else do
            g' <- go (findRestrictedCells g ^.. folded . _1) g
            checkContradictions g'
            reportSolved g'
            if solved g' || g' == g then return g' else rec (return g')
  where
    -- worker that first tries to find contradictions in a cell, then reruns the basic solver after updating the grid to remove
    -- values the solver was able to prove resulted in contradictions.
    go :: [CellPos] -> Grid -> Sudoku Grid
    go (loc : locs) g = do
        printNoticeStart loc
        put (S.empty @Digit, g) >> attemptToContradict applyAllCheapSimplifiersM loc <|> return ()
        cs <- use _1
        printNoticeEnd loc cs
        let g' = g & ixCell loc . _Possibly %~ (\\ cs) & contradictionSearched %~ S.insert loc
        checkContradictions g'
        g'' <- put (S.empty @Digit, g') >> runBasicSolver (return g')
        if solved g'' then return g'' else go locs g''
    go [] g = return g

printNotice :: (MonadIO m) => [String] -> m ()
printNotice ss = do
    printUnquoted $ replicate (9 * 12 + 3) '-'
    printUnquoted "NOTICE:"
    forM_ ss $ \s -> do
        printUnquoted s
    printUnquoted $ replicate (9 * 12 + 3) '-'

printNoticeStart :: (MonadIO m) => CellPos -> m ()
printNoticeStart loc =
    printNotice
        [ "using the backtracking solver to search for impossible values in " ++ showLoc loc
        , "another attempt to solve logically will be made after we've eliminated impossibilities from this cell."
        ]

printNoticeEnd :: (MonadIO m) => CellPos -> Contradicted -> m ()
printNoticeEnd loc cs =
    printNotice
        ["resuming logical solving with the knowledge that " ++ showLoc loc ++ " cannot be " ++ show (S.toList cs)]

printUnquoted :: (MonadIO m) => String -> m ()
printUnquoted = liftIO . print . Unquoted
{-# SPECIALIZE INLINE printUnquoted :: String -> Sudoku () #-}

-- | a `Cell`'s degree of `Restriction` is the number of possible values it can take.
restrictionScoreCell :: Cell -> Int
restrictionScoreCell = lengthOf (_Possibly . folded)
{-# INLINE restrictionScoreCell #-}

{- | `Traversal`s that pass through a given `Cell` form it's neighborhood -- that is, it's row/column/box. calculate the score for
each `Traversal` and sum. the `Restriction` score along a `Traversal` is the sum of the `Restriction` scores for each cell along it.
-}
restrictionScoreNeighborhood :: Grid -> CellPos -> Sum Int
restrictionScoreNeighborhood g loc = foldMap restrictionScoreTraversal (neighborhoodOf g loc)
  where
    restrictionScoreTraversal l = foldMapOf (runIndexedTraversal l) (Sum . restrictionScoreCell) g
{-# INLINE restrictionScoreNeighborhood #-}

{- | an `Eq`/`Ord` container to track restriction scores for `Cell`s, ensuring `Cell`s are compared first by their own
degree of `Restriction`. among `Cell`s that have the same degree of internal `Restriction`, we order them by
the degree of `Restriction` in their neighborhood.
-}
data Restriction = Restriction {cellScore :: !(Sum Int), neighborhoodScore :: !(Sum Int)} deriving (Eq, Ord)

restrictionScore :: Grid -> (CellPos, Cell) -> Restriction
restrictionScore g (loc, cell) = Restriction (Sum (restrictionScoreCell cell)) (restrictionScoreNeighborhood g loc)
{-# INLINE restrictionScore #-}

-- | a `Restriction`-ordered listing of `Cell`s
findRestrictedCells :: Grid -> [(CellPos, Cell)]
findRestrictedCells g =
    sortBy restrictionOrdering $
        g ^@.. grid . itraversed . indices (not . flip S.member (_contradictionSearched g)) . filtered (has _Possibly)
  where
    restrictionOrdering a b = restrictionScore g a `compare` restrictionScore g b
{-# INLINE findRestrictedCells #-}

{- | external interface to the solver. provide data to the solver in terms of `Constraint`s in JSON format and this function
reads them from the file and yields back the solved puzzle (if one could be found).
-}
runSolverOnFile :: String -> IO Grid
runSolverOnFile file = do
    g <- parseGrid file
    runSolver g

runSolver :: Grid -> IO Grid
runSolver g = runBacktrackingSolver >>> observeT (S.empty @Digit, g) >>> runWriterT >>> fmap fst $ return g

parseGrid :: String -> IO Grid
parseGrid file = do
    j <- BS.readFile file
    let rawConstraints = eitherDecodeStrict j ^. singular _Right
    let g = mkGrid $ mkRules rawConstraints
    return g

choosePossible :: (Alternative m, MonadLogic m, MonadState (s, Grid) m) => CellPos -> m Cell
choosePossible loc = use (_2 . singular (ixCell loc)) >>= foldrOf (_Possibly . folded) (interleave . pure . Known) empty
{-# SPECIALIZE INLINE choosePossible :: CellPos -> Sudoku Cell #-}

{- | search for values a `Cell` cannot take. it's completely possible that the solver accidentally solves the puzzle while
trying values for `Cell`s while trying to figure out what definitely doesn't work and why. however, because we're searching
for values that cause the solver to fail, we necessarily have to discard successful results. see the type of `Control.Monad.Logic.lnot`
to understand why.

we primarily use the `LogicStateT` monad in this function, moreso than anywhere else. `Grid` state is used as the backtracking half
of the state while we track values we've contradicted in `loc` in the global state component. the former is used within this
function and the latter is used to update the `Grid` backtracking began on.
-}
attemptToContradict ::
    (Monad m, MonadWriter BacktrackStateLog m) => (SudokuT m Grid -> SudokuT m Grid) -> CellPos -> SudokuT m ()
attemptToContradict act loc =
    choosePossible loc >>- \cell -> join . backtrack $ do
        _2 . ixCell loc .= cell
        g <- use _2
        lnot . act $ return g
        _1 %= S.insert (d cell)
        lift $ tell [BacktrackState g loc (d cell)]
        _2 . ixCell loc . _Possibly %= setOf (folded . filtered (/= d cell))
  where
    d cell = cell ^. singular _Known
{-# SPECIALIZE INLINE attemptToContradict :: (Sudoku Grid -> Sudoku Grid) -> CellPos -> Sudoku () #-}
