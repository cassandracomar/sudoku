{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Sudoku.Grid where

import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Class (MonadReader)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), ToJSON, defaultOptions, genericParseJSON)
import Data.Default (Default (def))
import Data.Foldable (Foldable (fold))
import Data.List (intersperse)
import Data.Set.Lens (setOf)
import Data.String (IsString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (fromLazyText)
import Data.Vector.Generic.Lens (ordinals)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Word (Word16)
import Sudoku.Cell
import TextShow (Builder, TextShow (showb), toLazyText, toString, unlinesB)
import TextShow.Data.Char (showbLitChar)
import TextShow.Data.List (showbListWith)

-- import Data.Array.Accelerate qualified as A
import Data.BitSet qualified as A.BS
import Data.List qualified as L
import Data.Set qualified as S
import Data.Text.Lazy.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)
{-# INLINE ensure #-}

guarded :: (Alternative m, Monad m) => m Bool -> m () -> m ()
guarded cond act = (cond >>= guard) *> act <|> pure ()
{-# INLINE guarded #-}

type SolverMonad m = (MonadIO m, Alternative m, MonadReader SolverOptions m)

data SolverOptions = SolverOptions {_fp :: FilePath, _testNoLog :: Bool, _verbose :: Bool}

instance Default SolverOptions where
    def = SolverOptions{_fp = "", _testNoLog = True, _verbose = False}

makeLenses ''SolverOptions

{- | the name lies slightly -- when running an automated test, we don't want any logging.
so silence logging in that case but otherwise output the log message, whether verbose logging
is enabled or not.
-}
printQuiet :: (SolverMonad m) => Text -> m ()
printQuiet = guarded (view testNoLog <&> not) . (liftIO . T.putStrLn)
{-# INLINE printQuiet #-}

checkVerbosity :: (SolverMonad m) => m Bool
checkVerbosity = view verbose
{-# INLINE checkVerbosity #-}

printVerbose :: (SolverMonad m) => Text -> m ()
printVerbose t = guarded checkVerbosity (printQuiet t)
{-# INLINE printVerbose #-}

type RowIx = Word8

type ColIx = Word8

data Constraint = Given !CellPos !Digit
    -- \| KillerCage {_sum :: !Word8, _cells :: !Cell}
    deriving stock (Ord, Eq, Generic)

instance NFData Constraint

makePrisms ''Constraint
makeLenses ''Constraint

newtype RawConstraintRows = RawConstraintRows {_rawrows :: [RawConstraintRow]} deriving stock (Ord, Eq, Show, Generic)

instance ToJSON RawConstraintRows

fixFields :: (IsString s, Show s, Eq s, Semigroup s) => s -> s
fixFields "_rawrows" = "rows"
fixFields "_row" = "row"
fixFields "_columns" = "columns"
fixFields "_column" = "column"
fixFields "_value" = "value"
fixFields field = error . show $ "unknown field: " <> field

instance FromJSON RawConstraintRows where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = fixFields}

data RawConstraintRow = RawConstraintRow {_row :: !RowIx, _columns :: ![RawConstraintColumn]}
    deriving stock (Ord, Eq, Show, Generic)

instance ToJSON RawConstraintRow

instance FromJSON RawConstraintRow where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = fixFields}

data RawConstraintColumn = RawConstraintColumn {_column :: !ColIx, _value :: !Int}
    deriving stock (Ord, Eq, Show, Generic)

instance ToJSON RawConstraintColumn

instance FromJSON RawConstraintColumn where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = fixFields}

makeLenses ''RawConstraintRows
makeLenses ''RawConstraintRow
makeLenses ''RawConstraintColumn

mkConstraint :: RowIx -> RawConstraintColumn -> Constraint
mkConstraint rix c = Given (rix, c ^. column, boxNumber rix (c ^. column)) (c ^. value . to (\i -> toEnum (i - 1)))

mkGivens :: RawConstraintRow -> [Constraint]
mkGivens r = over mapped (mkConstraint (r ^. row)) (r ^. columns)

newtype Rules = Rules
    { _constraints :: [Constraint]
    }
    deriving stock (Ord, Eq, Generic)

instance NFData Rules

makeLenses ''Rules

mkRules :: RawConstraintRows -> Rules
mkRules rs = Rules{_constraints = Data.Foldable.fold $ over mapped mkGivens (rs ^. rawrows)}

data CommonPossibilities a = CommonPossibilities
    { _commonCells :: {-# UNPACK #-} !(S.Set (CellPos, Cell a))
    , _sharedPossibilities :: {-# UNPACK #-} !(S.Set a)
    }
    deriving (Eq, Ord, Generic)

instance (NFData a) => NFData (CommonPossibilities a)

makeLenses ''CommonPossibilities

newtype Unquoted = Unquoted Text

instance TextShow Unquoted where
    showb (Unquoted s) = fromLazyText s

instance Show Unquoted where
    show = toString . showb

textShow :: (TextShow a) => a -> Text
textShow = toLazyText . showb

instance (Ord a) => Semigroup (CommonPossibilities a) where
    (<>) :: CommonPossibilities a -> CommonPossibilities a -> CommonPossibilities a
    cp <> cp' =
        cp
            & sharedPossibilities
                %~ S.union (cp' ^. sharedPossibilities)
            & commonCells
                %~ S.union (cp' ^. commonCells)
    {-# INLINE (<>) #-}

instance (Ord a) => Monoid (CommonPossibilities a) where
    mempty = CommonPossibilities mempty mempty

instance (TextShow a) => TextShow (CommonPossibilities a) where
    showb cp = "Tuple {cells=" <> (showbListWith showLocB locs <> (", digits=" <> (showb poss <> "}")))
      where
        locs = cp ^.. commonCells . folded . _1
        poss = cp ^.. sharedPossibilities . folded

instance (TextShow a) => Show (CommonPossibilities a) where
    show = toString . showb

type PartitionedPossibilities a = [CommonPossibilities a]

data Grid a = Grid
    { _grid :: {-# UNPACK #-} !(VU.Vector (Cell a))
    , _knownTuples :: !(S.Set (CommonPossibilities a))
    , _contradictionSearched :: !(S.Set CellPos)
    , _isSolved :: !Bool
    }
    deriving (Generic)

deriving instance (Eq a, Ord a, VU.Unbox (Cell a)) => Ord (Grid a)

deriving instance (Eq a, VU.Unbox (Cell a)) => Eq (Grid a)

instance (NFData a) => NFData (Grid a)

data ByLoc a = ByLoc {v :: a, regionNumber :: Int, region :: RegionIndicator}
    deriving (Generic)

allIndices :: VU.Vector CellPos
allIndices = VU.imap (\i _ -> i ^. cellIndex) (VU.replicate 81 ())

allIndicesV :: V.Vector CellPos
allIndicesV = V.imap (\i _ -> i ^. cellIndex) (V.replicate 81 ())

makeLenses ''Grid

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

instance (VU.Unbox (Cell a), Ord a, Enum a, Bounded a) => Default (Grid a) where
    def = Grid (VU.replicate 81 mkCell) mempty S.empty False

rowAt :: (VU.Unbox (Cell a)) => Word8 -> SudokuSetTraversal a
rowAt r =
    IndexedTraversal $ grid . reindexed rowColumn (ordinals [i | i <- [0 .. 80], let (r', _, _) = i ^. cellIndex, r == r'])
{-# INLINE rowAt #-}

colAt :: (VU.Unbox (Cell a)) => Word8 -> SudokuSetTraversal a
colAt c =
    IndexedTraversal $ grid . reindexed rowColumn (ordinals [i | i <- [0 .. 80], let (_, c', _) = i ^. cellIndex, c == c'])
{-# INLINE colAt #-}

boxAt :: (VU.Unbox (Cell a)) => Word8 -> SudokuSetTraversal a
boxAt b =
    IndexedTraversal $ grid . reindexed rowColumn (ordinals [i | i <- [0 .. 80], let (_, _, b') = i ^. cellIndex, b == b'])
{-# INLINE boxAt #-}

indicesAcross :: RegionIndicator -> Word8 -> V.Vector CellPos
indicesAcross Row r = [(r', c', b') | (r', c', b') <- allIndicesV, r == r']
indicesAcross Column c = [(r', c', b') | (r', c', b') <- allIndicesV, c == c']
indicesAcross Box b = [(r', c', b') | (r', c', b') <- allIndicesV, b == b']

allSame :: (CellPos -> CellPos -> Bool) -> V.Vector CellPos -> Bool
allSame p locs
    | V.length locs > 0 = V.all (p (V.head locs)) locs
    | otherwise = False

sameBox :: CellPos -> CellPos -> Bool
sameBox (_, _, b) (_, _, b') = b == b'

sameRow :: CellPos -> CellPos -> Bool
sameRow (r, _, _) (r', _, _) = r == r'

sameCol :: CellPos -> CellPos -> Bool
sameCol (_, c, _) (_, c', _) = c == c'

differenceOn ::
    Grid a
    -> SudokuSetTraversal a
    -> SudokuSetTraversal a
    -> SudokuSetTraversal a
differenceOn g l m = IndexedTraversal $ runIndexedTraversal l . indices (not . flip S.member s)
  where
    s = setOf (runIndexedTraversal m . asIndex) g
{-# INLINE differenceOn #-}

(@\\) ::
    SudokuSetTraversal a
    -> SudokuSetTraversal a
    -> Grid a
    -> SudokuSetTraversal a
l @\\ m = \g -> differenceOn g l m
{-# INLINE (@\\) #-}

infixl 4 @\\

removePossibilitiesOf :: (Enum a) => Fold s a -> s -> Cell a -> Cell a
removePossibilitiesOf l s = over (_Possibly . _CellSet) (A.BS.\\ A.BS.fromList (s ^.. l))
{-# INLINE removePossibilitiesOf #-}

removePossibilitiesOfOn :: (Enum a) => IndexedTraversal' CellPos (Grid a) (Cell a) -> Fold s a -> s -> Grid a -> Grid a
removePossibilitiesOfOn l d s = over l (removePossibilitiesOf d s)
{-# INLINE removePossibilitiesOfOn #-}

removePossibilities :: (Data.Foldable.Foldable t, Enum a) => t a -> Cell a -> Cell a
removePossibilities = removePossibilitiesOf folded
{-# INLINE removePossibilities #-}

removePossibility :: (Enum a) => a -> Cell a -> Cell a
removePossibility d = removePossibilities (S.singleton d)
{-# INLINE removePossibility #-}

boxIndicator :: RegionIndicator -> Builder
boxIndicator Row = dashRow
boxIndicator Column = "|"
boxIndicator _ = ""

addBoxes :: RegionIndicator -> [Builder] -> [Builder]
addBoxes ri = L.intercalate [boxIndicator ri] . chunksOf 6

showRow :: (TextShow a, Enum a, VU.IsoUnbox a Word16) => Grid a -> Word8 -> Builder
showRow g i = fold $ "|" : (addBoxes Column displayRow <> ["|"])
  where
    displayRow = intersperse "|" (g ^.. runIndexedTraversal (rowAt i) . to showb)

underlineRow :: Builder
underlineRow = fold $ replicate (9 * 12 + 3) (showbLitChar '_')

dashRow :: Builder
dashRow = fold $ replicate (9 * 12 + 3) "-"

showRows :: (TextShow a, Enum a, VU.IsoUnbox a Word16) => Grid a -> [Builder]
showRows g = dashRow : addBoxes Row (intersperse underlineRow ([1 .. 9] <&> showRow g)) <> [dashRow]

instance (TextShow a, Enum a, VU.IsoUnbox a Word16) => TextShow (Grid a) where
    showb g = unlinesB $ showRows g <> kts
      where
        kts = case lengthOf (knownTuples . folded) g > 0 of
            True -> "" : "Known Tuples:" : toListOf (knownTuples . folded . to showb) g
            False -> []
    {-# SPECIALIZE showb :: Grid Digit -> Builder #-}

instance (TextShow a, Enum a, VU.IsoUnbox a Word16) => Show (Grid a) where
    show = toString . showb

type SudokuSetTraversal a = ReifiedIndexedTraversal' CellPos (Grid a) (Cell a)

mkGrid :: Rules -> Grid Digit
mkGrid givenRules =
    Grid{_grid = mkBoard, _knownTuples = S.empty, _contradictionSearched = S.empty, _isSolved = False}
  where
    givenDigits = (givenRules ^.. constraints . traversed . _Given) <&> ((_2 %~ view (re _Known)) . (_1 %~ vindex))
    mkBoard = VU.replicate 81 mkCell VU.// givenDigits

type instance IxValue (Grid a) = Cell a

type instance Index (Grid a) = CellPos

instance Ixed (Grid a) where
    ix loc = grid . cells . index loc
    {-# INLINE ix #-}
