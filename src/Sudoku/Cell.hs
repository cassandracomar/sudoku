{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Sudoku.Cell where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON (toEncoding))
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Foldable (fold)
import Data.Hashable (Hashable)
import Data.List (intersperse)
import Data.Primitive (Prim (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Generic.Lens (vectorTraverse)
import Data.Vector.Unboxed (IsoUnbox (..))
import Data.Word (Word16, Word8)
import GHC.Exts (DataToTag (dataToTag#), int2Word#, tagToEnum#, word16ToWord#, word2Int#, wordToWord16#)
import GHC.Generics (Generic)
import GHC.Word (Word16 (..))
import Numeric.QuoteQuot (quoteQuot, quoteRem)
import TextShow (Builder, TextShow (..), toString)
import TextShow.Data.Char (showbLitChar)

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VG
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word16Set qualified as A.BS
import GHC.Exts qualified as Exts

data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Ord, Eq, Generic)

instance Hashable Digit

instance Enum Digit where
    toEnum = intToDigit
    fromEnum = digitToInt

instance Bounded Digit where
    minBound = One
    maxBound = Nine

-- | rather than convert back and forth through `Int`, just look up the enum values
digitToWord16 :: Digit -> Word16
digitToWord16 d = W16# (wordToWord16# (int2Word# (dataToTag# d)))

-- | as for `digitToWord16`, lookup the digit for an enum value directly
word16ToDigit :: Word16 -> Digit
word16ToDigit (W16# w) = tagToEnum# (word2Int# (word16ToWord# w))

-- | as for `digitToWord16`, lookup the digit for an enum value directly
intToDigit :: Int -> Digit
intToDigit (Exts.I# i) = tagToEnum# i

digitToInt :: Digit -> Int
digitToInt d = Exts.I# (dataToTag# d)

instance VP.Prim Digit where
    sizeOfType# _ = sizeOfType# (Proxy @Word16)
    alignmentOfType# _ = alignmentOfType# (Proxy @Word16)
    indexByteArray# bs i = word16ToDigit (indexByteArray# @Word16 bs i)
    readByteArray# bs i s = let !(# s', r #) = readByteArray# bs i s in (# s', word16ToDigit r #)
    writeByteArray# bs i d = writeByteArray# bs i (digitToWord16 d)
    setByteArray# bs o l d = setByteArray# bs o l (digitToWord16 d)
    indexOffAddr# a i = word16ToDigit (indexOffAddr# a i)
    readOffAddr# a i s = let !(# s', r #) = readOffAddr# a i s in (# s', word16ToDigit r #)
    writeOffAddr# a i d = writeOffAddr# a i (digitToWord16 d)

instance VU.IsoUnbox Digit Word16 where
    toURepr = digitToWord16
    fromURepr = word16ToDigit

instance NFData Digit

instance ToJSON Digit where
    toEncoding d = toEncoding (fromEnum d)
    toJSON d = toJSON (fromEnum d)

instance FromJSON Digit where
    parseJSON d = toEnum . (+ (-1)) <$> parseJSON d

newtype instance VU.MVector s Digit = MV_Digit (VP.MVector s Digit)

newtype instance VU.Vector Digit = V_Digit (VP.Vector Digit)

deriving via (VU.UnboxViaPrim Digit) instance VG.MVector MVU.MVector Digit

deriving via (VU.UnboxViaPrim Digit) instance VG.Vector VU.Vector Digit

instance VU.Unbox Digit

instance TextShow Digit where
    showb d = showb $ fromEnum d + 1

instance Show Digit where
    show = toString . showb

type role CellSet phantom

newtype CellSet a = CellSet {_bitSet :: A.BS.BitSet a}
    deriving (Generic)
    deriving (Semigroup, Monoid) via A.BS.BitSet a
    deriving (Ord, Eq, VP.Prim) via Word16

instance (NFData a) => NFData (CellSet a)

newtype instance VU.MVector s (CellSet a) = MV_CellSet (VP.MVector s (CellSet a))

newtype instance VU.Vector (CellSet a) = V_CellSet (VP.Vector (CellSet a))

deriving via (VU.UnboxViaPrim (CellSet a)) instance VG.MVector MVU.MVector (CellSet a)

deriving via (VU.UnboxViaPrim (CellSet a)) instance VG.Vector VU.Vector (CellSet a)

instance VU.Unbox (CellSet a)

instance (Enum a, TextShow a) => TextShow (CellSet a) where
    showb (CellSet ps) =
        if A.BS.size ps < 5
            then
                "("
                    <> ( fold (replicate (fromIntegral (4 - A.BS.size ps + 1)) (showbLitChar ' '))
                            <> ( fold (intersperse (showbLitChar ' ') (map showb $ A.BS.toList ps))
                                    <> ( fold (replicate (fromIntegral (4 - A.BS.size ps + 1)) (showbLitChar ' '))
                                            <> ")"
                                       )
                               )
                       )
            else "           "

instance (Enum a, TextShow a) => Show (CellSet a) where
    show = toString . showb

data Cell a
    = KnownRepr {-# UNPACK #-} !Word16
    | Possibly {-# UNPACK #-} !(CellSet a)
    deriving (Generic, Ord, Eq)

type role Cell phantom

pattern Known :: (Enum a, VU.IsoUnbox a Word16) => a -> Cell a
pattern Known d <- KnownRepr (Just . fromURepr -> Just d)
    where
        Known d = KnownRepr (toURepr d)

instance (NFData a) => NFData (Cell a)

isKnown :: forall a. Cell a -> Bool
isKnown (KnownRepr _) = True
isKnown _ = False

isPossibly :: forall a. Cell a -> Bool
isPossibly = not . isKnown

cellToWord16 :: Cell a -> Word16
cellToWord16 (KnownRepr (W16# !w)) = W16# ((w `Exts.uncheckedShiftLWord16#` 1#) `Exts.orWord16#` 1#Word16)
cellToWord16 (Possibly (CellSet (A.BS.BitSet (W16# !w)))) = W16# (w `Exts.uncheckedShiftLWord16#` 1#)

word16ToCell :: Word16 -> Cell a
word16ToCell (W16# !w) = case Exts.andWord16# w 1#Word16 of
    1#Word16 -> KnownRepr (W16# (w `Exts.uncheckedShiftRLWord16#` 1#))
    _ -> Possibly (CellSet (A.BS.BitSet (W16# (w `Exts.uncheckedShiftRLWord16#` 1#))))

instance VP.Prim (Cell a) where
    sizeOfType# _ = sizeOfType# (Proxy @Word16)
    alignmentOfType# _ = alignmentOfType# (Proxy @Word16)
    indexByteArray# bs i = word16ToCell (indexByteArray# @Word16 bs i)
    readByteArray# bs i s = let !(# s', r #) = readByteArray# bs i s in (# s', word16ToCell r #)
    writeByteArray# bs i d = writeByteArray# bs i (cellToWord16 d)
    setByteArray# bs o l d = setByteArray# bs o l (cellToWord16 d)
    indexOffAddr# a i = word16ToCell (indexOffAddr# a i)
    readOffAddr# a i s = let !(# s', r #) = readOffAddr# a i s in (# s', word16ToCell r #)
    writeOffAddr# a i d = writeOffAddr# a i (cellToWord16 d)

newtype instance VU.MVector s (Cell a) = MV_Cell (VP.MVector s (Cell a))

newtype instance VU.Vector (Cell a) = V_Cell (VP.Vector (Cell a))

deriving via (VU.UnboxViaPrim (Cell a)) instance VG.MVector MVU.MVector (Cell a)

deriving via (VU.UnboxViaPrim (Cell a)) instance VG.Vector VU.Vector (Cell a)

instance VU.Unbox (Cell a)

instance (TextShow a, Enum a, VU.IsoUnbox a Word16) => TextShow (Cell a) where
    showb (Possibly cs) = showb cs
    showb (KnownRepr d) = "     " <> (showb @a (fromURepr d) <> "     ")

instance (TextShow a, Enum a, VU.IsoUnbox a Word16) => Show (Cell a) where
    show = toString . showb

type CellPos = (Word8, Word8, Word8)

mkCell :: (Enum a, Bounded a) => Cell a
mkCell = Possibly (CellSet (A.BS.fromList [minBound .. maxBound]))

mkKnown :: (VU.IsoUnbox a Word16) => a -> Cell a
mkKnown = KnownRepr . toURepr

boxNumber :: Word8 -> Word8 -> Word8
boxNumber !r !c = quot3 (r - 1) * 3 + quot3 (c - 1) + 1
{-# INLINE boxNumber #-}

rowColumn :: Int -> CellPos
rowColumn !i = (r, c, b)
  where
    !r = fromIntegral $! quot9 i + 1
    !c = fromIntegral $! rem9 i + 1
    !b = boxNumber r c
{-# INLINE rowColumn #-}

vindex :: CellPos -> Int
vindex (!r, !c, _) = fromIntegral $! (r - 1) * 9 + c - 1
{-# INLINE vindex #-}

cellIndex :: Iso' Int CellPos
cellIndex = iso rowColumn vindex
{-# INLINE cellIndex #-}

cells :: (VU.Unbox a, VU.Unbox b) => IndexedTraversal CellPos (VU.Vector a) (VU.Vector b) a b
cells = reindexed rowColumn vectorTraverse
{-# INLINE cells #-}

boxIndex :: CellPos -> Word8
boxIndex (!r, !c, _) = let !b = rem3 (r - 1) * 3 + rem3 (c - 1) + 1 in b
{-# INLINE boxIndex #-}

withBoxIndex :: Iso' CellPos (Word8, Word8, Word8, Word8)
withBoxIndex = iso (\(!r, !c, !b) -> (r, c, b, boxIndex (r, c, b))) (\(r, c, b, _) -> (r, c, b))
{-# INLINE withBoxIndex #-}

quot3 :: Word8 -> Word8
quot3 = $$(quoteQuot 3)
{-# INLINE quot3 #-}

quot9 :: Int -> Int
quot9 = $$(quoteQuot 9)
{-# INLINE quot9 #-}

rem3 :: Word8 -> Word8
rem3 = $$(quoteRem 3)
{-# INLINE rem3 #-}

rem9 :: Int -> Int
rem9 = $$(quoteRem 9)
{-# INLINE rem9 #-}

boxIndexing :: Iso' CellPos (Word8, Word8)
boxIndexing =
    iso
        (\loc@(_, _, !b) -> (b, boxIndex loc))
        ( \(!b, !offset) ->
            let
                !r = quot3 (b - 1) * 3 + quot3 (offset - 1) + 1
                !c = rem3 (b - 1) * 3 + rem3 (offset - 1) + 1
            in
                (r, c, b)
        )
{-# INLINE boxIndexing #-}

data RegionIndicator = Row | Column | Box deriving (Eq, Ord, Generic, Enum)

instance NFData RegionIndicator

instance Hashable RegionIndicator

instance TextShow RegionIndicator where
    showb Row = "Row"
    showb Column = "Column"
    showb Box = "Box"

instance Show RegionIndicator where
    show = toString . showb

majorMinor :: RegionIndicator -> CellPos -> (RegionIndicator, Int, Int)
majorMinor Row (!r, !c, _) = (Row, rInt, cInt)
  where
    !rInt = fromIntegral r
    !cInt = fromIntegral c
majorMinor Column (!r, !c, _) = (Column, cInt, rInt)
  where
    !rInt = fromIntegral r
    !cInt = fromIntegral c
majorMinor Box loc@(_, _, !b) = (Box, bInt, bIdxInt)
  where
    !bInt = fromIntegral b
    !bIdxInt = fromIntegral $! boxIndex loc
{-# INLINE majorMinor #-}

fromMajorMinor :: (RegionIndicator, Int, Int) -> (RegionIndicator, CellPos)
fromMajorMinor (Row, !row, !col) = (Row, (rInt, cInt, bInt))
  where
    !rInt = fromIntegral row
    !cInt = fromIntegral col
    !bInt = boxNumber rInt cInt
fromMajorMinor (Column, !col, !row) = (Column, (rInt, cInt, bInt))
  where
    !rInt = fromIntegral row
    !cInt = fromIntegral col
    !bInt = boxNumber rInt cInt
fromMajorMinor (Box, !box, !boxIdx) = (Box, loc)
  where
    !bInt = fromIntegral box
    !bIdxInt = fromIntegral boxIdx
    !loc = (bInt, bIdxInt) ^. re boxIndexing
{-# INLINE fromMajorMinor #-}

_majorMinor :: Iso' (RegionIndicator, CellPos) (RegionIndicator, Int, Int)
_majorMinor = iso (uncurry majorMinor) fromMajorMinor
{-# INLINE _majorMinor #-}

makeLenses ''CellSet
makePrisms ''CellSet
makeLenses ''A.BS.BitSet
makePrisms ''A.BS.BitSet

_Known :: forall a. (IsoUnbox a Word16) => Prism' (Cell a) a
_Known =
    prism'
        (KnownRepr . toURepr)
        ( \case
            KnownRepr d -> Just (fromURepr d)
            Possibly _ -> Nothing
        )
{-# INLINE _Known #-}

_Possibly :: forall a. Prism' (Cell a) (CellSet a)
_Possibly =
    prism'
        Possibly
        ( \case
            KnownRepr _ -> Nothing
            Possibly cs -> Just cs
        )
{-# INLINE _Possibly #-}

showLocB :: CellPos -> Builder
showLocB (r, c, _) = "r" <> showb r <> "c" <> showb c

showLoc :: CellPos -> String
showLoc = toString . showLocB

-- | a `Traversal` that targets `Cell`s that can possibly take a particular value in the solved `Grid`
possibly :: (Enum a) => a -> Traversal' (Cell a) (Cell a)
possibly d = filtered (maybe False (A.BS.member d) . (^? _Possibly . _CellSet))
{-# INLINE possibly #-}

-- | a `Traversal` that targets `Cell`s that can take any of the values in the provided `CellSet`
possiblyOf :: (Enum a) => CellSet a -> Traversal' (Cell a) (Cell a)
possiblyOf (CellSet ds) = filteredBy (_Possibly . _CellSet . filtered (not . A.BS.null . A.BS.intersection ds))
{-# INLINE possiblyOf #-}
