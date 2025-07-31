{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Bits (Bits ((.|.)), testBit, (!<<.), (!>>.))
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Primitive (Prim (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Generic.Lens (vectorTraverse)
import Data.Vector.Unboxed (IsoUnbox (..))
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import GHC.Word (Word16 (..))
import TextShow (Builder, TextShow (..), toString)
import TextShow.Data.Char (showbLitChar)

import Data.BitSet qualified as A.BS
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VG
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU

data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Ord, Eq, Generic)

instance Enum Digit where
    toEnum = intToDigit
    fromEnum = digitToInt

instance Bounded Digit where
    minBound = One
    maxBound = Nine

-- | rather than convert back and forth through `Int`, just look up the enum values
digitToWord16 :: Digit -> Word16
digitToWord16 = \case
    One -> W16# 0#Word16
    Two -> W16# 1#Word16
    Three -> W16# 2#Word16
    Four -> W16# 3#Word16
    Five -> W16# 4#Word16
    Six -> W16# 5#Word16
    Seven -> W16# 6#Word16
    Eight -> W16# 7#Word16
    Nine -> W16# 8#Word16

-- | as for `digitToWord16`, lookup the digit for an enum value directly
word16ToDigit :: Word16 -> Digit
word16ToDigit = \case
    W16# 0#Word16 -> One
    W16# 1#Word16 -> Two
    W16# 2#Word16 -> Three
    W16# 3#Word16 -> Four
    W16# 4#Word16 -> Five
    W16# 5#Word16 -> Six
    W16# 6#Word16 -> Seven
    W16# 7#Word16 -> Eight
    W16# 8#Word16 -> Nine

-- | as for `digitToWord16`, lookup the digit for an enum value directly
intToDigit :: Int -> Digit
intToDigit = \case
    0 -> One
    1 -> Two
    2 -> Three
    3 -> Four
    4 -> Five
    5 -> Six
    6 -> Seven
    7 -> Eight
    8 -> Nine

digitToInt :: Digit -> Int
digitToInt = \case
    One -> 0
    Two -> 1
    Three -> 2
    Four -> 3
    Five -> 4
    Six -> 5
    Seven -> 6
    Eight -> 7
    Nine -> 8

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

bsfolded ::
    (Enum a, Enum a', Bits b, Num b, Contravariant f, Applicative f) => LensLike f (A.BS.BitSet b a) (A.BS.BitSet b a') a a'
bsfolded = foldring A.BS.foldr
{-# INLINE bsfolded #-}

type role CellSet phantom

newtype CellSet a = CellSet {_bitSet :: A.BS.BitSet Word16 a}
    deriving (Generic)
    deriving (Semigroup, Monoid) via A.BS.BitSet Word16 a
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
                    <> ( fold (replicate (4 - A.BS.size ps + 1) (showbLitChar ' '))
                            <> ( fold (intersperse (showbLitChar ' ') (map showb $ A.BS.toList ps))
                                    <> ( fold (replicate (4 - A.BS.size ps + 1) (showbLitChar ' '))
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
isKnown c =
    let w = cellToWord16 c
    in testBit w 0

isPossibly :: forall a. Cell a -> Bool
isPossibly = not . isKnown

cellToWord16 :: Cell a -> Word16
cellToWord16 (KnownRepr w) = (w !<<. 1) .|. 0b1
cellToWord16 (Possibly (CellSet (A.BS.BitSet w))) = w !<<. 1

word16ToCell :: Word16 -> Cell a
word16ToCell w
    | testBit w 0 = KnownRepr (w !>>. 1)
    | otherwise = Possibly (CellSet (A.BS.BitSet (w !>>. 1)))

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
boxNumber !r !c = (r - 1) `div` 3 * 3 + (c - 1) `div` 3 + 1

rowColumn :: Int -> CellPos
rowColumn !i = (r, c, b)
  where
    !r = fromIntegral $! i `div` 9 + 1
    !c = fromIntegral $! i `rem` 9 + 1
    !b = boxNumber r c

vindex :: CellPos -> Int
vindex (!r, !c, _) = fromIntegral $! (r - 1) * 9 + c - 1

cellIndex :: Iso' Int CellPos
cellIndex = iso rowColumn vindex
{-# INLINE cellIndex #-}

cellPos :: (VU.Unbox a, VU.Unbox b) => IndexedTraversal CellPos (VU.Vector a) (VU.Vector b) a b
cellPos = reindexed (view cellIndex) vectorTraverse
{-# INLINE cellPos #-}

boxIndex :: CellPos -> Word8
boxIndex (!r, !c, _) = (r - 1) `rem` 3 * 3 + (c - 1) `rem` 3 + 1

withBoxIndex :: Iso' CellPos (Word8, Word8, Word8, Word8)
withBoxIndex = iso (\(r, c, b) -> (r, c, b, boxIndex (r, c, b))) (\(r, c, b, _) -> (r, c, b))

boxIndexing :: Iso' CellPos (Word8, Word8)
boxIndexing =
    iso
        (\loc@(_, _, b) -> (b, boxIndex loc))
        ( \(b, offset) ->
            ( (b - 1) `div` 3 * 3 + (offset - 1) `div` 3 + 1
            , ((b - 1) `rem` 3) * 3 + (offset - 1) `rem` 3 + 1
            , b
            )
        )

data RegionIndicator = Row | Column | Box deriving (Eq, Ord, Generic, Enum)

instance TextShow RegionIndicator where
    showb Row = "Row"
    showb Column = "Column"
    showb Box = "Box"

instance Show RegionIndicator where
    show = toString . showb

majorMinor :: RegionIndicator -> CellPos -> (RegionIndicator, Int, Int)
majorMinor Row (!r, !c, _) = (Row, fromIntegral r, fromIntegral c)
majorMinor Column (!r, !c, _) = (Column, fromIntegral c, fromIntegral r)
majorMinor Box loc@(_, _, !b) = (Box, fromIntegral b, fromIntegral (boxIndex loc))

fromMajorMinor :: (RegionIndicator, Int, Int) -> (RegionIndicator, CellPos)
fromMajorMinor (Row, !row, !col) = (Row, (fromIntegral row, fromIntegral col, boxNumber (fromIntegral row) (fromIntegral col)))
fromMajorMinor (Column, !col, !row) = (Column, (fromIntegral row, fromIntegral col, boxNumber (fromIntegral row) (fromIntegral col)))
fromMajorMinor (Box, !box, !boxIdx) = (Box, (fromIntegral box, fromIntegral boxIdx) ^. re boxIndexing)

_majorMinor :: Iso' (RegionIndicator, CellPos) (RegionIndicator, Int, Int)
_majorMinor = iso (uncurry majorMinor) fromMajorMinor

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
