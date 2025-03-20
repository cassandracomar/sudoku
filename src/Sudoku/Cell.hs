{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sudoku.Cell where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (Bits)
import Data.Data (Typeable)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Primitive (Prim (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Generic.Lens (vectorTraverse)
import Data.Vector.Unboxed (IsoUnbox (fromURepr, toURepr))
import Data.Word (Word16, Word8)
import Foreign.C (CBool)
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
    deriving (Enum, Ord, Eq, Generic, Bounded)

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
    i -> error $ "invalid digit: " ++ show i

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

instance NFData Digit

instance ToJSON Digit

instance FromJSON Digit

newtype instance VU.MVector s Digit = MV_Digit (VP.MVector s Digit)

newtype instance VU.Vector Digit = V_Digit (VP.Vector Digit)

deriving via (VU.UnboxViaPrim Digit) instance VG.MVector MVU.MVector Digit

deriving via (VU.UnboxViaPrim Digit) instance VG.Vector VU.Vector Digit

instance VU.Unbox Digit

instance TextShow Digit where
    showb d = showb $ fromEnum d + 1

instance Show Digit where
    show = toString . showb

bsfolded :: (Enum a, Bits b, Num b) => Fold (A.BS.BitSet b a) a
bsfolded = folding A.BS.toList
{-# INLINE bsfolded #-}

newtype CellSet a = CellSet {_bitSet :: A.BS.BitSet Word16 a}
    deriving (Generic)
    deriving (Typeable, Semigroup, Monoid) via A.BS.BitSet Word16 a
    deriving (Ord, Eq, VP.Prim) via Word16

newtype instance VU.MVector s (CellSet a) = MV_CellSet (VP.MVector s Word16)

newtype instance VU.Vector (CellSet a) = V_CellSet (VP.Vector Word16)

deriving via (VU.UnboxViaPrim Word16) instance VG.MVector MVU.MVector (CellSet a)

deriving via (VU.UnboxViaPrim Word16) instance VG.Vector VU.Vector (CellSet a)

instance VU.Unbox (CellSet a)

instance (NFData a) => NFData (CellSet a)

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

newtype CellTag a = IsKnown CBool
    deriving (VP.Prim, Typeable, Generic, Ord, Eq)

instance NFData (CellTag a)

newtype instance VU.MVector s (CellTag a) = MV_CellTag (VP.MVector s (CellTag a))

newtype instance VU.Vector (CellTag a) = V_CellTag (VP.Vector (CellTag a))

deriving via (VU.UnboxViaPrim (CellTag a)) instance VG.MVector MVU.MVector (CellTag a)

deriving via (VU.UnboxViaPrim (CellTag a)) instance VG.Vector VU.Vector (CellTag a)

instance VU.Unbox (CellTag a)

newtype CellRepr a = CellRepr (CellTag a, Word16)
    deriving (Typeable, Generic, Ord, Eq)

instance NFData (CellRepr a)

instance VU.IsoUnbox (CellRepr a) (CellTag a, Word16) where
    toURepr (CellRepr cr) = cr
    fromURepr = CellRepr

newtype instance VU.MVector s (CellRepr a) = MV_CellRepr (VU.MVector s (CellTag a, Word16))

newtype instance VU.Vector (CellRepr a) = V_CellRepr (VU.Vector (CellTag a, Word16))

deriving via (CellRepr a `VU.As` (CellTag a, Word16)) instance VG.MVector MVU.MVector (CellRepr a)

deriving via (CellRepr a `VU.As` (CellTag a, Word16)) instance VG.Vector VU.Vector (CellRepr a)

instance VU.Unbox (CellRepr a)

data Cell a
    = KnownRepr {-# UNPACK #-} !Word16
    | Possibly {-# UNPACK #-} !(CellSet a)
    deriving (Typeable, Generic, Ord, Eq)

instance (NFData a) => NFData (Cell a)

instance VU.IsoUnbox (Cell a) (CellRepr a) where
    toURepr (KnownRepr b) = CellRepr (IsKnown 0, b)
    toURepr (Possibly (CellSet (A.BS.BitSet cs))) = CellRepr (IsKnown 1, cs)
    fromURepr (CellRepr (IsKnown 0, b)) = KnownRepr b
    fromURepr (CellRepr (IsKnown _, b)) = Possibly . CellSet . A.BS.BitSet $ b

newtype instance VU.MVector s (Cell a) = MV_Cell (VU.MVector s (CellRepr a))

newtype instance VU.Vector (Cell a) = V_Cell (VU.Vector (CellRepr a))

deriving via (Cell a `VU.As` CellRepr a) instance VG.MVector MVU.MVector (Cell a)

deriving via (Cell a `VU.As` CellRepr a) instance VG.Vector VU.Vector (Cell a)

instance VU.Unbox (Cell a)

instance (TextShow a, Enum a) => TextShow (Cell a) where
    showb (Possibly cs) = showb cs
    showb (KnownRepr d) = "     " <> (showb @a (toEnum @a (fromIntegral d)) <> "     ")

instance (TextShow a, Enum a) => Show (Cell a) where
    show = toString . showb

type CellPos = (Word8, Word8, Word8)

mkCell :: (Enum a) => Cell a
mkCell = Possibly (CellSet (A.BS.fromList [toEnum 0 ..]))

mkKnown :: (Enum a) => a -> Cell a
mkKnown = KnownRepr . fromIntegral . fromEnum

boxNumber :: Word8 -> Word8 -> Word8
boxNumber r c = fromIntegral $ (r - 1) `div` 3 * 3 + (c - 1) `div` 3 + 1

rowColumn :: Int -> CellPos
rowColumn i = (r, c, b)
  where
    r = fromIntegral $ i `div` 9 + 1
    c = fromIntegral $ i `rem` 9 + 1
    b = boxNumber r c

vindex :: CellPos -> Int
vindex (r, c, _) = fromIntegral $ (r - 1) * 9 + c - 1

cellIndex :: Iso' Int CellPos
cellIndex = iso rowColumn vindex
{-# INLINE cellIndex #-}

cellPos :: (VU.Unbox a, VU.Unbox b) => IndexedTraversal CellPos (VU.Vector a) (VU.Vector b) a b
cellPos = reindexed (view cellIndex) vectorTraverse
{-# INLINE cellPos #-}

boxIndex :: CellPos -> Word8
boxIndex (r, c, _) = (r - 1) `rem` 3 * 3 + (c - 1) `rem` 3 + 1

withBoxIndex :: Iso' CellPos (Word8, Word8, Word8, Word8)
withBoxIndex = iso (\(r, c, b) -> (r, c, b, boxIndex (r, c, b))) (\(r, c, b, _) -> (r, c, b))

boxIndexing :: Iso' CellPos (Word8, Word8)
boxIndexing =
    iso
        (\loc@(_, _, b) -> (b, boxIndex loc))
        (\(b, offset) -> ((b - 1) `div` 3 * 3 + (offset - 1) `div` 3 + 1, ((b - 1) `rem` 3) * 3 + (offset - 1) `rem` 3 + 1, b))

makeLenses ''CellSet
makePrisms ''CellSet
makeLenses ''A.BS.BitSet

_Known :: forall a. (Enum a) => Prism' (Cell a) a
_Known =
    prism'
        mkKnown
        ( \case
            KnownRepr d -> Just (toEnum (fromIntegral d))
            Possibly _ -> Nothing
        )

_Possibly :: forall a. Prism' (Cell a) (CellSet a)
_Possibly =
    prism'
        Possibly
        ( \case
            KnownRepr _ -> Nothing
            Possibly cs -> Just cs
        )

-- makePrisms ''Cell

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
