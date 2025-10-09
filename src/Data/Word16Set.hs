{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Data.BitSet
Copyright   : [2019..2020] The Accelerate Team
License     : BSD3

Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
Stability   : experimental
Portability : non-portable (GHC extensions)
-}
module Data.Word16Set where

-- foldl' is exported by Prelude from GHC 9.10

import Control.DeepSeq (NFData)
import Control.Lens hiding (ifoldr)
import Data.Bits
import Data.Coerce (coerce)
import Data.Vector.Unboxed (UnboxViaPrim)
import Data.Word (Word16)
import GHC.Base (Type)
import GHC.Generics
import GHC.Word (Word16 (W16#))
import Prelude hiding (foldl, foldl', foldr)

import Data.List qualified as List
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import GHC.Exts qualified as Exts

{- | A space-efficient implementation of a set data structure for
enumerated data types.
-}
newtype BitSet a = BitSet {getBits :: Word16}
    deriving (Eq, Ord, VP.Prim)

type role BitSet phantom

instance NFData (BitSet a)

deriving instance Generic (BitSet a)

instance VU.IsoUnbox (BitSet a) Word16 where
    toURepr = coerce
    fromURepr = coerce

newtype instance VU.MVector s (BitSet a) = MV_BitSet (VP.MVector s Word16)

newtype instance VU.Vector (BitSet a) = V_BitSet (VP.Vector Word16)

deriving via UnboxViaPrim Word16 instance MVG.MVector MVU.MVector (BitSet a)

deriving via UnboxViaPrim Word16 instance VG.Vector VU.Vector (BitSet a)

instance VU.Unbox (BitSet a)

instance (Enum a, Show a) => Show (BitSet a) where
    showsPrec p bs =
        showParen (p > 10) $
            showString "fromList " . shows (toList bs)

instance Semigroup (BitSet a) where
    (<>) = union

instance Monoid (BitSet a) where
    mempty = empty

instance (Enum a) => Exts.IsList (BitSet (a :: Type)) where
    type Item (BitSet a) = a
    fromList = fromList
    toList = toList
    {-# INLINE fromList #-}
    {-# INLINE toList #-}

-- | Is the bit set empty?
{-# INLINE null #-}
null :: BitSet a -> Bool
null (BitSet (W16# 0#Word16)) = True
null _ = False

-- | The number of elements in the bit set.
{-# INLINE size #-}
size :: BitSet a -> Word
size (BitSet (W16# !bits)) = Exts.W# (Exts.popCnt16# (Exts.word16ToWord# bits))

-- | Ask whether the item is in the bit set.
{-# INLINE member #-}
member :: (Enum a) => a -> BitSet a -> Bool
member x bs =
    let !(Exts.I# i) = fromEnum x
    in member# i bs

{-# INLINE member# #-}
member# :: (Enum a) => Exts.Int# -> BitSet a -> Bool
member# !i (BitSet (W16# !bits)) = case Exts.andWord16# (Exts.uncheckedShiftRLWord16# bits i) 1#Word16 of
    1#Word16 -> True
    _ -> False

-- | The empty bit set.
{-# INLINE empty #-}
empty :: BitSet a
empty = BitSet 0

-- | Create a singleton set.
{-# INLINE singleton #-}
singleton :: (Enum a) => a -> BitSet a
singleton x =
    let !(Exts.I# x#) = fromEnum x
    in BitSet $! W16# (Exts.uncheckedShiftLWord16# 1#Word16 x#)

-- | Insert an item into the bit set.
{-# INLINE insert #-}
insert :: (Enum a) => a -> BitSet a -> BitSet a
insert x bits = bits `union` singleton x

-- | Delete an item from the bit set.
{-# INLINE delete #-}
delete :: (Enum a) => a -> BitSet a -> BitSet a
delete x (BitSet (W16# bits)) =
    let !(BitSet (W16# x#)) = singleton x
    in BitSet $! W16# (bits `Exts.andWord16#` Exts.notWord16# x#)

-- | The union of two bit sets.
{-# INLINE union #-}
union :: BitSet a -> BitSet a -> BitSet a
union (BitSet (W16# bits1)) (BitSet (W16# bits2)) = BitSet $! W16# (bits1 `Exts.orWord16#` bits2)

-- | Difference of two bit sets.
{-# INLINE difference #-}
difference :: BitSet a -> BitSet a -> BitSet a
difference (BitSet (W16# bits1)) (BitSet (W16# bits2)) = BitSet $! W16# (bits1 `Exts.andWord16#` Exts.notWord16# bits2)

-- | See 'difference'.
infix 5 \\ -- comment to fool cpp: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#cpp-and-string-gaps

{-# INLINE (\\) #-}
(\\) :: BitSet a -> BitSet a -> BitSet a
(\\) = difference

-- | The intersection of two bit sets.
{-# INLINE intersection #-}
intersection :: BitSet a -> BitSet a -> BitSet a
intersection (BitSet (W16# bits1)) (BitSet (W16# bits2)) = BitSet $! W16# (bits1 `Exts.andWord16#` bits2)

{- | Transform this bit set by applying a function to every value.
Resulting bit set may be smaller then the original.
-}
{-# INLINE map #-}
map :: (Enum a, Enum b) => (a -> b) -> BitSet a -> BitSet b
map f = foldl' (\bs a -> f a `insert` bs) empty

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value. Each application of the operator is
evaluated before before using the result in the next application. This
function is strict in the starting value.
-}
{-# INLINE foldl' #-}
foldl' :: forall a b. (Enum a) => (b -> a -> b) -> b -> BitSet a -> b
foldl' f z bs = go z (size bs) 0
  where
    go :: b -> Word -> Int -> b
    go !acc 0 !_ = acc
    go !acc !n !b =
        let !(Exts.I# i) = b
        in if member# i bs
            then go (f acc $! toEnum b) (pred n) (succ b)
            else go acc n (succ b)

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE foldr #-}
foldr :: forall a b. (Enum a) => (a -> b -> b) -> b -> BitSet a -> b
foldr f z bs = go (size bs) 0
  where
    go :: Word -> Int -> b
    go 0 !_ = z
    go !n !b =
        let !(Exts.I# i) = b
        in if member# i bs
            then toEnum b `f` go (pred n) (succ b)
            else go n (succ b)

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE ifoldr #-}
ifoldr :: forall a b. (Enum a) => (Int -> a -> b -> b) -> b -> BitSet a -> b
ifoldr f z bs = go (size bs) 0
  where
    go :: Word -> Int -> b
    go 0 !_ = z
    go !n !b =
        let !(Exts.I# i) = b
        in if member# i bs
            then f b (toEnum b) (go (pred n) (succ b))
            else go n (succ b)

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE foldrIndex #-}
foldrIndex :: (Enum a) => (Int -> b -> b) -> b -> BitSet a -> b
foldrIndex f z bs = go (size bs) 0
  where
    go 0 !_ = z
    go !n !b =
        let !(Exts.I# i) = b
        in if member# i bs
            then b `f` go (pred n) (succ b)
            else go n (succ b)

first :: forall a. (Enum a, Bounded a) => BitSet a -> Maybe a
first (BitSet (W16# bits)) =
    let !count = Exts.I# (Exts.word2Int# (Exts.ctz16# (Exts.word16ToWord# bits)))
    in if count <= fromEnum (maxBound @a) then Just (toEnum count) else Nothing
{-# INLINE first #-}

last :: forall a. (Enum a, Bounded a) => BitSet a -> Maybe a
last (BitSet (W16# bits)) =
    let
        leading = Exts.I# (Exts.word2Int# (Exts.clz16# (Exts.word16ToWord# bits)))
        !count = 16 - leading
    in
        if count >= fromEnum (minBound @a) && count <= fromEnum (maxBound @a) then Just (toEnum count) else Nothing
{-# INLINE last #-}

-- | Convert this bit set set to a list of elements.
{-# INLINE [0] toList #-}
toList :: (Enum a) => BitSet a -> [a]
toList bs = Exts.build (\k z -> foldr k z bs)

-- | Make a bit set from a list of elements.
{-# INLINE [0] fromList #-}
fromList :: (Enum a) => [a] -> BitSet a
fromList xs = BitSet $! List.foldl' (\i x -> i `setBit` fromEnum x) 0 xs

{-# RULES
"fromList/toList" forall bs. fromList (toList bs) = bs
"toList/fromList" forall bs. toList (fromList bs) = bs
"shiftL/shiftR" forall c r.
    Exts.uncheckedShiftL# (Exts.uncheckedShiftRL# (Exts.word16ToWord# c) r) r =
        Exts.word16ToWord# c
"shiftL/shiftR/Word16" forall c r.
    Exts.uncheckedShiftLWord16# (Exts.uncheckedShiftRLWord16# c r) r =
        c
"unnecessaryAnd/Word16" forall c. Exts.and# c 65535## = c
"unnecessaryAnd/Word8" forall c. Exts.and# c 255## = c
    #-}

bsfolded ::
    (Enum a, Enum a', Contravariant f, Applicative f) =>
    IndexedLensLike Int f (BitSet a) (BitSet a') a a'
bsfolded = ifoldring ifoldr
{-# INLINE bsfolded #-}

bsindices ::
    (Enum a, Enum a', Contravariant f, Applicative f) =>
    IndexedLensLike Int f (BitSet a) (BitSet a') Int b
bsindices = ifoldring (\f -> foldrIndex (\i b -> f i i b))
{-# INLINE bsindices #-}

bitSetOf :: (Enum a) => Fold s a -> s -> BitSet a
bitSetOf f = foldlOf' f (flip insert) empty
