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
module Data.BitSet where

-- foldl' is exported by Prelude from GHC 9.10

import Control.DeepSeq (NFData)
import Control.Lens hiding (ifoldr)
import Data.Bits
import Data.Coerce (coerce)
import GHC.Base (Type)
import GHC.Generics
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
newtype BitSet c a = BitSet {getBits :: c}
    deriving (Eq, Ord, VP.Prim)

type role BitSet representational phantom

instance (NFData c) => NFData (BitSet c a)

deriving instance Generic (BitSet c a)

instance (VU.Unbox c, Generic c) => VU.IsoUnbox (BitSet c a) c where
    toURepr = coerce
    fromURepr = coerce

newtype instance VU.MVector s (BitSet c a) = MV_BitSet (VP.MVector s c)

newtype instance VU.Vector (BitSet c a) = V_BitSet (VP.Vector c)

deriving via (VU.UnboxViaPrim c) instance (VP.Prim c) => MVG.MVector MVU.MVector (BitSet c a)

deriving via (VU.UnboxViaPrim c) instance (VP.Prim c) => VG.Vector VU.Vector (BitSet c a)

instance (VP.Prim c) => VU.Unbox (BitSet c a)

instance (Enum a, Show a, Bits c, Num c) => Show (BitSet c a) where
    showsPrec p bs =
        showParen (p > 10) $
            showString "fromList " . shows (toList bs)

instance (Bits c) => Semigroup (BitSet c a) where
    (<>) = union

instance (Bits c, Num c) => Monoid (BitSet c a) where
    mempty = empty

instance (Enum a, Bits c, Num c) => Exts.IsList (BitSet c (a :: Type)) where
    type Item (BitSet c a) = a
    fromList = fromList
    toList = toList
    {-# INLINE fromList #-}
    {-# INLINE toList #-}

-- | Is the bit set empty?
{-# INLINE null #-}
null :: (Eq c, Num c) => BitSet c a -> Bool
null (BitSet bits) = bits == 0

-- | The number of elements in the bit set.
{-# INLINE size #-}
size :: (Bits c) => BitSet c a -> Int
size (BitSet bits) = popCount bits

-- | Ask whether the item is in the bit set.
{-# INLINE member #-}
member :: (Enum a, Bits c) => a -> BitSet c a -> Bool
member x (BitSet bits) = bits `testBit` fromEnum x

-- | The empty bit set.
{-# INLINE empty #-}
empty :: (Bits c, Num c) => BitSet c a
empty = BitSet 0

-- | Create a singleton set.
{-# INLINE singleton #-}
singleton :: (Enum a, Bits c, Num c) => a -> BitSet c a
singleton x = BitSet $! bit (fromEnum x)

-- | Insert an item into the bit set.
{-# INLINE insert #-}
insert :: (Enum a, Bits c) => a -> BitSet c a -> BitSet c a
insert x (BitSet bits) = BitSet $! bits `setBit` fromEnum x

-- | Delete an item from the bit set.
{-# INLINE delete #-}
delete :: (Enum a, Bits c) => a -> BitSet c a -> BitSet c a
delete x (BitSet bits) = BitSet $! bits `clearBit` fromEnum x

-- | The union of two bit sets.
{-# INLINE union #-}
union :: (Bits c) => BitSet c a -> BitSet c a -> BitSet c a
union (BitSet !bits1) (BitSet !bits2) = BitSet $! bits1 .|. bits2

-- | Difference of two bit sets.
{-# INLINE difference #-}
difference :: (Bits c) => BitSet c a -> BitSet c a -> BitSet c a
difference (BitSet !bits1) (BitSet !bits2) = BitSet $! bits1 .&. complement bits2

-- | See 'difference'.
infix 5 \\ -- comment to fool cpp: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#cpp-and-string-gaps

{-# INLINE (\\) #-}
(\\) :: (Bits c) => BitSet c a -> BitSet c a -> BitSet c a
(\\) = difference

-- | The intersection of two bit sets.
{-# INLINE intersection #-}
intersection :: (Bits c) => BitSet c a -> BitSet c a -> BitSet c a
intersection (BitSet !bits1) (BitSet !bits2) = BitSet $! bits1 .&. bits2

{- | Transform this bit set by applying a function to every value.
Resulting bit set may be smaller then the original.
-}
{-# INLINE map #-}
map :: (Enum a, Enum b, Bits c, Num c) => (a -> b) -> BitSet c a -> BitSet c b
map f = foldl' (\bs a -> f a `insert` bs) empty

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value. Each application of the operator is
evaluated before before using the result in the next application. This
function is strict in the starting value.
-}
{-# INLINE foldl' #-}
foldl' :: (Enum a, Bits c) => (b -> a -> b) -> b -> BitSet c a -> b
foldl' f z (BitSet bits) = go z (popCount bits) 0
  where
    go !acc 0 !_ = acc
    go !acc !n !b =
        if bits `testBit` b
            then go (f acc $ toEnum b) (pred n) (succ b)
            else go acc n (succ b)

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE foldr #-}
foldr :: (Enum a, Bits c) => (a -> b -> b) -> b -> BitSet c a -> b
foldr f z (BitSet bits) = go (popCount bits) 0
  where
    go 0 !_ = z
    go !n !b =
        if bits `testBit` b
            then toEnum b `f` go (pred n) (succ b)
            else go n (succ b)

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE ifoldr #-}
ifoldr :: (Enum a, Bits c) => (Int -> a -> b -> b) -> b -> BitSet c a -> b
ifoldr f z (BitSet bits) = go (popCount bits) 0
  where
    go 0 !_ = z
    go !n !b =
        if bits `testBit` b
            then f b (toEnum b) (go (pred n) (succ b))
            else go n (succ b)

{- | Reduce this bit set by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE foldrIndex #-}
foldrIndex :: (Enum a, Bits c) => (Int -> b -> b) -> b -> BitSet c a -> b
foldrIndex f z (BitSet bits) = go (popCount bits) 0
  where
    go 0 !_ = z
    go !n !b =
        if bits `testBit` b
            then b `f` go (pred n) (succ b)
            else go n (succ b)

first :: forall a c. (Enum a, Bounded a, FiniteBits c, Bits c) => BitSet c a -> Maybe a
first (BitSet !bits) =
    let !count = countTrailingZeros bits
    in if count <= fromEnum (maxBound @a) then Just (toEnum count) else Nothing
{-# INLINE first #-}

-- | Convert this bit set set to a list of elements.
{-# INLINE [0] toList #-}
toList :: (Enum a, Bits c, Num c) => BitSet c a -> [a]
toList bs = Exts.build (\k z -> foldr k z bs)

-- | Make a bit set from a list of elements.
{-# INLINE [0] fromList #-}
fromList :: (Enum a, Bits c, Num c) => [a] -> BitSet c a
fromList xs = BitSet $! List.foldl' (\i x -> i `setBit` fromEnum x) 0 xs

{-# RULES
"fromList/toList" forall bs. fromList (toList bs) = bs
"toList/fromList" forall bs. toList (fromList bs) = bs
"shiftL/shiftR" forall c r.
    Exts.uncheckedShiftL# (Exts.uncheckedShiftRL# (Exts.word16ToWord# c) r) r =
        Exts.word16ToWord# c
"unnecessaryAnd/Word16" forall c. Exts.and# c 65535## = c
"unnecessaryAnd/Word8" forall c. Exts.and# c 255## = c
    #-}

bsfolded ::
    (Enum a, Enum a', Bits b, Num b, Contravariant f, Applicative f) =>
    IndexedLensLike Int f (BitSet b a) (BitSet b a') a a'
bsfolded = ifoldring ifoldr
{-# INLINE bsfolded #-}

bitSetOf :: (Num c, Enum a, Bits c) => Fold s a -> s -> BitSet c a
bitSetOf f = foldlOf' f (flip insert) empty
