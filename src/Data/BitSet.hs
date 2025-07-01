{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

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
import Data.Bits
import GHC.Base (Type)
import GHC.Exts (IsList, build)
import GHC.Generics (Generic)
import Prelude hiding (foldl, foldl', foldr)

-- import Data.Array.Accelerate qualified as A
-- import Data.Array.Accelerate.Data.Bits qualified as A
import Data.List qualified as List
import GHC.Exts qualified as Exts

{- | A space-efficient implementation of a set data structure for
enumerated data types.
-}
newtype BitSet c a = BitSet {getBits :: c}
    deriving (Eq, Ord)

type role BitSet representational phantom

instance (NFData c) => NFData (BitSet c a)

deriving instance Generic (BitSet c a)

instance (Enum a, Show a, Bits c, Num c) => Show (BitSet c a) where
    showsPrec p bs =
        showParen (p > 10) $
            showString "fromList " . shows (toList bs)

instance (Bits c) => Semigroup (BitSet c a) where
    (<>) = union

instance (Bits c, Num c) => Monoid (BitSet c a) where
    mempty = empty

instance (Enum a, Bits c, Num c) => IsList (BitSet c (a :: Type)) where
    type Item (BitSet c a) = a
    fromList = fromList
    toList = toList
    {-# INLINE fromList #-}
    {-# INLINE toList #-}

-- instance (A.Elt c) => A.Elt (BitSet c a)

-- pattern BitSet_ :: (A.Elt c) => A.Exp c -> A.Exp (BitSet c a)
-- pattern BitSet_ bs = A.Pattern bs

-- {-# COMPLETE BitSet_ #-}

-- instance (A.Lift A.Exp c, A.Elt (A.Plain c)) => A.Lift A.Exp (BitSet c a) where
--     type Plain (BitSet c a) = BitSet (A.Plain c) a
--     lift (BitSet bs) = BitSet_ (A.lift bs)

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

{- | The intersection of two bit sets.
{\-# INLINE intersection_ #-\}
intersection_ :: (A.Bits c) => A.Exp (BitSet c a) -> A.Exp (BitSet c a) -> A.Exp (BitSet c a)
intersection_ = A.match \case
    (BitSet_ bits1) -> A.match \case
        (BitSet_ bits2) -> BitSet_ $! bits1 A..&. bits2
-}

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
{-# INLINE foldrIndex #-}
foldrIndex :: (Enum a, Bits c) => (Int -> b -> b) -> b -> BitSet c a -> b
foldrIndex f z (BitSet bits) = go (popCount bits) 0
  where
    go 0 !_ = z
    go !n !b =
        if bits `testBit` b
            then b `f` go (pred n) (succ b)
            else go n (succ b)

-- class IsIntegralExp c a where
--     fromRepr :: (A.HasCallStack) => A.Exp c -> A.Exp a
--     toRepr :: (A.HasCallStack) => A.Exp a -> A.Exp c

first :: forall a c. (Enum a, Bounded a, FiniteBits c, Bits c) => BitSet c a -> Maybe a
first = fmap toEnum . firstIndex
{-# INLINE first #-}

firstIndex :: forall a c. (Enum a, Bounded a, FiniteBits c, Bits c) => BitSet c a -> Maybe Int
firstIndex (BitSet !bits) =
    let !count = countTrailingZeros bits
    in if count <= fromEnum (maxBound @a) then Just count else Nothing
{-# INLINE firstIndex #-}

-- | Convert this bit set set to a list of elements.
{-# INLINE [0] toList #-}
toList :: (Enum a, Bits c, Num c) => BitSet c a -> [a]
toList bs = build (\k z -> foldr k z bs)

-- | Make a bit set from a list of elements.
{-# INLINE [0] fromList #-}
fromList :: (Enum a, Bits c, Num c) => [a] -> BitSet c a
fromList xs = BitSet $! List.foldl' (\i x -> i `setBit` fromEnum x) 0 xs

{-# RULES
"fromList/toList" forall bs. fromList (toList bs) = bs
"toList/fromList" forall bs. toList (fromList bs) = bs
    #-}
