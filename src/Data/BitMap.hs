{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.BitMap where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IndexedFold, IxValue, Ixed (..), asIndex, filtered, ifoldlMOf, ifoldring, lens)
import Control.Monad.ST (runST)
import Data.Bits (Bits)
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (IsoUnbox (..))
import Data.Word (Word16)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import GHC.TypeLits.Singletons (KnownNat)
import TextShow (TextShow (showbList, showbPrec), showbParen)
import Prelude hiding (foldl, foldl', foldr)

import Data.BitSet qualified as BS
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Vector.Unboxed.Mutable.Sized qualified as MVSU
import Data.Vector.Unboxed.Sized qualified as VSU
import GHC.Exts qualified as Exts

-- | a fixed size map stored as an unboxed vector.
newtype BitMap (n :: Nat) (c :: Type) (k :: Type) (v :: Type) where
    BitMap :: {_bitMap :: VSU.Vector n c} -> BitMap n c k v

-- | a `9` entry map
type DigitMap k v = BitMap 9 Word16 k v

deriving instance Generic (BitMap n c k v)

deriving via (VSU.Vector n c) instance (NFData k) => NFData (BitMap n c k v)

deriving via (VSU.Vector n c) instance (VU.Unbox c, Eq c, KnownNat n) => Eq (BitMap n c k v)

deriving via (VSU.Vector n c) instance (VU.Unbox c, Ord c, KnownNat n) => Ord (BitMap n c k v)

instance (Enum k, VU.IsoUnbox v c, Show k, Show v, VU.Unbox c, KnownNat n) => Show (BitMap n c k v) where
    showsPrec p bm = showParen (p > 10) $ showString "fromList " . shows (toList bm)

instance (Enum k, VU.IsoUnbox v c, TextShow k, TextShow v, VU.Unbox c, KnownNat n, TextShow k) => TextShow (BitMap n c k v) where
    showbPrec p bm = showbParen (p > 10) $ "fromList " <> showbList (toList bm)

instance (Monoid v, VU.IsoUnbox v c, VU.Unbox c, KnownNat n, Eq c) => Semigroup (BitMap n c k v) where
    (<>) = mergeWith (<>)
    {-# INLINE (<>) #-}

instance (Monoid v, VU.IsoUnbox v c, VU.Unbox c, KnownNat n, Eq c) => Monoid (BitMap n c k v) where
    mempty = empty
    {-# INLINE mempty #-}

instance (KnownNat n) => VU.IsoUnbox (BitMap n c k v) (VSU.Vector n c) where
    toURepr = coerce
    fromURepr = coerce

newtype instance VU.MVector s (BitMap n c k v) = MV_BitMap (VU.MVector s (VSU.Vector n c))

newtype instance VU.Vector (BitMap n c k v) = V_BitMap (VU.Vector (VSU.Vector n c))

deriving via (VSU.Vector n c) instance (VU.Unbox c, KnownNat n) => MVG.MVector MVU.MVector (BitMap n c k v)

deriving via (VSU.Vector n c) instance (VU.Unbox c, KnownNat n) => VG.Vector VU.Vector (BitMap n c k v)

instance (VU.Unbox c, KnownNat n) => VU.Unbox (BitMap n c k v)

{-# INLINE insertWith #-}
insertWith ::
    forall (n :: Nat) c k v.
    (Enum k, VU.Unbox c, VU.IsoUnbox v c) => (v -> v -> v) -> k -> v -> BitMap n c k v -> BitMap n c k v
insertWith f k v (BitMap bm) = BitMap $! bm `VSU.unsafeUpd` [(fromEnum k, toURepr $ existing `f` v)]
  where
    existing :: v
    existing = fromURepr $ bm `VSU.unsafeIndex` fromEnum k

{-# INLINE insertAllWith #-}
insertAllWith ::
    forall (n :: Nat) c k v.
    (Enum k, VU.Unbox c, VU.IsoUnbox v c) =>
    (v -> v -> v)
    -> BitMap n c k v
    -> [(k, v)]
    -> BitMap n c k v
insertAllWith f (BitMap bm) vs = BitMap $! bm `VSU.unsafeUpd` fmap (uncurry mergeExisting) vs
  where
    mergeExisting k v = (fromEnum k, toURepr $! fromURepr (bm `VSU.unsafeIndex` fromEnum k) `f` v)

{- | as this map is fixed size, we initialize all entries to `mempty`. consequently, `get` is not a partial function
as is usual for map types.
-}
get :: forall (n :: Nat) c k v. (Enum k, VU.IsoUnbox v c, VU.Unbox c) => k -> BitMap n c k v -> v
get k (BitMap bm) = fromURepr $ bm `VSU.unsafeIndex` fromEnum k
{-# INLINE get #-}

{-# INLINE (!) #-}
(!) :: forall (n :: Nat) c k v. (Enum k, VU.IsoUnbox v c, VU.Unbox c) => BitMap n c k v -> k -> v
(!) = flip get

{-# INLINE empty #-}
empty :: forall (n :: Nat) c k v. (VU.IsoUnbox v c, VU.Unbox c, VU.IsoUnbox v c, Monoid v, KnownNat n) => BitMap n c k v
empty = BitMap $! VSU.replicate (toURepr @v mempty)

{-# INLINE singleton #-}
singleton ::
    forall (n :: Nat) c k v. (Enum k, VU.IsoUnbox v c, VU.Unbox c, Monoid v, KnownNat n) => k -> v -> BitMap n c k v
singleton k v = BitMap $! _bitMap @_ @_ @_ @v empty `VSU.unsafeUpd` [(fromEnum k, toURepr v)]

{-# INLINE mergeWith #-}
mergeWith ::
    forall (n :: Nat) c k v.
    (VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => (v -> v -> v) -> BitMap n c k v -> BitMap n c k v -> BitMap n c k v
mergeWith f (BitMap b) (BitMap b') = BitMap $! VSU.zipWith f' b b'
  where
    f' a a' = toURepr $ fromURepr a `f` fromURepr a'

{-# INLINE mergeWithKey #-}
mergeWithKey ::
    forall (n :: Nat) c k v.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) =>
    (k -> v -> v -> v) -> BitMap n c k v -> BitMap n c k v -> BitMap n c k v
mergeWithKey f (BitMap b) (BitMap b') = BitMap $! VSU.izipWith f' b b'
  where
    f' k a a' = toURepr $ fromURepr a `g` fromURepr a'
      where
        g = f (toEnum $ fromEnum k)

{- | Reduce this bit map by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE foldr #-}
foldr ::
    forall (n :: Nat) c k v b.
    (Monoid v, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => (v -> b -> b) -> b -> BitMap n c k v -> b
foldr f z (BitMap bs) = go (VSU.length bs) 0
  where
    go 0 !_ = z
    go !n !i = fromURepr (bs `VSU.unsafeIndex` i) `f` go (pred n) (succ i)

{- | Reduce this bit map by applying a binary function to all elements,
using the given starting value. this version is strict in the accumulator
and associates to the left. it's also tail-recursive.
-}
{-# INLINE foldl' #-}
foldl' ::
    forall (n :: Nat) c k v b. (VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => (b -> v -> b) -> b -> BitMap n c k v -> b
foldl' f z (BitMap bs) = go z (VSU.length bs) 0
  where
    go !acc 0 !_ = acc
    go !acc !n !i = go (f acc . fromURepr $ bs `VSU.unsafeIndex` i) (pred n) (succ i)

{- | Reduce this bit map by applying a binary function to all elements,
using the given starting value.
-}
{-# INLINE ifoldr #-}
ifoldr ::
    forall (n :: Nat) c k v b.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => (k -> v -> b -> b) -> b -> BitMap n c k v -> b
ifoldr f z (BitMap bs) = go (VSU.length bs) 0
  where
    go 0 !_ = z
    go !n !i = f (toEnum i) (fromURepr (bs `VSU.unsafeIndex` i)) $ go (pred n) (succ i)

{- | Reduce this bit map by applying an indexed function to all elements,
using the given starting value. this version is strict in the accumulator
and associates to the left. it's also tail-recursive.
-}
{-# INLINE ifoldl' #-}
ifoldl' ::
    forall (n :: Nat) c k v b.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => (k -> b -> v -> b) -> b -> BitMap n c k v -> b
ifoldl' f z (BitMap bs) = go z (VSU.length bs) 0
  where
    go !acc 0 _ = acc
    go !acc !n !i = go (f (toEnum i) acc . fromURepr $ bs `VSU.unsafeIndex` i) (pred n) (succ i)

{-# INLINE ifoldMap' #-}
ifoldMap' ::
    forall (n :: Nat) c k v b.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, Monoid b, KnownNat n) => (k -> v -> b) -> BitMap n c k v -> b
ifoldMap' f = ifoldl' (\k b v -> b <> f k v) mempty

{-# INLINE ifoldMap #-}
ifoldMap ::
    forall (n :: Nat) c k v b.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, Monoid b, KnownNat n) => (k -> v -> b) -> BitMap n c k v -> b
ifoldMap f = ifoldr (\k v b -> b <> f k v) mempty

{-# INLINE foldMap #-}
foldMap ::
    forall (n :: Nat) c k v b.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, Monoid b, KnownNat n) => (v -> b) -> BitMap n c k v -> b
foldMap f = ifoldMap (const f)

{-# INLINE foldMap' #-}
foldMap' ::
    forall (n :: Nat) c k v b.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, Monoid b, KnownNat n) => (v -> b) -> BitMap n c k v -> b
foldMap' f = ifoldMap' (const f)

{-# INLINE bitMapping #-}
bitMapping ::
    forall (n :: Nat) c k v. (Enum k, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => IndexedFold k (BitMap n c k v) v
bitMapping = ifoldring ifoldr

{-# INLINE bitMapOf #-}
bitMapOf ::
    forall (n :: Nat) c k v s.
    (Enum k, VU.IsoUnbox v c, VU.Unbox c, Semigroup v, Num c, Monoid v, KnownNat n) =>
    IndexedFold k s v
    -> s
    -> BitMap n c k v
bitMapOf l s = BitMap $ runST do
    b <- MVSU.new
    b' <- ifoldlMOf l write b s
    VSU.unsafeFreeze b'
  where
    write k b v = MVSU.unsafeWrite b (fromEnum k) (toURepr v) $> b

{-# INLINE toList #-}
toList :: forall (n :: Nat) c k v. (Enum k, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => BitMap n c k v -> [(k, v)]
toList bm = Exts.build (\k z -> ifoldr (curry k) z bm)

{-# INLINE fromList #-}
fromList ::
    forall (n :: Nat) c k v. (Enum k, Monoid v, VU.IsoUnbox v c, VU.Unbox c, KnownNat n) => [(k, v)] -> BitMap n c k v
fromList = insertAllWith (<>) empty

{-# INLINE keys #-}
keys ::
    forall (n :: Nat) c k v.
    (Enum k, Enum c, VU.IsoUnbox v c, VU.Unbox c, KnownNat n, Num c, Bits c, Eq v, Monoid v) =>
    BitMap n c k v -> BS.BitSet c k
keys = BS.bitSetOf (bitMapping . filtered (/= mempty) . asIndex)

{-# INLINE null #-}
null :: forall (n :: Nat) c k v. (VU.Unbox c, Eq v, Monoid v, VU.IsoUnbox v c) => BitMap n c k v -> Bool
null (BitMap bm) = VSU.all ((== mempty) . fromURepr @v @c) bm

{-# INLINE filter #-}
filter ::
    forall (n :: Nat) c k v.
    (VU.IsoUnbox v c, VU.Unbox c, KnownNat n, Monoid v) =>
    (v -> Bool) -> BitMap n c k v -> BitMap n c k v
filter p (BitMap bm) = BitMap $! VSU.map (\c -> if p (fromURepr c) then c else toURepr @v mempty) bm

type instance IxValue (BitMap n c k v) = v

type instance Index (BitMap n c k v) = k

instance (Enum k, VU.IsoUnbox v c, VU.Unbox c, Monoid v, KnownNat n) => Ixed (BitMap n c k v) where
    ix k = lens (get k) (flip (insertWith (<>) k))

instance VU.IsoUnbox (Sum Word16) Word16 where
    toURepr = coerce
    fromURepr = coerce
