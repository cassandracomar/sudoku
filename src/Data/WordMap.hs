module Data.WordMap where

import Data.Vector.Unboxed qualified as VU

newtype WordMap k v = WordMap {runWordMap :: VU.Vector v}

type role WordMap nominal nominal

extend :: (VU.Unbox v, Monoid v) => VU.Vector v -> VU.Vector v -> VU.Vector v
extend m m' =
    if VU.length m < VU.length m'
        then m VU.++ VU.replicate (VU.length m' - VU.length m) mempty
        else m

instance (Monoid v, VU.Unbox v) => Semigroup (WordMap k v) where
    (WordMap m) <> (WordMap m') = WordMap $ VU.zipWith (<>) (extend m m') (extend m' m)

instance (VU.Unbox v, Monoid v) => Monoid (WordMap k v) where
    mempty = WordMap VU.empty

insert :: (Integral k, VU.Unbox v, Monoid v) => k -> v -> WordMap k v -> WordMap k v
insert k v (WordMap w)
    | fromIntegral k < VU.length w = WordMap $ w VU.// [(fromIntegral k, v)]
    | otherwise = insert k v . WordMap $ w VU.++ VU.replicate (VU.length w) mempty
