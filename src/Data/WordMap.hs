{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.WordMap where

import Control.Lens
import Control.Lens (Index)
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Control.Lens (At (..), Ixed, liftLens)
import Data.Array.Accelerate.Data.HashMap (Hashable (..))
import Data.Array.Accelerate.Data.Hashable (Hashable (..))
import Data.Array.Accelerate.LLVM.Native (run)
import Data.Monoid (Sum (Sum))
import Sudoku.Grid (RegionIndicator)

import Prelude qualified as P hiding (Eq, Integral)

type WordMap sh v = Array sh v

lookup :: (Shape sh, P.Monoid v, Elt v) => Exp sh -> Acc (WordMap sh v) -> Exp v
lookup k vs = vs ! k

-- | `mappend` `v` to the elements at each `k` in `ks`
insert :: (Shape sh, P.Monoid v, Elt v) => Acc (A.Vector sh) -> Exp v -> Acc (WordMap sh v) -> Acc (WordMap sh v)
insert ks v vs = undefined

test :: Array (Z :. Int) (Sum Int)
test = run (A.fold (P.<>) 0 mat)
  where
    vec :: Acc (Array (Z :. Int) (Sum Int))
    vec = A.use (A.fromList (Z :. 5) (P.map Sum [1 ..]))
    mat :: Acc (Array (Z :. Int :. Int) (Sum Int))
    mat = A.replicate (constant $ Z :. (3 :: Int) :. All) vec
