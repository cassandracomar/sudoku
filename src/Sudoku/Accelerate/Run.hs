{-# LANGUAGE CPP #-}
#ifdef PRELINK
{-# LANGUAGE TemplateHaskell #-}
#endif

module Sudoku.Accelerate.Run where

import Data.Array.Accelerate as A
#ifndef PRELINK
import Data.Array.Accelerate.LLVM.Native (run1)
#else
import Data.Array.Accelerate.LLVM.Native (runQ)
#endif
import Sudoku.Accelerate.Summaries
import Sudoku.Cell (Cell, Digit)

fullSimplifyStepKnowns :: A.Vector (Cell Digit) -> RawSimplifierRes Digit
#ifndef PRELINK
fullSimplifyStepKnowns = run1 accFullSimplifyStepKnowns
#else
fullSimplifyStepKnowns = $(runQ accFullSimplifyStepKnowns)
#endif
{-# NOINLINE fullSimplifyStepKnowns #-}

fullSimplifyStepNakedSingles :: A.Vector (Cell Digit) -> RawSimplifierRes Digit
#ifndef PRELINK
fullSimplifyStepNakedSingles = run1 accFullSimplifyStepNakedSingles
#else
fullSimplifyStepNakedSingles = $(runQ accFullSimplifyStepNakedSingles)
#endif
{-# NOINLINE fullSimplifyStepNakedSingles #-}

fullSimplifyStepHiddenSingles :: A.Vector (Cell Digit) -> RawSimplifierRes Digit
#ifndef PRELINK
fullSimplifyStepHiddenSingles = run1 accFullSimplifyStepHiddenSingles
#else
fullSimplifyStepHiddenSingles = $(runQ accFullSimplifyStepHiddenSingles)
#endif
{-# NOINLINE fullSimplifyStepHiddenSingles #-}
