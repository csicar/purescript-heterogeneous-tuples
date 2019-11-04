module Apply where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))

class ApplyTuple io i o | io i -> o, o i -> io, io o -> i where
  applyTuple :: io -> i -> o

instance applyTupleUnit :: ApplyTuple Unit Unit Unit where
  applyTuple _ _ = unit

instance applyTupleCons :: ApplyTuple ioTail iTail oTail => ApplyTuple ((a -> b) /\ ioTail) (a /\ iTail) (b /\ oTail) where
  applyTuple (f /\ ioTail) (arg /\ iTail) = f arg /\ applyTuple ioTail iTail
