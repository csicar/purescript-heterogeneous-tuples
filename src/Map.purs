module Map where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple.Nested (type (/\), (/\))

class MapTuples mapper tupleInput tupleOutput | mapper tupleInput -> tupleOutput, mapper tupleOutput -> tupleInput where
  mapTuples :: mapper -> tupleInput -> tupleOutput
  

class Mapper mapper input out | mapper input -> out, mapper out -> input where
  mapValues :: mapper -> input -> out

instance mapNil :: MapTuples mapper Unit Unit where
  mapTuples _ _ = unit
else
instance mapCons :: 
  ( MapTuples mapper tail tailOut
  , Mapper mapper headIn headOut
  ) 
  => MapTuples mapper (headIn /\ tail) (headOut /\tailOut) where
  mapTuples mapper (headIn /\ tailIn) = (mapValues mapper headIn) /\ mapTuples mapper tailIn