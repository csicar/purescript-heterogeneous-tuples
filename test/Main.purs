module Test.Main where

import Prelude
import Apply
import Map
import Data.Tuple.Nested
import Data.Tuple

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."


data MapToShow = MapToShow
instance mapToShow :: Show a => Mapper MapToShow a String where
  mapValues _ = show

test :: String /\ String /\ String /\ Unit
test = mapTuples MapToShow (2 /\ 3 /\ "asd" /\ unit)


testApply :: Tuple (Int -> Int) (Tuple (Number -> Number) (Tuple (String -> String) Unit))
testApply = ((+) /\ (*) /\ (<>) /\ unit) `applyTuple` (1 /\ 2.0 /\ "as" /\ unit)

data MapToEmpty = MapToEmpty
instance mapToEmpty :: Mapper MapToEmpty {empty :: a | r} a where
  mapValues _ = _.empty

monoidEmpty :: ∀nested res. MapTuples MapToEmpty nested res => nested -> res
monoidEmpty nested = mapTuples MapToEmpty $ nested

data MapToCombine = MapToCombine
instance mapToCombine :: Mapper MapToCombine {combine :: a | r} a where
  mapValues _ = _.combine

monoidCombine :: ∀nested res. MapTuples MapToCombine nested res => nested -> res
monoidCombine nested = mapTuples MapToCombine nested

testMonoidNested = monoidEmpty $ {empty: ""} /\ unit

testMonoidCombine :: forall t85. Semiring t85 => Tuple (t85 -> t85 -> t85) Unit
testMonoidCombine = monoidCombine $ {combine: (+)} /\ unit

monoidLiftCombine :: forall t81 t82 t83 t85. ApplyTuple t81 t82 t83 => MapTuples MapToCombine t85 t81 => t85 -> t82 -> t83
monoidLiftCombine f a = (monoidCombine f) `applyTuple` a