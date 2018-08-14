module Test.Main where

import Prelude

import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Diff (diff)

main :: Effect Unit
main = do
  let m1 = Map.fromFoldable [Tuple "a" 100, Tuple "b" 300, Tuple "e" 500]
  let m2 = Map.fromFoldable [Tuple "a" 200, Tuple "d" 400, Tuple "e" 500]

  let a = [1,2,3]
  let b = [1,5,6]

  logShow $ diff m1 m2
  logShow $ diff a b
