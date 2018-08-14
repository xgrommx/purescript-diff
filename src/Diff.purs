module Diff where

import Prelude

import Control.Apply (lift2)
import Data.Align (class Align, padZipWith)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

type DeltaUnit a = { old :: a, new :: a }

type Rec k a = { key :: k, oldVal :: (Maybe a), newVal :: (Maybe a) }

data Delta a
  = Delta (DeltaUnit a)
  | Same a
  | Old  a
  | New  a

derive instance eqDelta :: Eq a => Eq (Delta a)
derive instance ordDelta :: Ord a => Ord (Delta a)
derive instance functorDelta :: Functor Delta
derive instance genericDelta :: Generic (Delta a) _

instance foldableDelta :: Foldable Delta where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = case _ of
    Delta r -> f r.old <> f r.new
    Same a -> f a
    Old a -> f a
    New a -> f a

instance traverseDelta :: Traversable Delta where
  sequence = sequenceDefault
  traverse f = case _ of
    Delta r -> lift2 (\o n -> Delta { old: o, new: n }) (f r.old) (f r.new)
    Same a -> Same <$> f a
    Old a -> Old <$> f a
    New a -> New <$> f a

instance showDelta :: Show a => Show (Delta a) where
  show d = genericShow d

data M = M1 | M2

derive instance genericM :: Generic M _

instance showM :: Show M where
  show m = genericShow m

isSame :: forall a. Eq a => Delta a -> Boolean
isSame (Same _) = true
isSame (Delta {old, new}) = if old == new then true else false
isSame _ = false

isOld :: forall a. Delta a -> Boolean
isOld (Old _) = true
isOld (Delta {old: _, new: _}) = true
isOld _ = false

isNew :: forall a. Delta a -> Boolean
isNew (New _ ) = true
isNew (Delta {old: _, new: _}) = true
isNew _ = false

isDelta :: forall a. Delta a -> Boolean
isDelta (Delta _) = true
isDelta _ = false

getSame :: forall a. Eq a => Delta a -> Maybe a
getSame (Same a) = Just a
getSame (Delta {old, new}) = if old == new then Just old else Nothing
getSame _ = Nothing

getOld :: forall a. Delta a -> Maybe a
getOld (Delta r) = Just r.old
getOld (Old a) = Just a
getOld _ = Nothing

getNew :: forall a. Delta a -> Maybe a
getNew (Delta r) = Just r.new
getNew (New a) = Just a
getNew _ = Nothing

getDelta :: forall a. Delta a -> Maybe (DeltaUnit a)
getDelta (Delta d) = Just d
getDelta _ = Nothing

getOriginal :: forall a. M -> Delta a -> Maybe a
getOriginal M1 (Delta r) = Just r.old
getOriginal M2 (Delta r) = Just r.new
getOriginal _  (Same x) = Just x
getOriginal M1 (Old x) = Just x
getOriginal _  (Old _) = Nothing
getOriginal M2 (New x) = Just x
getOriginal _  (New _) = Nothing

getOriginals :: forall a. Delta a -> Tuple (Maybe a) (Maybe a)
getOriginals (Delta {old, new}) = Tuple (Just old) (Just new)
getOriginals (Same x) = Tuple (Just x) (Just x)
getOriginals (Old x) = Tuple (Just x) (Nothing)
getOriginals (New x) = Tuple Nothing (Just x)

getRecord :: forall k a. k -> Delta a -> Rec k a
getRecord k (Delta r) = { key: k, oldVal: Just r.old, newVal: Just r.new }
getRecord k (Same x) = { key: k, oldVal: Just x, newVal: Just x }
getRecord k (Old x) = { key: k, oldVal: Just x, newVal: Nothing }
getRecord k (New x) = { key: k, oldVal: Nothing, newVal: Just x }

getRecordWith :: forall k a b. (a -> b) -> k -> Delta a -> Rec k b
getRecordWith f k (Delta {old, new}) =
  let o = Just $ f old
      n = Just $ f new
  in { key: k, oldVal: o, newVal: n }
getRecordWith f k (Same x) =
  let o = Just $ f x
      n = Just $ f x
  in { key: k, oldVal: o, newVal: n }
getRecordWith f k (Old x) =
  let o = Just $ f x
      n = Nothing
  in { key: k, oldVal: o, newVal: n }
getRecordWith f k (New x) =
  let o = Nothing
      n = Just $ f x
  in { key: k, oldVal: o, newVal: n }

toSame :: forall a f. Functor f => Filterable f => Eq a => f (Delta a) -> f a
toSame = filterMap getSame

toOld :: forall a f. Functor f => Filterable f => Eq a => f (Delta a) -> f a
toOld = filterMap getOld

toNew :: forall a f. Functor f => Filterable f => Eq a => f (Delta a) -> f a
toNew = filterMap getNew

toDelta :: forall a f. Functor f => Filterable f => Eq a => f (Delta a) -> f (DeltaUnit a)
toDelta = filterMap getDelta

toOriginal :: forall a f. Functor f => Filterable f => Eq a => M -> f (Delta a) -> f a
toOriginal m = filterMap (getOriginal m)

toOriginals :: forall a f. Functor f => Filterable f => Eq a => f (Delta a) -> Tuple (f a) (f a)
toOriginals m = Tuple (toOriginal M1 m) (toOriginal M2 m)

mapSame :: forall f a b. Functor f => Filterable f => Eq a => (a -> b) -> f (Delta a) -> f b
mapSame f = filterMap (map f <<< getSame)

mapOld :: forall f a b. Functor f => Filterable f => Eq a => (a -> b) -> f (Delta a) -> f b
mapOld f = filterMap (map f <<< getOld)

mapNew :: forall f a b. Functor f => Filterable f => Eq a => (a -> b) -> f (Delta a) -> f b
mapNew f = filterMap (map f <<< getNew)

mapSame' :: forall f a. Functor f => Filterable f => Eq a => (a -> a) -> f (Delta a) -> f (Delta a)
mapSame' f = map (\x -> if isSame x then map f x else x)

mapOld' :: forall f a. Functor f => Filterable f => Eq a => (a -> a) -> f (Delta a) -> f (Delta a)
mapOld' f = map $ case _ of
  Old x -> Old (f x)
  Delta r -> Delta r {old = f r.old}
  x -> x

mapNew' :: forall f a. Functor f => Filterable f => Eq a => (a -> a) -> f (Delta a) -> f (Delta a)
mapNew' f = map $ case _ of
  New x -> New (f x)
  Delta r -> Delta r {new = f r.new}
  x -> x

toRecord :: forall f a i. FunctorWithIndex i f => f (Delta a) -> f (Rec i a)
toRecord = mapWithIndex getRecord

toRecordWith :: forall f a b i. FunctorWithIndex i f => (a -> b) -> f (Delta a) -> f (Rec i b)
toRecordWith f = mapWithIndex (getRecordWith f)

diff :: forall f a. Align f => Eq a => f a -> f a -> f (Delta a)
diff m1 m2 = padZipWith (\x y -> unsafePartial $ diff' x y) m1 m2
  where
    diff' :: Partial => Eq a => Maybe a -> Maybe a -> Delta a 
    diff' = case _, _ of
      Just x, Just y 
        | x == y -> Same x
        | otherwise -> Delta ({old: x, new: y})
      Nothing, Just y -> New y
      Just x, Nothing -> Old x