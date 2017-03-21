module Main where

import Data.String as String
import Prelude
import Data.Map as Map
import Data.Set as Set
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe, maybe)
import Data.Set (Set, fromFoldable)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

-- | a*b

data Node a
  = Start
  | Leaf a
  | End

instance nodeOrd :: Ord a => Ord (Node a) where
  compare Start Start = EQ
  compare Start _ = LT
  compare _ Start = GT
  compare (Leaf a) (Leaf b) = compare a b
  compare End End = EQ
  compare _ End = GT
  compare End _ = LT

instance eqNode :: Eq a => Eq (Node a) where
  eq Start Start = true
  eq (Leaf a) (Leaf b) = eq a b
  eq End End = true
  eq _ _ = false

instance showNode :: Show a => Show (Node a) where
  show Start = "Start"
  show End = "End"
  show (Leaf a) = "Leaf " <> show a

type Regex = Map (Node Char) (Set (Node Char))

simpleRegex :: Regex
simpleRegex =
  Map.fromFoldable
  [ Tuple Start (fromFoldable [ Leaf 'a', Leaf 'b' ])
  , Tuple (Leaf 'a') (fromFoldable [ Leaf 'a', Leaf 'b' ])
  , Tuple (Leaf 'b') (fromFoldable [ End ])
  ]

match :: Regex -> String -> Boolean
match regex string =
  Set.member End lastHeads
  where
    lastHeads :: Set (Node Char)
    lastHeads = foldl (step regex) (Set.singleton Start) (toCharArray string)


step :: Regex -> Set (Node Char) -> Char -> Set (Node Char)
step regex heads current =
  foldl reducer Set.empty heads
  where
    reducer :: Set (Node Char) -> (Node Char) -> Set (Node Char)
    reducer accum head = Set.union accum (withDefault Set.empty (singleHead regex current head))

withDefault :: forall a. a -> Maybe a -> a
withDefault x = maybe x id

singleHead :: Regex -> Char -> Node Char -> Maybe (Set (Node Char))
singleHead regex current head =
    lookup (Leaf current) regex
