module Main where

import Data.Foldable (foldl)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude

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

type Regex = Map (Node Int) (Map Char (Set (Node Int)))

-- | a*b
regex1 :: Regex
regex1 =
  Map.fromFoldable
  [ Tuple Start (Map.fromFoldable [ Tuple 'a' (Set.singleton Start), Tuple 'b' (Set.singleton End) ])
  ]

-- | (ab|ac)+
regex2 :: Regex
regex2 =
  Map.fromFoldable
  [ Tuple Start (Map.fromFoldable [ Tuple 'a' (Set.fromFoldable [ Leaf 1, Leaf 2 ]) ])
  , Tuple (Leaf 1) (Map.fromFoldable [ Tuple 'b' (Set.fromFoldable [ Start, End ]) ])
  , Tuple (Leaf 2) (Map.fromFoldable [ Tuple 'c' (Set.fromFoldable [ Start, End ]) ])
  ]

match :: Regex -> String -> Boolean
match regex string =
  Set.member End lastHeads
  where
    lastHeads :: Set (Node Int)
    lastHeads = foldl (step regex) (Set.singleton Start) (toCharArray string)


step :: Regex -> Set (Node Int) -> Char -> Set (Node Int)
step regex heads current =
  foldl reducer Set.empty heads
  where
    reducer :: Set (Node Int) -> (Node Int) -> Set (Node Int)
    reducer accum head = Set.union accum (singleHead regex current head)

withDefault :: forall a. a -> Maybe a -> a
withDefault x = maybe x id

singleHead :: Regex -> Char -> Node Int -> Set (Node Int)
singleHead regex current head = withDefault Set.empty $ do
  links <- lookup head regex
  lookup current links
