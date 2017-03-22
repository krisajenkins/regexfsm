module Main where

import Prelude
import Data.Map as Map
import Data.Set as Set
import Data.Foldable (class Foldable, foldl)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))

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
  show (Leaf a) = "(Leaf " <> show a <> ")"

type Regex = Map (Node Int) (Map Char (Set (Node Int)))

match :: Regex -> String -> Boolean
match regex string =
  Set.member End lastHeads
  where
    lastHeads :: Set (Node Int)
    lastHeads = foldl (stepHeads regex) (Set.singleton Start) (toCharArray string)

-- | Given the current character in the string we're matching, stepHeads
-- | each head (each pointer into the graph) forward.
stepHeads :: Regex -> Set (Node Int) -> Char -> Set (Node Int)
stepHeads regex heads current =
  foldl reducer Set.empty heads
  where
    reducer :: Set (Node Int) -> (Node Int) -> Set (Node Int)
    reducer accum head = Set.union accum $ stepHead regex current head


stepHead :: Regex -> Char -> Node Int -> Set (Node Int)
stepHead regex current head = withDefault Set.empty $ do
  links <- lookup head regex
  lookup current links

withDefault :: forall a. a -> Maybe a -> a
withDefault x = maybe x id

------------------------------------------------------------
-- Sample regex state machines.

-- | a*b
regex1 :: Regex
regex1 =
  Map.fromFoldable
  [ Start -=> [ 'a' =-> [ Start ]
              , 'b' =-> [ End ] ]
  ]


-- | (ab|ac)+
regex2 :: Regex
regex2 =
  Map.fromFoldable
  [ Start -=> [ 'a' =-> [ Leaf 1, Leaf 2 ] ]
  , Leaf 1 -=> [ 'b' =-> [ Start, End ] ]
  , Leaf 2 -=> [ 'c' =-> [ Start, End ] ]
  ]

------------------------------------------------------------
-- Helpers for constructing graphs by-hand.

nodeToEdge :: forall h t c f.
  (Ord c , Foldable f) => h -> f (Tuple c t) -> Tuple h (Map c t)
nodeToEdge head tail = Tuple head (Map.fromFoldable tail)

edgeToNodes :: forall h t f.
  (Foldable f, Ord t) => h -> f t -> Tuple h (Set t)
edgeToNodes head tail = Tuple head (Set.fromFoldable tail)

infixl 9 nodeToEdge as -=>
infixl 9 edgeToNodes as =->
