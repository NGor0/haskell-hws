{-# LANGUAGE InstanceSigs #-}
module Block3
  ( TST(..)
  , seek
  , add
  , kmin

  , N(..)
  , natSum
  , natMult
  , superNat

  -- helpers
  , fromList
  , toList
  ) where

import Control.Applicative ( (<|>) )

-- Ternary Search Tree
data TST a
  = Branch a (TST a) (TST a) (TST a)
  | Leaf
  deriving (Eq, Show)

-- Foldable is needed to convert TST
-- to list with O(n) time complexity
instance Foldable TST where
  foldr :: (a -> b -> b) -> b -> TST a -> b
  foldr _ z Leaf = z
  foldr f z (Branch value left central right) =
    foldr f (foldr f (foldr f (foldr f z right) [value]) central) left

instance Functor TST where
  fmap :: (a -> b) -> TST a -> TST b
  fmap _ Leaf = Leaf
  fmap f (Branch value left central right) =
    Branch (f value) (fmap f left) (fmap f central) (fmap f right)

-- return 'Just value' if TST contains value,
-- otherwise return 'Nothing'
seek :: Ord a => a -> TST a -> Maybe a
seek value Leaf = Nothing
seek value (Branch nodeValue left central right)
  | value == nodeValue = return value
  | value > nodeValue = seek value right
  | value < nodeValue = seek value left <|> seek value central

-- add value to TST and return new TST
-- if the value was not contained in it
-- otherwise return old TST
add :: Ord a => a -> TST a -> TST a
add value tst = go value tst 0
  where
    go value Leaf _ = Branch value Leaf Leaf Leaf
    go value tst@(Branch nodeValue left central right) counter
      | value == nodeValue = tst
      | value > nodeValue = Branch nodeValue left central
                              (go value right $ succ counter)
      | value < nodeValue =
        if even counter
        then Branch nodeValue (go value left $ succ counter) central right
        else Branch nodeValue left (go value central $ succ counter) right

fromList :: Ord a => [a] -> TST a
fromList = foldl (flip add) Leaf

toList :: Ord a => TST a -> [a]
toList = foldr (:) mempty

-- return first k minimal values
-- contained in TST
kmin :: Ord a => Int -> TST a -> [a]
kmin k = take k . toList


-- Natural number
data N = Z | S N deriving Show

instance Eq N where
  (==) :: N -> N -> Bool
  Z == Z = True
  Z == _ = False
  _ == Z = False
  S n == S m = n == m

instance Ord N where
  compare :: N -> N -> Ordering
  compare Z Z = EQ
  compare _ Z = GT
  compare Z _ = LT
  compare (S n) (S m) = compare n m

-- add 2 natural numbers
natSum :: N -> N -> N
natSum n Z = n
natSum Z n = n
natSum (S n) m = S $ natSum n m

-- multiply 2 natural numbers
natMult :: N -> N -> N
natMult _ Z = Z
natMult Z _ = Z
natMult (S n) m = natSum m $ natMult n m

-- raise 1st natural number
-- to the power of 2nd natural number
natExp :: N -> N -> N
natExp _ Z = S Z
natExp n (S Z) = n
natExp n (S m) = natMult n $ natExp n m

-- hyperoperator for natural numbers,
-- where the first argument means:
-- Z - addition
-- S Z - multiplication
-- S (S Z) - exponentiation
superNat :: N -> N -> N -> N
superNat Z = natSum
superNat (S Z) = natMult
superNat (S (S Z)) = natExp
superNat _ = error
  "superNat only supports addition (Z), multiplication (S Z) and exponentiation (S (S Z))"

-- Done: 9/12