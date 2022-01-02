module TestTypes
  ( Matrix (..),
    MInt,
  )
where

import Test.QuickCheck

newtype Matrix e = Matrix [[e]] deriving (Show, Eq)

instance (Arbitrary e) => Arbitrary (Matrix e) where
  arbitrary :: Gen (Matrix e)
  arbitrary = do
    width <- (arbitrary :: Gen (Positive Int))
    height <- (arbitrary :: Gen (Positive Int))
    list <- vectorOf (getPositive width * getPositive height) (arbitrary :: Gen e)
    return (Matrix $ convert2D (getPositive width) list [])
    where
      convert2D w [] acc = acc
      convert2D w ls acc = let (front, rest) = splitAt w ls in front : convert2D w rest acc

  shrink :: Matrix e -> [Matrix e]
  shrink (Matrix []) = []
  shrink (Matrix [[]]) = []
  shrink (Matrix [[a]]) = []
  shrink (Matrix [x]) = map (\a -> Matrix [[a]]) x
  shrink (Matrix (x : xs)) = [Matrix [x], Matrix xs]

type MInt = Int

instance Semigroup MInt where
  (<>) = (+)

instance Monoid MInt where
  mempty = 0