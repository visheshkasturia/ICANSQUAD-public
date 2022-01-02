{-# LANGUAGE TemplateHaskell #-}

module ImageTest (main) where

import Data.AEq
import Image (Pixel (..), PixelValue (..), deconstructTree, overlay)
import Quad (Box (..), Q (..), QuadTree (..), buildTree, compressTree)
import Test.QuickCheck
import TestTypes (MInt, Matrix (..))

instance Arbitrary Pixel where
  arbitrary :: Gen Pixel
  arbitrary = do
    r <- choose (0.0, 255)
    g <- choose (0.0, 255)
    b <- choose (0.0, 255)
    P (Val r g b) . getPositive <$> (arbitrary :: Gen (Positive Int))

  shrink :: Pixel -> [Pixel]
  shrink (P v 0) = []
  shrink (P v n) = [P v (n - 1)]

width :: [[a]] -> Int
width m = case m of
  [] -> 0
  (x : xs) -> length x

-- Helper function to index into a 2d array/Matrix so a tree can be built
index :: Int -> Int -> [[e]] -> Int -> Int -> Maybe e
index width height m col row =
  if row >= 0 && row < height && col >= 0 && col < width
    then rowInd row col m
    else Nothing
  where
    rowInd 0 col (x : xs) = colInd col x
    rowInd row col (x : xs) = rowInd (row - 1) col xs
    rowInd _ _ _ = Nothing

    colInd 0 (x : xs) = Just x
    colInd col (x : xs) = colInd (col - 1) xs
    colInd _ _ = Nothing

-- Building a tree and deconstructing the tree should return the original 2d array
prop_buildDeconstructRoundTrip :: Matrix MInt -> Bool
prop_buildDeconstructRoundTrip (Matrix m) =
  deconstructTree False tree mempty == m
  where
    tree = buildTree w h (index w h m)
    w = width m
    h = length m

-- Building a tree and compressing with equality which is lossless, then
-- deconstructing the tree should return the original image
prop_buildCompressDeconstructRoundTrip :: Matrix Pixel -> Bool
prop_buildCompressDeconstructRoundTrip (Matrix m) =
  deconstructTree False tree mempty == m
  where
    tree = compressTree (buildTree w h (index w h m)) (==)
    w = width m
    h = length m

-- The dimensions of a overlaid image after building and compressing should be
-- the same as the original image
prop_buildCompressOverlayRoundTrip :: Matrix Pixel -> Bool
prop_buildCompressOverlayRoundTrip (Matrix m) = w == sw && h == sh
  where
    tree = compressTree (buildTree w h (index w h m)) (==)
    segmented = overlay tree mempty (flip (index w h m))
    w = width m
    h = length m

    sw = width segmented
    sh = length segmented

-- Semigroup and Monoid laws are held by the Pixel monoid
prop_semigroup :: Pixel -> Pixel -> Pixel -> Bool
prop_semigroup a b c = (a <> b) <> c ~== a <> (b <> c)

prop_memptyLeft :: Pixel -> Bool
prop_memptyLeft p = mempty <> p ~== p

prop_memptyRight :: Pixel -> Bool
prop_memptyRight p = p <> mempty ~== p

-- Semigroup and Monoid laws are held by the MInt monoid
prop_semigroupMInt :: MInt -> MInt -> MInt -> Bool
prop_semigroupMInt a b c = (a <> b) <> c ~== a <> (b <> c)

prop_memptyLeftMInt :: MInt -> Bool
prop_memptyLeftMInt p = mempty <> p ~== p

prop_memptyRightMInt :: MInt -> Bool
prop_memptyRightMInt p = p <> mempty ~== p

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  _ <- runTests
  return ()