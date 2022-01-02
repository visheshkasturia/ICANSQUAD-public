{-# LANGUAGE TemplateHaskell #-}

module QuadTest (main) where

import Data.AEq
import Quad
import Test.HUnit
import Test.QuickCheck
import TestTypes (MInt, Matrix (..))
import Vectors

instance (Arbitrary e, Monoid e) => Arbitrary (Q e) where
  arbitrary :: Gen (Q e)
  arbitrary = do
    lowerXBound <- (arbitrary :: Gen Double)
    lowerYBound <- (arbitrary :: Gen Double)
    width <- (arbitrary :: Gen (Positive Double))
    height <- (arbitrary :: Gen (Positive Double))
    pointList <-
      listOf
        ( (,)
            <$> ( (,)
                    <$> choose (lowerXBound, lowerXBound + getPositive width)
                    <*> choose (lowerYBound, lowerYBound + getPositive height)
                )
            <*> (arbitrary :: Gen e)
        )
    let eQ = emptyQ (Rect (lowerXBound, lowerYBound) (lowerXBound + getPositive width, lowerYBound + getPositive height))
     in let mTree =
              foldr
                ( \((x, y), e) acc -> case acc of
                    Nothing -> Nothing
                    Just q -> insert q (x, y) e
                )
                (Just eQ)
                pointList
         in case mTree of
              Nothing -> return (emptyQ (Rect (0, 0) (100, 100)))
              Just tree -> return tree

  shrink :: Q e -> [Q e]
  shrink (QuadTree tree _) = shrink' tree
    where
      shrink' E = []
      shrink' (Leaf b _ _) = [QuadTree E b]
      shrink' (N _ _ tL tR bL bR) = [makeQ tL, makeQ tR, makeQ bL, makeQ bR]

      makeQ t = QuadTree t (getBox t)

      getBox E = Rect (0, 0) (0, 0)
      getBox x = box x

twoQuad :: Q MInt
twoQuad =
  QuadTree
    ( N
        (Rect (0, 0) (2, 2))
        10
        (Leaf (Rect (0, 1) (1, 2)) (1, 2) 1)
        (Leaf (Rect (1, 1) (2, 2)) (2, 2) 2)
        (Leaf (Rect (0, 0) (1, 1)) (1, 1) 3)
        (Leaf (Rect (1, 0) (2, 1)) (2, 1) 4)
    )
    (Rect (0, 0) (2, 2))

-- Checks if point is on bottom boundary of its box and entire quadtree's box
touchingBottomBoundary :: Point -> Box -> Box -> Bool
touchingBottomBoundary (x, y) (Rect (xL, yL) (xT, _)) (Rect (xL', yL') (xT', _)) = y == yL && yL == yL' && (xL <= x && x <= xT) && (xL' <= x && x <= xT')

-- Checks if point is on left boundary of its box and entire quadtree's box
touchingLeftBoundary :: Point -> Box -> Box -> Bool
touchingLeftBoundary (x, y) (Rect (xL, yL) (_, yT)) (Rect (xL', yL') (_, yT')) = x == xL && xL == xL' && (yL <= y && y <= yT) && (yL' <= y && y <= yT')

-- top boundary and right boundary inclusive check for point in box
isPointInBox :: Point -> Box -> Bool
isPointInBox (x, y) (Rect (xL, yL) (xT, yT)) = (xL < x && x <= xT) && (yL < y && y <= yT)

-- Checks if all points are within leaf node's bounding box
locationInvariant :: Q e -> Bool
locationInvariant (QuadTree tree box) = foldTreeHelper locInv True tree
  where
    locInv (Leaf b loc _) acc =
      acc
        && (isPointInBox loc b || touchingBottomBoundary loc b box || touchingLeftBoundary loc b box)
    locInv _ acc = acc

locInside :: Q MInt
locInside =
  QuadTree
    (Leaf (Rect (0, 0) (2, 2)) (0, 0) 1)
    (Rect (0, 0) (2, 2))

locOutside :: Q MInt
locOutside =
  QuadTree
    (Leaf (Rect (0, 0) (2, 2)) (-1, -1) 1)
    (Rect (0, 0) (2, 2))

testLocationInvariant :: Test
testLocationInvariant =
  TestList
    [ "leafAtOrigin" ~: locationInvariant locInside ~?= True,
      "leafOutside" ~: locationInvariant locOutside ~?= False
    ]

prop_locationInvariant :: Q MInt -> Bool
prop_locationInvariant = locationInvariant

-- Checks for no parent node with all 4 empty children
existsChildInvariant :: Q e -> Bool
existsChildInvariant (QuadTree tree box) = foldTreeHelper childInv True tree
  where
    childInv (N _ _ tL tR bL bR) acc =
      acc && not (isEmptyQuad tL && isEmptyQuad tR && isEmptyQuad bL && isEmptyQuad bR)
    childInv _ acc = acc

emptyChildren :: Q MInt
emptyChildren =
  QuadTree
    (N (Rect (0, 0) (2, 2)) 0 E E E E)
    (Rect (0, 0) (2, 2))

testChildInvariant :: Test
testChildInvariant =
  TestList
    [ "noEmptyParent" ~: existsChildInvariant twoQuad ~?= True,
      "emptyTree" ~: existsChildInvariant (QuadTree E (Rect (0, 0) (1, 1))) ~?= True,
      "emptyChildren" ~: existsChildInvariant emptyChildren ~?= False
    ]

prop_childrenInvariant :: Q MInt -> Bool
prop_childrenInvariant = existsChildInvariant

appLessThan :: Double -> Double -> Bool
appLessThan x y = x < y || x ~== y

-- Checks if children are contained in the parent
dimensionInvariant :: Q e -> Bool
dimensionInvariant (QuadTree tree box) = foldTreeHelper dimInv True tree
  where
    dimInv n@(N _ _ tL tR bL bR) acc =
      acc && appLessThan (heightQuad tL + heightQuad bL) (heightQuad n)
        && appLessThan (heightQuad tR + heightQuad bR) (heightQuad n)
        && appLessThan (widthQuad tL + widthQuad tR) (widthQuad n)
        && appLessThan (widthQuad bL + widthQuad bR) (widthQuad n)
    dimInv _ acc = acc

childTooBig :: Q MInt
childTooBig =
  QuadTree
    (N (Rect (0, 0) (2, 2)) 10 (Leaf (Rect (0, 1) (2, 4)) (2, 4) 10) E E E)
    (Rect (0, 0) (2, 2))

childrenTooBig :: Q MInt
childrenTooBig =
  QuadTree
    ( N
        (Rect (0, 0) (2, 2))
        10
        (Leaf (Rect (0, 1) (1.5, 2)) (1.5, 2) 5)
        (Leaf (Rect (0.5, 1) (2, 2)) (2, 2) 5)
        E
        E
    )
    (Rect (0, 0) (2, 2))

testDimensionInvariant :: Test
testDimensionInvariant =
  TestList
    [ "twoQuadDimensions" ~: dimensionInvariant twoQuad ~?= True,
      "emptyTree" ~: dimensionInvariant (QuadTree E (Rect (0, 0) (1, 1))) ~?= True,
      "childTooBig" ~: dimensionInvariant childTooBig ~?= False,
      "childrenTooBig" ~: dimensionInvariant childrenTooBig ~?= False
    ]

prop_dimensionInvariant :: Q MInt -> Bool
prop_dimensionInvariant = dimensionInvariant

-- Checks if children are equal quadrants of parent
halfInvariant :: Q e -> Bool
halfInvariant (QuadTree tree _) = foldTreeHelper halfInv True tree
  where
    halfInv n@(N _ _ tL tR bL bR) acc =
      acc && check tL && check tR && check bL && check bR
      where
        halfHeight = heightQuad n / 2
        halfWidth = widthQuad n / 2

        checkHeight t = isEmptyQuad t || heightQuad t ~== halfHeight
        checkWidth t = isEmptyQuad t || widthQuad t ~== halfWidth
        check t = checkHeight t && checkWidth t
    halfInv _ acc = acc

testHalfInvariant :: Test
testHalfInvariant =
  TestList
    [ "twoQuadDimensions" ~: halfInvariant twoQuad ~?= True,
      "emptyTree" ~: halfInvariant (QuadTree E (Rect (0, 0) (1, 1))) ~?= True,
      "childTooBig" ~: halfInvariant childTooBig ~?= False,
      "childrenTooBig" ~: halfInvariant childrenTooBig ~?= False
    ]

prop_halfInvariant :: Q MInt -> Bool
prop_halfInvariant = halfInvariant

-- Checks if children are around the midpoint
midpointInvariant :: Q e -> Bool
midpointInvariant (QuadTree tree _) = foldTreeHelper midpointInv True tree
  where
    midpointInv (N b _ tL tR bL bR) acc =
      acc && cmp tL True True x && cmp tL False False y
        && cmp tR True False x
        && cmp tR False False y
        && cmp bL True True x
        && cmp bL False True y
        && cmp bR True False x
        && cmp bR False True y
      where
        (x, y) = midpoint b

        cmp E = const (const (const True))
        cmp (Leaf b' _ _) = cmp' b'
        cmp (N b' _ _ _ _ _) = cmp' b'
        cmp' (Rect (xL, yL) (xT, yT)) xDim less c
          | xDim && less = appLessThan xT c
          | xDim = appLessThan c xL
          | less = appLessThan yT c
          | otherwise = appLessThan c yL
    midpointInv _ acc = acc

childMiddle :: Q MInt
childMiddle =
  QuadTree
    (N (Rect (0, 0) (2, 2)) 10 (Leaf (Rect (0.5, 1) (1.5, 2)) (1.5, 2) 10) E E E)
    (Rect (0, 0) (2, 2))

childWrongQuadrant :: Q MInt
childWrongQuadrant =
  QuadTree
    (N (Rect (0, 0) (2, 2)) 10 (Leaf (Rect (0, 1) (2, 2)) (2, 2) 10) E E E)
    (Rect (0, 0) (2, 2))

testMidpointInvariant :: Test
testMidpointInvariant =
  TestList
    [ "twoQuadDimensions" ~: midpointInvariant twoQuad ~?= True,
      "emptyTree" ~: midpointInvariant (QuadTree E (Rect (0, 0) (1, 1))) ~?= True,
      "childTooBig" ~: midpointInvariant childTooBig ~?= False,
      "childrenTooBig" ~: midpointInvariant childrenTooBig ~?= False,
      "childMiddle" ~: midpointInvariant childMiddle ~?= False,
      "childWrongQuadrant" ~: midpointInvariant childWrongQuadrant ~?= False
    ]

prop_midpointInvariant :: Q MInt -> Bool
prop_midpointInvariant = midpointInvariant

valid :: Q e -> Bool
valid t =
  locationInvariant t && existsChildInvariant t
    && dimensionInvariant t
    && halfInvariant t
    && midpointInvariant t

prop_Valid :: Q MInt -> Bool
prop_Valid = valid

prop_buildTree :: Matrix MInt -> Int -> Int -> Bool
prop_buildTree (Matrix m) x y =
  valid tree && index (x - 1) (height - y) == getValue tree (fromIntegral x, fromIntegral y)
    && countLeafNodes tree == height * width
  where
    tree = buildTree width height index

    height = length m
    width = case m of
      [] -> 0
      (x : xs) -> length x

    index col row = if row >= 0 && row < height && col >= 0 && col < width then index' row col m else Nothing

    index' 0 col (x : xs) = index'' col x
    index' row col (x : xs) = index' (row - 1) col xs
    index' _ _ _ = Nothing

    index'' 0 (x : xs) = Just x
    index'' col (x : xs) = index'' (col - 1) xs
    index'' _ _ = Nothing

testCountsInBetween :: Test
testCountsInBetween =
  TestList
    [ "01" ~: countIntsBetween 0 1 ~?= 1,
      "half1" ~: countIntsBetween 0.5 1 ~?= 1,
      "closeTo1" ~: countIntsBetween 0.9 1 ~?= 1,
      "negative" ~: countIntsBetween (-1) 1 ~?= 2,
      "negative1.9" ~: countIntsBetween (-1) 1.9 ~?= 2
    ]

-- CountsIntbetween counts the number of integers which should be bounded in a range of 2
prop_countsInBetween :: Double -> Double -> Bool
prop_countsInBetween a b = a' + count - 1 < b' && a' + count + 1 > b'
  where
    count = fromIntegral (countIntsBetween a b)
    a' = min a b
    b' = max a b

testCountNodes :: Test
testCountNodes =
  TestList
    [ "twoQuad" ~: countNodes twoQuad ~?= 5,
      "locInside" ~: countNodes locInside ~?= 1,
      "empty" ~: countNodes (emptyQ (Rect (0, 0) (1, 1))) ~?= 0,
      "emptyChildren" ~: countNodes emptyChildren ~?= 1
    ]

testCountLeafNodes :: Test
testCountLeafNodes =
  TestList
    [ "twoQuad" ~: countLeafNodes twoQuad ~?= 4,
      "locInside" ~: countLeafNodes locInside ~?= 1,
      "empty" ~: countLeafNodes (emptyQ (Rect (0, 0) (1, 1))) ~?= 0,
      "emptyChildren" ~: countLeafNodes emptyChildren ~?= 0
    ]

-- Should not have more leaf nodes than total nodes
prop_countNodes :: Q MInt -> Bool
prop_countNodes q = countNodes q >= countLeafNodes q

emptyChildren' :: Q MInt
emptyChildren' =
  QuadTree
    (N (Rect (0, 0) (2, 2)) 0 E E (Leaf (Rect (0, 0) (1, 1)) (0, 0) 0) E)
    (Rect (0, 0) (2, 2))

emptyInsert :: Q MInt
emptyInsert =
  QuadTree
    (Leaf (Rect (0, 0) (1, 1)) (0, 0) 0)
    (Rect (0, 0) (1, 1))

emptyInsert' :: Q MInt
emptyInsert' =
  QuadTree
    ( N
        (Rect (0, 0) (1, 1))
        1
        (Leaf (Rect (0, 0.5) (0.5, 1)) (0, 1) 1)
        E
        (Leaf (Rect (0, 0) (0.5, 0.5)) (0, 0) 0)
        E
    )
    (Rect (0, 0) (1, 1))

testInsert :: Test
testInsert =
  TestList
    [ "emptyChildren" ~: insert emptyChildren (0, 0) 0 ~?= Just emptyChildren',
      "emptyInsert" ~: insert (emptyQ (Rect (0, 0) (1, 1))) (0, 0) 0 ~?= Just emptyInsert,
      "split" ~: insert emptyInsert (0, 1) 1 ~?= Just emptyInsert'
    ]

-- Insertions at most add one leaf node
prop_insertCount :: Q MInt -> MInt -> Bool
prop_insertCount q@(QuadTree _ b) i =
  case insert q p i of
    Nothing -> False
    Just q' -> if pointExists then countNodes q' == countNodes q else countLeafNodes q' == countLeafNodes q + 1
  where
    p = midpoint b
    pointExists = case getValue q p of
      Nothing -> False
      _ -> True

-- Insertions are order-independent
prop_insertInsert :: Q MInt -> MInt -> MInt -> Bool
prop_insertInsert q@(QuadTree _ b) i i' = qRes == qRes'
  where
    p = midpoint b
    (tL, tR, bL, bR) = divideBox b p
    p' = midpoint tL

    qRes = case insert q p i of
      Nothing -> error "Bug"
      Just q1 -> case insert q1 p' i' of
        Nothing -> error "Bug"
        Just q2 -> q2

    qRes' = case insert q p' i' of
      Nothing -> error "Bug"
      Just q1 -> case insert q1 p i of
        Nothing -> error "Bug"
        Just q2 -> q2

-- Should have same number of nodes as bounding boxes
prop_getAndCountAll :: Q MInt -> Bool
prop_getAndCountAll q = countNodes q == length (getAllBoxes q)

-- Should have same number of leaf nodes as leaf bounding boxes
prop_getAndCountLeaves :: Q MInt -> Bool
prop_getAndCountLeaves q = countLeafNodes q == length (getLeafBoxes q)

-- Leaf boxes should be a subset of all boxes
prop_getAllAndLeaf :: Q MInt -> Bool
prop_getAllAndLeaf q = length allBoxes >= length leafBoxes && length (filter (`elem` leafBoxes) allBoxes) == length leafBoxes
  where
    allBoxes = getAllBoxes q
    leafBoxes = getLeafBoxes q

equalChildren :: Q MInt
equalChildren =
  QuadTree
    ( N
        (Rect (0, 0) (1, 1))
        4
        (Leaf (Rect (0, 0.5) (0.5, 1)) (0, 1) 1)
        (Leaf (Rect (0.5, 0.5) (1, 1)) (1, 1) 1)
        (Leaf (Rect (0, 0) (0.5, 0.5)) (0, 0) 1)
        (Leaf (Rect (0.5, 0) (1, 0.5)) (1, 0.5) 1)
    )
    (Rect (0, 0) (1, 1))

notEqualChildren :: Q MInt
notEqualChildren =
  QuadTree
    ( N
        (Rect (0, 0) (1, 1))
        4
        (Leaf (Rect (0, 0.5) (0.5, 1)) (0, 1) 1)
        (Leaf (Rect (0.5, 0.5) (1, 1)) (1, 1) 1)
        (Leaf (Rect (0, 0) (0.5, 0.5)) (0, 0) 1)
        (Leaf (Rect (0.5, 0) (1, 0.5)) (1, 0.5) 2)
    )
    (Rect (0, 0) (1, 1))

testCompress :: Test
testCompress =
  TestList
    [ "equalChildren" ~: compressTree equalChildren (==)
        ~?= QuadTree
          ( Leaf (Rect (0, 0) (1, 1)) (0, 0) 4
          )
          (Rect (0, 0) (1, 1)),
      "notEqualChildren" ~: compressTree notEqualChildren (==) ~?= notEqualChildren
    ]

-- Compressing any quadtree completely should result in 1 node
prop_compressAll :: Q MInt -> Property
prop_compressAll q = not (isEmptyQ q) ==> countNodes (compressTree q (const (const True))) == 1

return []

runHUnitTests :: IO ()
runHUnitTests = do
  runTestTT $
    TestList
      [ testLocationInvariant,
        testChildInvariant,
        testDimensionInvariant,
        testHalfInvariant,
        testMidpointInvariant,
        testCountsInBetween,
        testCountNodes,
        testCountLeafNodes,
        testInsert,
        testCompress
      ]
  return ()

runQuickCheckTests :: IO Bool
runQuickCheckTests = $quickCheckAll

main :: IO ()
main = do
  runHUnitTests
  _ <- runQuickCheckTests
  return ()