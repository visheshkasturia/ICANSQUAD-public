module Quad where

import Data.Monoid (Sum (Sum, getSum))
import Vectors

data Box = Rect Point Point deriving (Show, Eq)

data QuadTree e
  = E -- empty tree
  | Leaf
      { box :: Box,
        loc :: Point,
        val :: e
      }
  | N -- non-empty tree, with...
      { box :: Box,
        summary :: e,
        topLeft :: QuadTree e,
        topRight :: QuadTree e,
        bottomLeft :: QuadTree e,
        bottomRight :: QuadTree e
      }
  deriving (Show, Eq)

data Q e = QuadTree
  { tree :: QuadTree e,
    bb :: Box
  }
  deriving (Show, Eq)

instance Foldable QuadTree where
  foldr :: (a -> b -> b) -> b -> QuadTree a -> b
  foldr f b E = b
  foldr f b (Leaf _ _ v) = f v b
  foldr f b (N _ v tL tR bL bR) = f v (foldr f (foldr f (foldr f (foldr f b tL) tR) bL) bR)

-- A way to initialize a quadtree given boundary of 2d space of interest
emptyQ :: Box -> Q e
emptyQ = QuadTree E

{-
===========
  -- Helper functions to extract info from quadtree and bounding boxes
===========
-}

isEmptyQ :: Q e -> Bool
isEmptyQ (QuadTree t _) = isEmptyQuad t

isEmptyQuad :: QuadTree e -> Bool
isEmptyQuad E = True
isEmptyQuad _ = False

divideBox :: Box -> Point -> (Box, Box, Box, Box)
divideBox (Rect (xL, yL) (xT, yT)) (xM, yM) =
  ( Rect (xL, yM) (xM, yT),
    Rect (xM, yM) (xT, yT),
    Rect (xL, yL) (xM, yM),
    Rect (xM, yL) (xT, yM)
  )

midpoint :: Box -> Point
midpoint (Rect (xL, yL) (xT, yT)) = ((xL + xT) / 2, (yL + yT) / 2)

width :: Box -> Double
width (Rect (xL, _) (xT, _)) = xT - xL

height :: Box -> Double
height (Rect (_, yL) (_, yT)) = yT - yL

widthQuad :: QuadTree e -> Double
widthQuad E = 0
widthQuad (Leaf b _ _) = width b
widthQuad (N b _ _ _ _ _) = width b

heightQuad :: QuadTree e -> Double
heightQuad E = 0
heightQuad (Leaf b _ _) = height b
heightQuad (N b _ _ _ _ _) = height b

value :: Monoid e => QuadTree e -> e
value E = mempty
value (Leaf _ _ v) = v
value (N _ s _ _ _ _) = s

-- Close version of foldr except it takes a quadtree instead of just the value in the quadtree
-- to provide and extract more spatial information
foldTreeHelper :: (QuadTree e -> t -> t) -> t -> QuadTree e -> t
foldTreeHelper f b = aux b
  where
    aux b n@(N _ _ tL tR bL bR) = aux (aux (aux (aux (f n b) tL) tR) bL) bR
    aux b t = f t b

-- Uses foldable instance to get number of nodes in tree
countNodes :: Q e -> Int
countNodes (QuadTree tree _) = getSum (foldMap (Sum . const 1) tree)

countLeafNodes :: Q e -> Int
countLeafNodes (QuadTree tree _) = foldTreeHelper countNodes' 0 tree
  where
    countNodes' Leaf {} = (+ 1)
    countNodes' _ = id

getAllBoxes :: Q e -> [Box]
getAllBoxes (QuadTree tree _) = foldTreeHelper getBoxes' [] tree
  where
    getBoxes' E = id
    getBoxes' (Leaf b _ _) = (b :)
    getBoxes' (N b _ _ _ _ _) = (b :)

getLeafBoxes :: Q e -> [Box]
getLeafBoxes (QuadTree tree _) = foldTreeHelper getBoxes' [] tree
  where
    getBoxes' (Leaf b _ _) = (b :)
    getBoxes' _ = id

getLeafValues :: Monoid e => Q e -> [e]
getLeafValues (QuadTree tree _) = foldTreeHelper getLeafValues' [] tree
  where
    getLeafValues' (Leaf _ _ v) = (v :)
    getLeafValues' _ = id

-- Helps determine number of integer locations between 2 real locations in a tree
countIntsBetween :: Double -> Double -> Int
countIntsBetween a b
  | b < a = countIntsBetween b a
  | otherwise = floor b - floor a

-- Finds number of integer locations in a box from the tree
numPointsInBox :: Box -> Int
numPointsInBox (Rect (xL, yL) (xT, yT)) = countIntsBetween xL xT * countIntsBetween yL yT

pointInBox :: Box -> Point -> Bool
pointInBox (Rect (xL, yL) (xT, yT)) (x, y) = (xL <= x && x <= xT) && (yL <= y && y <= yT)

pointInTreeBoundary :: Q e -> Point -> Bool
pointInTreeBoundary (QuadTree _ box) = pointInBox box

constructTree :: Monoid e => Box -> QuadTree e -> QuadTree e -> QuadTree e -> QuadTree e -> QuadTree e
constructTree b tL tR bL bR =
  N b (value tL <> value tR <> value bL <> value bR) tL tR bL bR

{-
===========
  -- Main functions of library
===========
-}

-- Inserts a value at a certain point in a quadtree - potentially failing
-- Note: Monoid mappend needs to be commutative
insert :: Monoid e => Q e -> Point -> e -> Maybe (Q e)
insert q@(QuadTree tree bb) (x, y) e
  | not (pointInTreeBoundary q (x, y)) = Nothing
  | otherwise = Just (QuadTree (insert' tree bb) bb)
  where
    insert' E r = Leaf r (x, y) e -- r needed here
    insert' (Leaf b p v) r = if (x, y) == p then Leaf b p e else insert' (splitLeaf b p v) r
    insert' (N b _ tL tR bL bR) r
      | x <= midpointX && y <= midpointY = let n = insert' bL bL' in constructTree b tL tR n bR
      | x <= midpointX && y > midpointY = let n = insert' tL tL' in constructTree b n tR bL bR
      | x > midpointX && y <= midpointY = let n = insert' bR bR' in constructTree b tL tR bL n
      | otherwise = let n = insert' tR tR' in constructTree b tL n bL bR
      where
        (midpointX, midpointY) = midpoint b
        (tL', tR', bL', bR') = divideBox b (midpointX, midpointY)

    splitLeaf b p@(x', y') v
      | x' <= midpointX && y' <= midpointY = N b v E E (Leaf bL p v) E -- bL
      | x' <= midpointX && y' > midpointY = N b v (Leaf tL p v) E E E -- tL
      | x' > midpointX && y' <= midpointY = N b v E E E (Leaf bR p v) -- bR
      | otherwise = N b v E (Leaf tR p v) E E -- tR
      where
        (midpointX, midpointY) = midpoint b
        (tL, tR, bL, bR) = divideBox b (midpointX, midpointY)

-- Builds a quadtree given the width and height of 2d space plus function to
-- query value at integer locations of that space
-- This function will iterate over all integer locations from (0, 0) to (w, h)
-- The function takes the column and row and returns a maybe value
-- Note: Monoid mappend needs to be commutative
buildTree :: Monoid e => Int -> Int -> (Int -> Int -> Maybe e) -> Q e
buildTree w h f = QuadTree (buildTree' fullBox) fullBox
  where
    fullBox = Rect (0, 0) (fromIntegral w, fromIntegral h)

    buildTree' box@(Rect (xL, yL) (xT, yT))
      | numPointsInBox box == 1 = case f (floor xT - 1) (h - floor yT) of
        Nothing -> E
        Just v -> Leaf box (fromIntegral (floor xT), fromIntegral (floor yT)) v
      | numPointsInBox box == 0 = E
      | otherwise =
        let (a', b', c', d') = divideBox box (midpoint box)
         in let (a'', b'', c'', d'') = (buildTree' a', buildTree' b', buildTree' c', buildTree' d')
             in constructTree box a'' b'' c'' d''

-- Returns value of (x, y) from quadtree
getValue :: Q e -> Point -> Maybe e
getValue q@(QuadTree tree bb) p@(x, y)
  | not (pointInTreeBoundary q p) = Nothing
  | otherwise = getValue' tree
  where
    getValue' E = Nothing
    getValue' (Leaf _ p' e) = if p' == p then Just e else Nothing
    getValue' (N b s tL tR bL bR)
      | x <= midpointX && y <= midpointY = getValue' bL
      | x <= midpointX && y > midpointY = getValue' tL
      | x > midpointX && y <= midpointY = getValue' bR
      | otherwise = getValue' tR
      where
        (midpointX, midpointY) = midpoint b

compressTree :: (Monoid e) => Q e -> (e -> e -> Bool) -> Q e
compressTree (QuadTree tree b) pred = QuadTree (compressTree' tree) b
  where
    compressTree' E = E
    compressTree' l@Leaf {} = l
    compressTree' (N b s tL tR bL bR) =
      let (tL', tR', bL', bR') = (compressTree' tL, compressTree' tR, compressTree' bL, compressTree' bR)
       in if all isLeaf [tL', tR', bL', bR'] && isEqual tL' tR' bL' bR'
            then Leaf b (0, 0) (value tL' <> value tR' <> value bL' <> value bR')
            else N b s tL' tR' bL' bR'

    isLeaf N {} = False
    isLeaf _ = True

    isEqual a b c d = eq a b && eq a c && eq a d && eq b c && eq b d && eq c d -- check every pair with predicate
    eq E _ = True
    eq _ E = True
    eq t1 t2 = pred (value t1) (value t2)
