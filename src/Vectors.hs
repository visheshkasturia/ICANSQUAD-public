module Vectors where

import Data.AEq

-- Vector is defined as the displacement from origin (0,0) to Point (x, y)
type Vector = (Double, Double)

-- Point is defined as a position vector (x, y) on the 2D plane
type Point = Vector

origin :: Point
origin = (0, 0)

zeroVector :: Vector
zeroVector = (0, 0)

vector1 :: Vector
vector1 = (3, 4)

vectorDirection :: Vector -> Double
vectorDirection v@(vx, vy) =
  if v == zeroVector
    then 0
    else vy / vx

vectorMagnitude :: Vector -> Double
vectorMagnitude (vx, vy) = sqrt (vx ^ 2 + vy ^ 2)

-- Vector from Point P (x1, y1) -> Point Q (x2, y2) is defined as V (x2- x1, y2 -y1)
vectorPQ :: Point -> Point -> Vector
vectorPQ (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- Distance between two points is the maginitude of
distancePQ :: Point -> Point -> Double
distancePQ p1 p2 = vectorMagnitude (vectorPQ p1 p2)

-- Vectors can be scaled by multiplying with a scalar
scaleVector :: Double -> Vector -> Vector
scaleVector c (vx, vy) = (c * vx, c * vy)

{-
The Unit Vector of a vector is defined as the vector having the same direction
and a magnitude of 1.
Unit vector is not defined for zero vector
-}

unitVector :: Vector -> Vector
unitVector v =
  if v == zeroVector
    then zeroVector
    else scaleVector (1 / vectorMagnitude v) v

{-
Since the monoid instance for vectors is ambiguous
in that it can refer to dot-product, cross-product or addition.
We are not defining a monoid instance but instead an addition function
-}

addVectors :: Vector -> Vector -> Vector
addVectors (vx1, vy1) (vx2, vy2) = (vx1 + vx2, vy1 + vy2)

{-
Vectors have the same direction if they have the same slope y/x
To account for the edge case 'Infinity' == 'Infinity'
We have added the OR condition
-}

almostEqualPoints :: Point -> Point -> Bool
almostEqualPoints (x1, y1) (x2, y2) = x1 ~== x2 && y1 ~== y2

almostSameDirection :: Vector -> Vector -> Bool
almostSameDirection v1 v2 = vectorDirection v1 ~== vectorDirection v2 || vectorDirection v1 == vectorDirection v2

almostSameMagnitude :: Vector -> Vector -> Bool
almostSameMagnitude v1 v2 = vectorMagnitude v1 ~== vectorMagnitude v2

almostSameVector :: Vector -> Vector -> Bool
almostSameVector v1 v2 = almostSameDirection v1 v2 && almostSameMagnitude v1 v2

-- That's all we need for the Vectors Module
