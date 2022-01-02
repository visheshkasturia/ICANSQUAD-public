module NBodyQuad where

import NBody
import Quad
import Vectors

-- Calculate Center of Mass of 2 bodies
calcCoM :: Body -> Body -> Point
calcCoM (Body (x1, y1) _ m1) (Body (x2, y2) _ m2) =
  if m1 + m2 == 0
    then (0, 0)
    else ((m1 * x1 + m2 * x2) / (m1 + m2), (m1 * y1 + m2 * y2) / (m1 + m2))

-- Semigroup and Monoid instance of Body for QuadTree
instance Semigroup Body where
  b1@(Body p1 v1 m1) <> b2@(Body p2 v2 m2) = Body (calcCoM b1 b2) (v1 `addVectors` v2) (m1 + m2)

instance Monoid Body where
  mempty = (Body (0, 0) zeroVector 0)

-- Self defined QuadTrees
qtSingletonBody :: Q Body
qtSingletonBody = QuadTree (Leaf (Rect (-5, -5) (5, 5)) (1, 1) (Body (1, 1) (2, 3) 20)) (Rect (-5, -5) (5, 5))

qtSubdivided :: Q Body
qtSubdivided =
  QuadTree
    ( N
        (Rect (-5, -5) (5, 5))
        (Body (1.5, 1.15) (1, 1) 20)
        E
        ( N
            (Rect (0, 0) (5, 5))
            (Body (2.8461538461538463, 2.8461538461538463) (-1, 5) 13)
            E
            (Leaf (Rect (2.5, 2.5) (5, 5)) (4, 4) (Body (4, 4) (1, 3) 8))
            (Leaf (Rect (0, 0) (2.5, 2.5)) (1, 1) (Body (1, 1) (-2, 2) 5))
            E
        )
        (Leaf (Rect (-5, -5) (0, 0)) (-1, -2) (Body (-1, -2) (2, -4) 7))
        E
    )
    (Rect (-5, -5) (5, 5))

qtTwoBodies :: Q Body
qtTwoBodies =
  QuadTree
    ( N
        (Rect (-5, -5) (5, 5))
        (Body (1, -2 / 3) (3, 0) 30)
        E
        (Leaf (Rect (0, 0) (5, 5)) (1, 1) (Body (1, 1) (2, 3) 20))
        E
        (Leaf (Rect (0, -5) (5, 0)) (1, -4) (Body (1, -4) (1, -3) 10))
    )
    (Rect (-5, -5) (5, 5))

-- buildQtBody : Build QuadTree given a List of Body
buildQtBody :: [Body] -> Box -> (Q Body)
buildQtBody bs bb =
  foldr
    ( \b@(Body p u m) q -> case insert q p b of
        Nothing -> q
        Just x -> x
    )
    (emptyQ bb)
    bs

qtPlanets :: Q Body
qtPlanets = buildQtBody planets (Rect (-3700e9, -3700e9) (3700e9, 3700e9))

qtPolarBodies :: Q Body
qtPolarBodies = buildQtBody polarBodies (Rect (-200, -200) (200, 200))

qtBodies1 :: Q Body
qtBodies1 = buildQtBody bodies1 (Rect (-1e7, -1e7) (1e7, 1e7))

qtBodies12 :: Q Body
qtBodies12 = buildQtBody (bodies2 ++ bodies1) (Rect (-1e7, -1e7) (1e7, 1e7))

longerSideBox :: Box -> Double
longerSideBox (Rect (xMin, yMin) (xMax, yMax)) = max (xMax - xMin) (yMax - yMin)

{-
Calculating Acceleration:

Calculating acceleration on a body using QuadTree
If Empty then Force exerted/ acceleration caused is 0
If Leaf then Acceleration is same as acceleration Body Body
If Node then
  case 1: Distance between CoM Node/ longerSide of bounding < thresh
  case 2: Body is in Node
  If either case1 or case 2 is True then we call the function on children
  Else Take Node as representative of Children and call accelerate Body Summary of Node

-}
quadAcceleration :: Double -> Body -> Q Body -> Accel
quadAcceleration thresh b (QuadTree tree _) = qtAcceleration thresh b tree
  where
    qtAcceleration :: Double -> Body -> QuadTree Body -> Accel
    qtAcceleration _ b E = zeroVector
    qtAcceleration _ b (Leaf box l b') = acceleration b b'
    qtAcceleration threshold b@(Body p v m) (N box b' tl tr bl br) =
      if pointInBox box p || distancePQ p (midpoint box) / longerSideBox box < threshold
        then qtAcceleration threshold b tl `addVectors` qtAcceleration threshold b tr `addVectors` qtAcceleration threshold b bl `addVectors` qtAcceleration threshold b br
        else acceleration b b'

moveBodiesQuad :: TimeStep -> TimeStep -> TimeStep -> Q Body -> Q Body
moveBodiesQuad t dT oneS qb@(QuadTree _ box) = foldr (\a acc -> moveOnce acc) qb [1 .. oneS]
  where
    moveOnce qtacc = buildQtBody (map (\b -> updateBody b (t * dT) (quadAcceleration 0.5 b qtacc)) (getLeafValues qtacc)) box

runForTimeQuad :: Double -> Q Body -> Q Body
runForTimeQuad t qb =
  if t <= 0
    then qb
    else runForTimeQuad (t - 1) nextqb
  where
    nextqb = moveBodiesQuad 1 1 1 qb

{-
Verifying the solutions:
For every run the time is 10000 sec, each sec = 1 sec and dt = 1 sec
If we run NBody simulation using lists on the list Body 1 containing 30 Bodies:

*NBodyQuad> runForTime 10000 bodies1
*Big list of bodies*
(82.59 secs, 43,245,542,768 bytes)

*NBodyQuad> runForTimeQuad 10000 qtBodies1
*Big QuadTree of Bodies*
(58.89 secs, 28,493,063,936 bytes)

Now if we double the bodies to 60.

*NBodyQuad> runForTime 10000 (bodies1 ++ bodies2)
*Big list of bodies*
(340.68 secs, 174,829,322,632 bytes)

*NBodyQuad> runForTimeQuad 10000 qtBodies12
*Big QuadTree of Bodies*
(122.70 secs, 65,461,028,800 bytes)

We can see that doubling the number of bodies has close to linear time increase
in case of QuadTrees - NLogN

Whereas in case of list the time increase is quadruple i.e N^2

*Complete output can be found in the outputQuad.txt and outputList.txt*
-}