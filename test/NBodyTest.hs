{-# LANGUAGE TemplateHaskell #-}

module NBodyTest where

import Control.Applicative
import Control.Monad (liftM2, liftM3)
import Data.AEq
import NBody
import NBodyQuad
import Quad
import qualified QuadTest as QTest
import Test.HUnit hiding (State)
import Test.QuickCheck hiding (elements)
import Vectors

testForce :: Test
testForce =
  TestList
    [ (force sun earth) / 1e22 ~== 3.564460665160571 ~?= True,
      (force sun mercury) / 1e22 ~== 1.3131448718981272 ~?= True
    ]

testAcceleration :: Test
testAcceleration =
  TestList
    [almostSameVector (acceleration person earthAsCenter) (-9.799088, 0) ~?= True]

testUpdateVelocity :: Test
testUpdateVelocity =
  TestList
    [ updateVelocity zeroVector 1 (-4, 4) ~?= (-4, 4),
      updateVelocity (10, 0) 10 (2, 0) ~?= (30, 0)
    ]

testUpdatePosition :: Test
testUpdatePosition =
  TestList
    [ updatePosition zeroVector (0, 0) 1 (4, 0) ~?= (2, 0),
      updatePosition (2, 0) (2, 0) 10 (2, 0) ~?= (122, 0)
    ]

testUpdateBody :: Test
testUpdateBody =
  TestList
    [ fst (v (updateBody earth 1 (acceleration earth sun))) ~?= fst (acceleration earth sun)
    ]

testCalcCoM :: Test
testCalcCoM =
  TestList
    [ calcCoM (Body (3, 4) (0, 0) 0) (Body (-6, 8) (0, 0) 7) ~?= (-6, 8),
      calcCoM (Body (5, 3) (0, 0) 3) (Body (2, -1) (0, 0) 5) ~?= (25 / 8, 0.5)
    ]

testQtInsert :: Test
testQtInsert =
  TestList
    [ insert (emptyQ (Rect (-5, -5) (5, 5))) (1, 1) (Body (1, 1) (2, 3) 20) ~?= Just qtSingletonBody,
      insert qtSingletonBody (1, -4) (Body (1, -4) (1, -3) 10) ~?= Just qtTwoBodies
    ]

testBuildQtBody :: Test
testBuildQtBody =
  TestList
    [ buildQtBody
        [ (Body (4, 4) (1, 3) 8),
          (Body (1, 1) (-2, 2) 5),
          (Body (-1, -2) (2, -4) 7)
        ]
        (Rect (-5, -5) (5, 5))
        ~?= qtSubdivided
    ]

-- If threshold is large netAcc should be equal to the quadAcceleration
-- If threshold is really low quadAcceleration should be same as acceleration against Summary of Node
testQuadAcceleration :: Test
testQuadAcceleration =
  TestList
    [ almostSameVector (quadAcceleration (longerSideBox (bb qtPlanets)) earth qtPlanets) (netAcc earth planets) ~?= True,
      almostSameVector
        (quadAcceleration 0 (Body (-99, -99) (2, 3) 30) qtPolarBodies)
        (acceleration (Body (-99, -99) (2, 3) 30) (Body (94.2, 94.2) (0.0, 0.0) 150.0))
        ~?= True
    ]

instance Arbitrary Body where
  arbitrary =
    liftM3
      Body
      (arbitrary :: Gen (Double, Double))
      (arbitrary :: Gen (Double, Double))
      (abs <$> arbitrary :: Gen Double)
  shrink (Body p v m) =
    liftM3
      Body
      (shrink (p :: (Double, Double)))
      (shrink (p :: (Double, Double)))
      (abs <$> shrink (m :: Double))

getMassQT :: QuadTree Body -> Mass
getMassQT E = 0
getMassQT (Leaf _ _ v) = m v
getMassQT (N _ s _ _ _ _) = m s

-- Sum of mass of children nodes is equal to parent nodes
prop_sumMass :: Q Body -> Bool
prop_sumMass (QuadTree tree box) = foldTreeHelper sumInv True tree
  where
    sumInv (N _ s tl tr bl br) acc =
      acc
        && (m s) ~== (getMassQT tl + getMassQT tr + getMassQT bl + getMassQT br)
    sumInv _ acc = acc

-- Semigroup and Monoid laws are held by the Body monoid

prop_semigroupBody :: Body -> Body -> Body -> Bool
prop_semigroupBody a b c = almostEqualBodies ((a <> b) <> c) (a <> (b <> c))

prop_memptyLeftBody :: Body -> Bool
prop_memptyLeftBody p = almostEqualBodies (mempty <> p) p

prop_memptyRightBody :: Body -> Bool
prop_memptyRightBody p = almostEqualBodies (p <> mempty) p

return []

runHUnitTests :: IO ()
runHUnitTests = do
  runTestTT $
    TestList
      [ testForce,
        testAcceleration,
        testUpdateVelocity,
        testUpdatePosition,
        testUpdateBody,
        testCalcCoM,
        testQtInsert,
        testBuildQtBody,
        testQuadAcceleration
      ]
  return ()

runQuickCheckTests :: IO Bool
runQuickCheckTests = $quickCheckAll

main :: IO ()
main = do
  runHUnitTests
  _ <- runQuickCheckTests
  return ()