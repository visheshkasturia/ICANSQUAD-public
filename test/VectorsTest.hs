{-# LANGUAGE TemplateHaskell #-}

module VectorsTest where

import Test.HUnit hiding (State)
import Test.QuickCheck
import Vectors

--Test: Vector from origin to (3, 4) should give vector1
testVectorPQ :: Test
testVectorPQ =
  TestList
    [ vectorPQ origin (3, 4) ~?= vector1,
      vectorPQ (-1, 2) (4, 2) ~?= (5, 0)
    ]

testAddVectors :: Test
testAddVectors =
  TestList
    [ addVectors zeroVector vector1 ~?= vector1,
      addVectors (1, 2) (2, -1) ~?= (3, 1)
    ]

-- Invariant: Unit Vector always has the same direction as the parent vector
prop_unitVector_almostSameDirection :: Vector -> Bool
prop_unitVector_almostSameDirection v = almostSameDirection (unitVector v) v

-- Invariant :: unitVector scaledBy vectorMagnitude is equal to Vector itself
prop_unitVector_scaled :: Vector -> Bool
prop_unitVector_scaled v = almostSameVector (scaleVector (vectorMagnitude v) (unitVector v)) v

return []

runHUnitTests :: IO ()
runHUnitTests = do
  runTestTT $
    TestList
      [ testVectorPQ,
        testAddVectors
      ]
  return ()

runQuickCheckTests :: IO Bool
runQuickCheckTests = $quickCheckAll

main :: IO ()
main = do
  runHUnitTests
  _ <- runQuickCheckTests
  return ()