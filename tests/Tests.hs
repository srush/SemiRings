module Main where 

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import NLP.SemiRing
import NLP.SemiRing.Accept
import NLP.SemiRing.Prob

import Data.List


main = defaultMain tests

tests = [
        testGroup "SemiRing Props" [
                testProperty "semi1" prop_boolRing,
                testProperty "semi2" prop_probRing

            ]
        ]

instance Arbitrary ProbRing where 
    arbitrary = do
      v <- choose (0.0, 1.0)
      return $ ProbRing v 

instance Arbitrary BoolRing where 
    arbitrary = do
      v <- choose (True, False)
      return $ BoolRing v 

type Eql s = (s -> s -> Bool) 

prop_SemiComTimes :: (SemiRing s) => (s,s,s) -> Eql s -> Bool
prop_SemiComTimes (s1, s2, s3) eq = ((s1 `times` s2) `times` s3) `eq`
                                    (s1 `times` (s2 `times` s3))

prop_SemiComPlus :: (SemiRing s) => (s,s,s) -> Eql s -> Bool
prop_SemiComPlus (s1, s2, s3) eq = ((s1 `mappend` s2) `mappend` s3) `eq`
                                   (s1 `mappend` (s2 `mappend` s3))

prop_SemiDist :: (SemiRing s) => (s,s,s) -> Eql s -> Bool
prop_SemiDist (s1, s2, s3) eq = (s1 `times` (s2 `mappend` s3)) `eq`
                                ((s1 `times` s2) `mappend` (s1 `times` s3))

semiRingProps :: (SemiRing s) => (s,s,s) -> Eql s -> Bool
semiRingProps s eq = and [prop_SemiComTimes s eq, prop_SemiComPlus s eq, prop_SemiDist s eq]

prop_probRing s1 s2 s3 = semiRingProps (s1, s2, s3) (\ a b -> abs (a - b) < 0.000001 )
  where types = ((s1,s2,s3 ):: (ProbRing, ProbRing, ProbRing))

prop_boolRing s1 s2 s3 = semiRingProps (s1, s2, s3) (==)
  where types = ((s1,s2,s3 ):: (BoolRing, BoolRing, BoolRing))


