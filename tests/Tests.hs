{-# LANGUAGE ScopedTypeVariables #-}
module Main where 

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import NLP.Semiring
import NLP.Semiring.Boolean
import NLP.Semiring.Prob
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBest
import NLP.Semiring.Counting
import NLP.Semiring.Derivation
import NLP.Semiring.ViterbiNBestDerivation

import qualified Data.Set as S
import Data.List
import Control.Monad (liftM)

main = defaultMain tests

tests = [
        testGroup "Semiring Props"  [
                       testProperty "semiProb bool" prop_boolRing,
                       testProperty "semiProb prob" prop_probRing,
                       testProperty "semiProb viterbi" prop_viterbiRing,
                       testProperty "semiProb counting" prop_counting, 
                       testProperty "semiProb viterbi n-best" prop_viterbiNBest,
                       testProperty "semiProb derivation" prop_derivation,
                       testProperty "semiProb multi-derivation" prop_multiDerivation,
                       testProperty "semiProb nbest derivation" prop_nbestMultiDerivation
            ]
        ]

instance Arbitrary Prob where 
    arbitrary = Prob `liftM` choose (0.0, 1.0)

instance (N n, Ord a, Arbitrary a) => Arbitrary (ViterbiNBest n a) where 
    arbitrary = do
      v <- arbitrary
      return $ ViterbiNBest $ reverse $ sort $ take (n $ (mkN::n)) $ v
 
instance Arbitrary Boolean where 
    arbitrary = Boolean `liftM` choose (True, False)

instance Arbitrary Counting where 
    arbitrary = Counting `liftM` abs `liftM` arbitrary

instance (Arbitrary a) => Arbitrary (Derivation a) where 
    arbitrary = Derivation `liftM` arbitrary 

instance (Arbitrary a, Ord a) => Arbitrary (MultiDerivation a) where 
    arbitrary = (MultiDerivation . S.fromList . take 10) `liftM` arbitrary 
           
instance (Arbitrary a, Arbitrary b) => Arbitrary (Weighted a b) where 
    arbitrary = Weighted `liftM` arbitrary 


type Eql s = (s -> s -> Bool) 


-- (a * b) * c = a * (b * c)
associativeTimes :: (Semiring s) => (s,s,s) -> Eql s -> Bool
associativeTimes (s1, s2, s3) eq =  
    ((s1 `times` s2) `times` s3) `eq`
    (s1 `times` (s2 `times` s3))

-- (a + b) + c = a + (b + c)
associativePlus :: (Semiring s) => (s,s,s) -> Eql s -> Bool
associativePlus (s1, s2, s3) eq = 
    ((s1 `mappend` s2) `mappend` s3) `eq`
     (s1 `mappend` (s2 `mappend` s3))


-- a + b = b + a
commutativePlus :: (Semiring s) => (s,s,s) -> Eql s -> Bool
commutativePlus (a, b, _) eq = 
    (a `mappend` b)  `eq`
    (b `mappend` a)


-- a * (b + c) = (a * b) +  (a * c)
distribution :: (Semiring s) => (s,s,s) -> Eql s -> Bool
distribution (s1, s2, s3) eq = 
    (s1 `times` (s2 `mappend` s3)) `eq`
    ((s1 `times` s2) `mappend` (s1 `times` s3))


-- a + 0 = 0 + a = a
zeroAdd :: (Semiring s) => (s,s,s) -> Eql s -> Bool
zeroAdd (a, _, _) eq = 
    (mempty `mappend` a) `eq` a &&
    (a `mappend` mempty) `eq` a


-- a * 0 = 0
zeroMult :: (Semiring s) => (s,s,s) -> Eql s -> Bool
zeroMult (a, _, _) eq = 
    (mempty `times` a) `eq` mempty && 
    (a `times` mempty) `eq` mempty 
                     


oneMult :: (Semiring s) => (s,s,s) -> Eql s -> Bool
oneMult (a, _, _) eq = 
    (one `times` a) `eq` a &&
    (a `times` one) `eq` a

semiRingProps :: (Semiring s) => (s,s,s) -> Eql s -> Bool
semiRingProps s eq = and [distribution s eq, 
                          associativePlus s eq, 
                          zeroAdd s eq, 
                          zeroMult s eq, 
                          oneMult s eq, 
                          commutativePlus s eq, 
                          associativeTimes s eq]

doubEq a b = abs (a - b) < 0.000001 

prop_probRing s1 s2 s3 = semiRingProps (s1, s2, s3) doubEq
  where types = ((s1,s2,s3 ):: (Prob, Prob, Prob))

prop_boolRing s1 s2 s3 = semiRingProps (s1, s2, s3) (==)
  where types = ((s1,s2,s3 ):: (Boolean, Boolean, Boolean))

prop_viterbiRing s1 s2 s3 = semiRingProps (s1, s2, s3) 
                            (\(ViterbiNBest a) (ViterbiNBest b) -> and $ zipWith doubEq a b)
  where types = ((s1,s2,s3 ):: (Viterbi Prob, Viterbi Prob, Viterbi Prob))

prop_counting s1 s2 s3 = semiRingProps (s1, s2, s3) (==)
  where types = ((s1,s2,s3 ):: (Counting, Counting, Counting))


prop_viterbiNBest s1 s2 s3 = semiRingProps (s1, s2, s3) (==)
  where types = ((s1,s2,s3 ):: (Viterbi10Best Counting, Viterbi10Best Counting, Viterbi10Best Counting))


prop_derivation s1 s2 s3 = semiRingProps (s1, s2, s3) (==)
  where types = ((s1,s2,s3):: (Derivation String, Derivation String, Derivation String))

prop_multiDerivation s1 s2 s3 = semiRingProps (s1, s2, s3) (==)
  where types = ((s1,s2,s3):: (MultiDerivation String, MultiDerivation String, MultiDerivation String))

prop_nbestMultiDerivation s1 s2 s3 = 
    semiRingProps (s1, s2, s3) 
                      (\(ViterbiNBest a) (ViterbiNBest b) ->
                           and $ zipWith 
                                   (\(Weighted (a,b)) (Weighted (a',b')) -> 
                                        doubEq a a' && b == b') a b)
  where types = ((s1,s2,s3):: (ViterbiNBestDerivation Ten String, ViterbiNBestDerivation Ten String, ViterbiNBestDerivation Ten String))