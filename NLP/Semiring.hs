{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Semiring(
  -- * Semiring
  --                    
  -- $SemiringDesc

  Multiplicative(..), 
  Monoid(..), 
  Semiring, 
  WeightedSemiring, 
  Weighted(..)) where
 
import Data.Monoid
import Data.Monoid.Multiplicative 
import Data.Monoid.Additive 
import Data.Function (on)



-- $SemiringDesc
-- A Semirings (rings without additive inverses, http://en.wikipedia.org/wiki/Semiring) are 
-- the fundamental structure for performing computations over finite state machines, 
-- parsers, and other dynamic programmy-systems. This library extends the basic structures  
-- defined for Monoids to Semirings and includes implementations of the major semirings 
-- in parsing. 
--
-- This work is based largely on "Semiring Parsing" by Joshua Goodman. (http://www.ldc.upenn.edu/acl/J/J99/J99-4004.pdf)  
-- which describes many of the interesting parsing semirings.



-- | A 'Semiring' is made up of an additive Monoid and a Multiplicative.
--   It must also satisfy several other algebraic properties checked by quickcheck. 
class (Multiplicative a, Monoid a) => Semiring a


-- | A 'WeightedSemiring' also includes a sensical ordering over choices. 
--   i.e. out of two choices which is better. This is used for Viterbi selection.   
class (Semiring a, Ord a) => WeightedSemiring a

   
instance (Multiplicative a, Multiplicative b) => Multiplicative (a,b) where 
    one = (one, one)
    times (a, b) (a', b') = (a `times` a', b `times` b')


-- | Dual semirings can be useful. For instance combining the  
--   Prob semiring and the MultiDerivation ring gives the total likelihood of 
--   a derivation along with the paths to get there. 
instance (Semiring a, Semiring b) => Semiring (a,b)  


-- | The 'Weighted' type is the main type of WeightedSemiring.
--   It combines scoring semiring with a history semiring.
-- 
--   The best example of this is the ViterbiDerivation semiring.
newtype Weighted semi1 semi2 = Weighted (semi1, semi2)
    deriving (Eq, Show, Monoid, Multiplicative, Semiring)

instance (Ord semi1, Eq semi2) => Ord (Weighted semi1 semi2) where 
    compare (Weighted s1) (Weighted s2) = (compare `on` fst) s1 s2 

instance (WeightedSemiring a, Eq b, Semiring b) => WeightedSemiring (Weighted a b)