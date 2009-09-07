{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing(Multiplicative(..), 
                    Monoid(..), 
                    SemiRing, 
                    WeightedSemiRing, 
                    Weighted(..)) where
 
import Data.Monoid
import Data.Monoid.Multiplicative 
import Data.Monoid.Additive 
import Data.Function (on)

-- Dynamic Programming requires a semi-ring for it's internal values
class (Multiplicative a, Monoid a, Eq a) => SemiRing a

class (SemiRing a, Ord a) => WeightedSemiRing a

instance (Multiplicative a, Multiplicative b) => Multiplicative (a,b) where 
    one = (one, one)
    times (a, b) (a', b') = (a `times` a', b `times` b')

instance (SemiRing a, SemiRing b) => SemiRing (a,b)  

newtype Weighted semi1 semi2 = Weighted (semi1, semi2)
    deriving (Eq, Show, Monoid, Multiplicative, SemiRing)

instance (Ord semi1, Eq semi2) => Ord (Weighted semi1 semi2) where 
    compare (Weighted s1) (Weighted s2) = (compare `on` fst) s1 s2 

instance (WeightedSemiRing a, SemiRing b) => WeightedSemiRing (Weighted a b)