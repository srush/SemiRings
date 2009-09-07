{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing.Prob where 
import NLP.SemiRing

-- | The 'Prob' semiring keeps track of the likelihood of the known output 
--   by keeping track of the probability of all paths. 
newtype Prob = Prob Double
    deriving (Eq, Show, Num, Fractional, Ord) 

instance Multiplicative Prob where
    one = 1.0
    times = (*) 

instance Monoid Prob where 
    mempty = 0.0
    mappend = (+)

instance SemiRing Prob 
instance WeightedSemiRing Prob 
