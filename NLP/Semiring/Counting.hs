{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Semiring.Counting where
import NLP.Semiring

-- | The 'Counting' semiring keeps track of the number of paths 
--   or derivations led to a given output.
newtype Counting = Counting Integer
    deriving (Eq, Show, Num, Ord) 

instance Multiplicative Counting where
    one = 1
    times = (*) 

instance Monoid Counting where 
    mempty = 0
    mappend = (+)


instance Semiring Counting 
instance WeightedSemiring Counting 

