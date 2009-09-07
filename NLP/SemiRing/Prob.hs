{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing.Prob where 
import NLP.SemiRing

newtype ProbRing = ProbRing Double
    deriving (Eq, Show, Num, Fractional, Ord) 

instance Multiplicative ProbRing where
    one = 1.0
    times = (*) 

instance Monoid ProbRing where 
    mempty = 0.0
    mappend = (+)

instance SemiRing ProbRing 

instance ProbSemiRing ProbRing where 
    fromDouble = ProbRing
