module NLP.ChartParse.SemiRing.Prob where 

newtype ProbRing = ProbRing Double
    deriving (Eq, Show, Num, Fractional) 

instance Multiplicative ProbRing where
    one = 1.0
    times = (*) 

instance Monoid ProbRing where 
    mempty = 0.0
    mappend = (+)

instance SemiRing ProbRing 

instance ProbSemiRing ProbRing where 
    fromDouble = ProbRing
