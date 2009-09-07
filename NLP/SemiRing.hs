{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.ChartParse.SemiRing where
 
import Data.Monoid
import Data.Monoid.Multiplicative 
import Data.Monoid.Additive 
import Data.Boolean

-- Dynamic Programming requires a semi-ring for it's internal values
class (Multiplicative a, Monoid a) => SemiRing a

class (SemiRing a) => ProbSemiRing a where
    fromDouble :: Double -> a


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
newtype BoolRing = BoolRing Bool
    deriving (Eq, Show, Boolean) 

instance Multiplicative BoolRing where
    one = true
    times = (&&*)

instance Monoid BoolRing where 
    mempty = false
    mappend = (||*)

instance SemiRing BoolRing 