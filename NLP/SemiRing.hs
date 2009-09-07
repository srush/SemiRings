module NLP.SemiRing(Multiplicative(..), 
                    Monoid(..), 
                    SemiRing,
                    ProbSemiRing(..)) where
 
import Data.Monoid
import Data.Monoid.Multiplicative 
import Data.Monoid.Additive 

-- Dynamic Programming requires a semi-ring for it's internal values
class (Multiplicative a, Monoid a, Eq a) => SemiRing a

class (SemiRing a) => ProbSemiRing a where
    fromDouble :: Double -> a



