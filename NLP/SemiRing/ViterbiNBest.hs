{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module NLP.SemiRing.ViterbiNBest where
import NLP.SemiRing
import NLP.SemiRing.Helpers
import Data.List 


class N a where 
    mkN :: a
    n :: a -> Int
    
data ViterbiNBest n semi = ViterbiNBest [semi] 
  deriving (Eq, Show)

instance (N n, Ord semi, WeightedSemiRing semi) => Multiplicative (ViterbiNBest n semi) where
    one = ViterbiNBest [one]
    times (ViterbiNBest a) (ViterbiNBest b) = 
        ViterbiNBest $
        take (n (mkN::n)) $
        reverse $ sort $
        map (uncurry times) $ cartesian a b 

instance (N n, WeightedSemiRing semi, Ord semi) => Monoid (ViterbiNBest n semi) where 
    mempty = ViterbiNBest []
    mappend (ViterbiNBest a) (ViterbiNBest b) = 
        ViterbiNBest $ take (n (mkN::n)) $ reverse $ sort (a ++ b)

instance (N n, WeightedSemiRing semi, Ord semi) => SemiRing (ViterbiNBest n semi)


data Ten = Ten  
instance N Ten where 
    mkN = Ten
    n _ = 10

type Viterbi10Best semi = ViterbiNBest Ten semi
