{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Semiring.Viterbi where
import NLP.Semiring
import NLP.Semiring.ViterbiNBest

data One = One  
instance N One where 
    mkN = One
    n _ = 1

type Viterbi semi = ViterbiNBest One semi

