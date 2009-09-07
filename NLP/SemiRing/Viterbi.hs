{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing.Viterbi where
import NLP.SemiRing
import NLP.SemiRing.ViterbiNBest

data One = One  
instance N One where 
    mkN = One
    n _ = 1

type Viterbi semi = ViterbiNBest One semi

