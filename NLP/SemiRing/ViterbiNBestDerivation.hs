module NLP.SemiRing.ViterbiNBestDerivation where
import NLP.SemiRing
import Data.List 
import NLP.SemiRing.Viterbi
import NLP.SemiRing.ViterbiNBest
import NLP.SemiRing.Prob
import NLP.SemiRing.Derivation

type ViterbiNBestDerivation n m = ViterbiNBest n (Weighted Prob (Derivation m))
type ViterbiDerivation m  = Viterbi (Weighted Prob (Derivation m))
