module NLP.Semiring.ViterbiNBestDerivation where
import NLP.Semiring
import Data.List 
import NLP.Semiring.Viterbi
import NLP.Semiring.ViterbiNBest
import NLP.Semiring.Prob
import NLP.Semiring.Derivation

-- | The 'ViterbiNBestDerivation' is an example of a more complicated semiring
--   built up from smaller components. It keeps track of the top N scoring paths 
--   along with their derivations.
-- 
-- > type ViterbiNBestDerivation n m = ViterbiNBest n (Weighted Prob (Derivation m))
type ViterbiNBestDerivation n m = ViterbiNBest n (Weighted Prob (Derivation m))


-- | The 'ViterbiDerivation' is a simpler semiring. It just keeps track of the best 
--   scoring path and it's derivation.
--  
-- > type ViterbiDerivation m  = Viterbi (Weighted Prob (Derivation m))
type ViterbiDerivation m  = Viterbi (Weighted Prob (Derivation m))
