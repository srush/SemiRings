{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing.Boolean where
import NLP.SemiRing
import qualified Data.Boolean as B
import Data.Boolean ((&&*),(||*)) 
newtype Boolean = Boolean Bool
    deriving (Eq, Show, B.Boolean) 

instance Multiplicative Boolean where
    one = B.true
    times = (&&*)

instance Monoid Boolean where 
    mempty = B.false
    mappend = (||*)

instance SemiRing Boolean 