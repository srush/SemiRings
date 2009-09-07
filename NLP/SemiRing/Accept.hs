{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing.Accept where
import NLP.SemiRing
import Data.Boolean
newtype BoolRing = BoolRing Bool
    deriving (Eq, Show, Boolean) 

instance Multiplicative BoolRing where
    one = true
    times = (&&*)

instance Monoid BoolRing where 
    mempty = false
    mappend = (||*)

instance SemiRing BoolRing 