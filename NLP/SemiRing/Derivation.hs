{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.SemiRing.Derivation (Derivation(..), MultiDerivation(..)) where
import NLP.SemiRing
import NLP.SemiRing.Helpers
import qualified Data.Set as S 
import Data.Monoid
import Data.Maybe (isNothing)

newtype Derivation m = Derivation (Maybe m)
    deriving (Eq, Show, Ord) 

instance (Monoid m) => Multiplicative (Derivation m) where
    one = Derivation $ Just mempty
    times (Derivation d1) (Derivation d2) = Derivation $ do 
        d1' <- d1
        d2' <- d2
        return $ mappend d1' d2'

instance (Ord m) => Monoid (Derivation m) where 
    mempty = Derivation Nothing
    mappend (Derivation s1) (Derivation s2) = 
        Derivation $ case (s1,s2) of 
                       (Nothing, s2) -> s2
                       (s1, Nothing) -> s1
                       (s1, s2) -> max s1 s2                          

instance (Monoid m, Eq m, Ord m) => SemiRing (Derivation m)


newtype MultiDerivation m = MultiDerivation (S.Set m)
    deriving (Eq, Show, Ord) 

instance (Monoid m, Ord m) => Multiplicative (MultiDerivation m) where
    one = MultiDerivation $ S.fromList [mempty]
    times (MultiDerivation d1) (MultiDerivation d2) = MultiDerivation $ 
        S.fromList $ 
        map (uncurry mappend) $ 
        cartesian (S.toList d1) (S.toList d2) 

instance (Ord m) => Monoid (MultiDerivation m) where 
    mempty = MultiDerivation S.empty
    mappend (MultiDerivation s1) (MultiDerivation s2) = MultiDerivation $ S.union s1 s2

instance (Ord m, Monoid m, Eq m) => SemiRing (MultiDerivation m)

