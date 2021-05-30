{-# LANGUAGE ExistentialQuantification, ViewPatterns, RankNTypes #-}
module Year2019.Day22 where

import Challenge

import Data.Mod
import Data.Semigroup (stimes)
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, SomeNat(..), someNatVal)

data Technique
  = Cut     Integer -- ^ cut N cards
  | DealInc Integer -- ^ deal with increment N
  | DealNew         -- ^ deal into new stack
  deriving Show

parseTech :: String -> Technique
parseTech tech = case words tech of
  "deal":"into":_ -> DealNew
  "deal":_:_:n:_  -> DealInc (read n)
  "cut":n:_       -> Cut (read n)

data LinearFn a = LinearFn { _scale :: a, _shift :: a }

apply :: (Num a) => LinearFn a -> a -> a
apply (LinearFn scale shift) x = scale * x + shift

invert :: Fractional a => LinearFn a -> LinearFn a
invert (LinearFn a b) = LinearFn a' (-b*a')
  where a' = recip a

instance Num a => Semigroup (LinearFn a) where
  LinearFn c d <> LinearFn a b = LinearFn (a*c) (b + a*d)

instance Num a => Monoid (LinearFn a) where
  mempty = LinearFn 1 0

techToLinearFn :: KnownNat n => Technique -> LinearFn (Mod n)
techToLinearFn DealNew = LinearFn -1 -1
techToLinearFn (Cut i) = LinearFn 1 (- fromInteger i)
techToLinearFn (DealInc i) = LinearFn (fromInteger i) 0

techsToLinearFn :: KnownNat n => [Technique] -> LinearFn (Mod n)
techsToLinearFn = foldMap techToLinearFn

withModulus :: (forall n. KnownNat n => LinearFn (Mod n)) -> Natural -> Natural -> Natural
f `withModulus` (someNatVal -> SomeNat m) = unMod . asMod m . apply f . fromIntegral

asMod :: proxy n -> Mod n -> Mod n
asMod _ x = x

instance Challenge [Technique] where
  parse = fmap parseTech . lines
  partOne techs = show ((techsToLinearFn techs `withModulus` 10007) 2019)
  partTwo techs = show ((stimes 101741582076661 (invert $ techsToLinearFn techs) `withModulus` 119315717514047) 2020)