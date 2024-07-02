{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}

module ZkFold.Cardano.Plonk.OnChain.BLS12_381.F where

import           Data.Aeson                      (FromJSON, ToJSON)
import           GHC.Generics                    (Generic)
import           GHC.Natural                     (Natural, naturalToInteger)
import           PlutusTx                        (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins
import           PlutusTx.Prelude
import           Prelude                         (Show)

import qualified ZkFold.Base.Algebra.Basic.Class as ZkFold

--------------------------------------- F --------------------------------------

bls12_381_field_prime :: Integer
bls12_381_field_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

newtype F = F Integer
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)
makeLift ''F
makeIsDataIndexed ''F [('F,0)]

{-# INLINABLE toF #-}
toF :: Integer -> F
toF = F . (`modulo` bls12_381_field_prime)

instance Eq F where
    {-# INLINABLE (==) #-}
    (F a) == (F b) = a == b

instance ZkFold.AdditiveSemigroup F where
    {-# INLINABLE (+) #-}
    (F a) + (F b) = F $ (a + b) `modulo` bls12_381_field_prime

instance ZkFold.FromConstant Natural F where
    {-# INLINABLE fromConstant #-}
    fromConstant = ZkFold.fromConstant . naturalToInteger

instance ZkFold.Scale Natural F where
    {-# INLINABLE scale #-}
    scale = (ZkFold.*) . ZkFold.fromConstant

instance ZkFold.FromConstant Integer F where
    {-# INLINABLE fromConstant #-}
    fromConstant = F . (`modulo` bls12_381_field_prime)

instance ZkFold.Scale Integer F where
    {-# INLINABLE scale #-}
    scale = (ZkFold.*) . F

instance ZkFold.AdditiveMonoid F where
    {-# INLINABLE zero #-}
    zero = F 0

instance ZkFold.AdditiveGroup F where
    {-# INLINABLE (-) #-}
    (F a) - (F b) = F $ (a - b) `modulo` bls12_381_field_prime

instance ZkFold.MultiplicativeSemigroup F where
    {-# INLINABLE (*) #-}
    (F a) * (F b) = F $ (a * b) `modulo` bls12_381_field_prime

instance ZkFold.Exponent F Natural where
    {-# INLINABLE (^) #-}
    (^) f n = powMod f (naturalToInteger n)

instance ZkFold.MultiplicativeMonoid F where
    {-# INLINABLE one #-}
    one = F 1

-- Extended Euclidean algorithm.
{-# INLINABLE eea #-}
eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea a b =
    if a == zero then (b, zero, one)
    else let (gcd, x1, y1) = eea (b `modulo` a) a
         in (gcd, y1 - b `divide` a * x1, x1)

{-# INLINABLE powMod #-}
powMod :: F -> Integer -> F
powMod b e
    | e < 0     = error ()
    | e == 0    = ZkFold.one
    | even e    = powMod (b ZkFold.* b) (e `divide` 2)
    | otherwise = b ZkFold.* powMod (b ZkFold.* b) ((e - 1) `divide` 2)

{-# INLINABLE powTwo #-}
powTwo :: F -> Integer -> F
powTwo x k
    | k < 0     = error ()
    | k == 0    = x
    | otherwise = powTwo (x ZkFold.* x) (k - 1)

instance ZkFold.Exponent F Integer where
    {-# INLINABLE (^) #-}
    (^) = powMod

instance ZkFold.MultiplicativeGroup F where
    {-# INLINABLE invert #-}
    invert (F a)
        | gcd == 1  = F $ ((x `modulo` m) + m) `modulo` m
        | otherwise = error ()
      where m = bls12_381_field_prime
            (gcd, x, _) = eea a m

    {-# INLINABLE (/) #-}
    a / b = a ZkFold.* ZkFold.invert b


