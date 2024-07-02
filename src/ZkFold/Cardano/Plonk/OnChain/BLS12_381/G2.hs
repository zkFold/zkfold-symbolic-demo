{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module ZkFold.Cardano.Plonk.OnChain.BLS12_381.G2 where

import           GHC.Natural                     (Natural, naturalToInteger)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                (traceError)

import qualified ZkFold.Base.Algebra.Basic.Class as ZkFold

type G2 = BuiltinBLS12_381_G2_Element

instance ZkFold.AdditiveSemigroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G2_add

instance ZkFold.FromConstant Natural BuiltinBLS12_381_G2_Element where
    fromConstant = traceError "not supported on onchain"

instance ZkFold.Scale Natural BuiltinBLS12_381_G2_Element where
    scale n = ZkFold.scale (naturalToInteger n)

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance ZkFold.FromConstant Integer BuiltinBLS12_381_G2_Element where
    fromConstant = traceError "not supported on onchain"

instance ZkFold.Scale Integer BuiltinBLS12_381_G2_Element where
    {-# INLINABLE scale #-}
    scale = bls12_381_G2_scalarMul

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G2_add g (bls12_381_G2_neg h)
